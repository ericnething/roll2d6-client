-- Roll2d6 Virtual Tabletop Project
--
-- Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>
--
-- This program is free software: you can redistribute it
-- and/or modify it under the terms of the GNU Affero
-- General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR A
-- PARTICULAR PURPOSE.  See the GNU Affero General Public
-- License for more details.
--
-- You should have received a copy of the GNU Affero General
-- Public License along with this program. If not, see
-- <https://www.gnu.org/licenses/>.

module RedMarkets.CharacterSheet.Update exposing (update)

import Array exposing (Array)
import RedMarkets.CharacterSheet.Types exposing (..)
import List.Extra exposing (stableSortWith)
import Util exposing (removeIndexFromArray)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateName name ->
            ( { model | name = name }
            , Cmd.none
            )

        UpdateDescription description ->
            ( { model | description = description }
            , Cmd.none
            )

        UpdateCrew crew ->
            ( { model | crew = crew }
            , Cmd.none
            )

        UpdateWeakSpot weakSpot ->
            ( { model | weakSpot = weakSpot }
            , Cmd.none
            )

        UpdateSoftSpot softSpot ->
            ( { model | softSpot = softSpot }
            , Cmd.none
            )

        UpdateToughSpot toughSpot ->
            ( { model | toughSpot = toughSpot }
            , Cmd.none
            )
            
        UpdatePotential type_ rating ->
            ( updatePotential
                  model
                  type_
                  (updatePotentialRating rating)
            , Cmd.none
            )

        UpdateSkill type_ index skill ->
            ( updatePotential
                  model
                  type_
                  (updateSkill index skill)
            , Cmd.none
            )

        AddNewSkill type_ skillName ->
            ( updatePotential
                  model
                  type_
                  (addNewSkill (Skill skillName 0))
            , Cmd.none
            )

        RemoveSkill type_ index ->
            ( updatePotential
                  model
                  type_
                  (removeSkill index)
            , Cmd.none
            )

        UpdateDependent index relationship ->
            ({ model
                 | dependents
                     = Array.set index relationship model.dependents
             }
            , Cmd.none
            )

        AddNewDependent relationship ->
            ({ model
                 | dependents
                     = Array.push relationship model.dependents
             }
            , Cmd.none
            )

        RemoveDependent index ->
            ({ model
                 | dependents
                     = removeIndexFromArray index model.dependents
             }
            , Cmd.none
            )

        UpdateReference index relationship ->
            ({ model
                 | references
                     = Array.set index relationship model.references
             }
            , Cmd.none
            )

        AddNewReference relationship ->
            ({ model
                 | references
                     = Array.push relationship model.references
             }
            , Cmd.none
            )

        RemoveReference index ->
            ({ model
                 | references
                     = removeIndexFromArray index model.references
             }
            , Cmd.none
            )

        UpdateDetachment index threat ->
            ({ model
                 | detachment
                     = Array.set index threat model.detachment
             }
            , Cmd.none
            )

        UpdateStress index threat ->
            ({ model
                 | stress
                     = Array.set index threat model.stress
             }
            , Cmd.none
            )

        UpdateTrauma index threat ->
            ({ model
                 | trauma
                     = Array.set index threat model.trauma
             }
            , Cmd.none
            )

        UpdateWound location index wound ->
            ( updateWound
                  model
                  location
                  index
                  wound
            , Cmd.none
            )

        UpdateGear index gear ->
            ({ model
                 | gear = Array.set index gear model.gear
             }
            , Cmd.none
            )

        AddNewGear ->
            let
                emptyGear =
                    { title = ""
                    , charges = Array.repeat 10 Charge
                    , upkeep = 1
                    , effect = ""
                    , qualities = Array.empty
                    , upgrades = Array.empty
                    }
            in
                ({ model
                     | gear = Array.push emptyGear model.gear
                 }
                , Cmd.none
                )

        RemoveGear index ->
            ({ model
                 | gear = removeIndexFromArray index model.gear
             }
            , Cmd.none
            )

        UpdateGearCharge { gearIndex, chargeIndex, charge } ->
            let
                newModel =
                    case Array.get gearIndex model.gear of
                        Nothing ->
                            model

                        Just item ->
                            { model
                                | gear =
                                    Array.set
                                    gearIndex
                                    ({ item
                                         | charges =
                                             Array.set
                                             chargeIndex
                                             charge
                                             item.charges
                                     })
                                    model.gear
                            }
            in
                ( newModel, Cmd.none )

        UpdateGearQuality gearIndex qualityIndex quality ->
            let
                newModel =
                    case Array.get gearIndex model.gear of
                        Nothing ->
                            model

                        Just item ->
                            { model
                                | gear =
                                    Array.set
                                    gearIndex
                                    ({ item
                                         | qualities =
                                             Array.set
                                             qualityIndex
                                             quality
                                             item.qualities
                                     })
                                    model.gear
                            }
            in
                ( newModel, Cmd.none )

        AddNewGearQuality gearIndex ->
            let
                emptyQuality =
                    { title = ""
                    , description = ""
                    }

                newModel =
                    case Array.get gearIndex model.gear of
                        Nothing ->
                            model

                        Just item ->
                            { model
                                | gear =
                                    Array.set
                                    gearIndex
                                    ({ item
                                         | qualities =
                                             Array.push
                                             emptyQuality
                                             item.qualities
                                     })
                                    model.gear
                            }
            in
                ( newModel, Cmd.none )

        RemoveGearQuality gearIndex qualityIndex ->
            let
                newModel =
                    case Array.get gearIndex model.gear of
                        Nothing ->
                            model

                        Just item ->
                            { model
                                | gear =
                                    Array.set
                                    gearIndex
                                    ({ item
                                         | qualities =
                                             removeIndexFromArray
                                             qualityIndex
                                             item.qualities
                                     })
                                    model.gear
                            }
            in
                ( newModel, Cmd.none )

        UpdateNotes notes ->
            ( { model | notes = notes }
            , Cmd.none
            )


updatePotential : Model
                -> PotentialType
                -> (Potential -> Potential)
                -> Model
updatePotential model type_ toPotential =
    case type_ of
        Strength ->
            { model | str = toPotential model.str }

        Speed ->
            { model | spd = toPotential model.spd }

        Adaptability ->
            { model | adp = toPotential model.adp }

        Intelligence ->
            { model | int = toPotential model.int }

        Charm ->
            { model | cha = toPotential model.cha }

        Will ->
            { model | wil = toPotential model.wil }

updatePotentialRating : Int -> Potential -> Potential
updatePotentialRating rating (Potential _ skills) =
    Potential rating skills

updateSkill : Index ->  Skill -> Potential -> Potential
updateSkill index skill (Potential rating skills) =
    Potential rating (Array.set index skill skills)

addNewSkill : Skill -> Potential -> Potential
addNewSkill skill (Potential rating skills) =
    Potential rating (Array.push skill skills)

removeSkill : Index -> Potential -> Potential
removeSkill index (Potential rating skills) =
    Potential rating (removeIndexFromArray index skills)


updateWound : Model -> WoundLocation -> Index -> Wound -> Model
updateWound model location index wound =
    case location of
        RightLeg ->
            { model
                | rightLegWounds
                    = Array.set index wound model.rightLegWounds
            }

        LeftLeg ->
            { model
                | leftLegWounds
                    = Array.set index wound model.leftLegWounds
            }

        RightArm ->
            { model
                | rightArmWounds
                    = Array.set index wound model.rightArmWounds
            }

        LeftArm ->
            { model
                | leftArmWounds
                    = Array.set index wound model.leftArmWounds
            }

        Torso ->
            { model
                | torsoWounds
                    = Array.set index wound model.torsoWounds
            }

        Head ->
            { model
                | headWounds
                    = Array.set index wound model.headWounds
            }
