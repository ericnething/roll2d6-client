{-
   Roll2d6 Virtual Tabletop Project

   Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public
   License along with this program. If not, see
   <https://www.gnu.org/licenses/>.
-}


module Fate.CharacterSheet.Update exposing (update)

import Array exposing (Array)
import Fate.CharacterSheet.Types exposing (..)
import List.Extra exposing (stableSortWith)
import Util exposing (removeIndexFromArray)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateName name ->
            ( { model | name = name }
            , Cmd.none
            )

        UpdateDescription description ->
            ( { model | description = description }
            , Cmd.none
            )

        UpdateRefresh points ->
            ( { model | refresh = points }
            , Cmd.none
            )

        UpdateFatePoints points ->
            ( { model | fatePoints = points }
            , Cmd.none
            )

        UpdateAspect index aspect ->
            ( { model
                | aspects =
                    Array.set index aspect model.aspects
              }
            , Cmd.none
            )

        AddNewAspect title ->
            ( { model
                | aspects =
                    Array.push (Aspect title 0) model.aspects
              }
            , Cmd.none
            )

        RemoveAspect index ->
            ( { model
                | aspects =
                    removeIndexFromArray index model.aspects
              }
            , Cmd.none
            )

        UpdateSkill index skill ->
            ( { model
                | skills =
                    Array.set index skill model.skills
              }
            , Cmd.none
            )

        AddNewSkill skill ->
            ( { model
                | skills =
                    Array.push skill model.skills
                        |> Array.toList
                        |> List.sortBy
                            (\(Skill rating _) ->
                                skillRatingToInt rating
                            )
                        |> List.reverse
                        |> Array.fromList
              }
            , Cmd.none
            )

        RemoveSkill index ->
            ( { model
                | skills =
                    removeIndexFromArray index model.skills
              }
            , Cmd.none
            )

        UpdateStunt index stunt ->
            ( { model
                | stunts =
                    Array.set index stunt model.stunts
              }
            , Cmd.none
            )

        AddNewStunt title description ->
            ( { model
                | stunts =
                    Array.push (Stunt title description) model.stunts
              }
            , Cmd.none
            )

        RemoveStunt index ->
            ( { model
                | stunts =
                    removeIndexFromArray index model.stunts
              }
            , Cmd.none
            )

        UpdateStressBox trackIndex index stressBox ->
            let
                newModel =
                    case Array.get trackIndex model.stress of
                        Nothing ->
                            model

                        Just (StressTrack title boxes) ->
                            { model
                                | stress =
                                    Array.set
                                        trackIndex
                                        (StressTrack
                                            title
                                            (Array.set index stressBox boxes)
                                        )
                                        model.stress
                            }
            in
            ( newModel, Cmd.none )

        AddNewStressTrack stressTrack ->
            ( { model
                | stress =
                    Array.push stressTrack model.stress
              }
            , Cmd.none
            )

        RemoveStressTrack index ->
            ( { model
                | stress =
                    removeIndexFromArray index model.stress
              }
            , Cmd.none
            )

        UpdateStressTrack index stressTrack ->
            ( { model
                | stress =
                    Array.set index stressTrack model.stress
              }
            , Cmd.none
            )

        AddStressBox trackIndex stressBox ->
            let
                newModel =
                    case Array.get trackIndex model.stress of
                        Nothing ->
                            model

                        Just (StressTrack title boxes) ->
                            { model
                                | stress =
                                    Array.set
                                        trackIndex
                                        (StressTrack
                                            title
                                            (Array.push stressBox boxes)
                                        )
                                        model.stress
                            }
            in
            ( newModel, Cmd.none )

        RemoveStressBox trackIndex ->
            let
                newModel =
                    case Array.get trackIndex model.stress of
                        Nothing ->
                            model

                        Just (StressTrack title boxes) ->
                            { model
                                | stress =
                                    Array.set
                                        trackIndex
                                        (StressTrack
                                            title
                                            (Array.slice 0 -1 boxes)
                                        )
                                        model.stress
                            }
            in
            ( newModel, Cmd.none )

        UpdateConsequence index consequence ->
            ( { model
                | consequences =
                    Array.set index consequence model.consequences
              }
            , Cmd.none
            )

        AddNewConsequence newConsequence ->
            ( { model
                | consequences =
                    Array.push newConsequence model.consequences
                        |> Array.toList
                        |> stableSortWith
                            (\(Consequence a _ _) (Consequence b _ _) ->
                                Basics.compare
                                    (severityToInt b)
                                    (severityToInt a)
                            )
                        |> Array.fromList
              }
            , Cmd.none
            )

        RemoveConsequence index ->
            ( { model
                | consequences =
                    removeIndexFromArray index model.consequences
              }
            , Cmd.none
            )

        UpdateConditionBox trackIndex index stressBox ->
            let
                newModel =
                    case Array.get trackIndex model.conditions of
                        Nothing ->
                            model

                        Just (Condition title boxes) ->
                            { model
                                | conditions =
                                    Array.set
                                        trackIndex
                                        (Condition
                                            title
                                            (Array.set index stressBox boxes)
                                        )
                                        model.conditions
                            }
            in
            ( newModel, Cmd.none )

        AddConditionBox trackIndex stressBox ->
            let
                newModel =
                    case Array.get trackIndex model.conditions of
                        Nothing ->
                            model

                        Just (Condition title boxes) ->
                            { model
                                | conditions =
                                    Array.set
                                        trackIndex
                                        (Condition
                                            title
                                            (Array.push stressBox boxes)
                                        )
                                        model.conditions
                            }
            in
            ( newModel, Cmd.none )

        RemoveConditionBox trackIndex ->
            let
                newModel =
                    case Array.get trackIndex model.conditions of
                        Nothing ->
                            model

                        Just (Condition title boxes) ->
                            { model
                                | conditions =
                                    Array.set
                                        trackIndex
                                        (Condition
                                            title
                                            (Array.slice 0 -1 boxes)
                                        )
                                        model.conditions
                            }
            in
            ( newModel, Cmd.none )

        UpdateCondition trackIndex condition ->
            ( { model
                | conditions =
                    Array.set trackIndex condition model.conditions
              }
            , Cmd.none
            )

        AddNewCondition condition ->
            ( { model
                | conditions =
                    Array.push condition model.conditions
              }
            , Cmd.none
            )

        RemoveCondition index ->
            ( { model
                | conditions =
                    removeIndexFromArray index model.conditions
              }
            , Cmd.none
            )

        UpdateNotes notes ->
            ( { model | notes = notes }
            , Cmd.none
            )
