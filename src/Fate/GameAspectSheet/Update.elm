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

module Fate.GameAspectSheet.Update exposing (update)

import Array exposing (Array)
import Fate.GameAspectSheet.Types exposing (..)
import Fate.CharacterSheet.Types exposing (Aspect(..))
import List.Extra exposing (stableSortWith)
import Util exposing (removeIndexFromArray)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AddNewScene ->
            ({ model
                 | scenes =
                     Array.push
                         (Scene "" (Array.fromList []))
                         model.scenes
             }
            , Cmd.none
            )

        RemoveScene index ->
            ({ model
                 | scenes =
                     removeIndexFromArray index model.scenes
            }
            , Cmd.none
            )

        UpdateSceneTitle index title ->
            case Array.get index model.scenes of
                Nothing ->
                    ( model, Cmd.none )

                Just scene ->
                    ( { model
                        | scenes =
                            Array.set
                                index
                                { scene | title = title }
                                model.scenes
                      }
                    , Cmd.none
                    )
        UpdateAspect sceneIndex aspectIndex aspect ->
            case Array.get sceneIndex model.scenes of
                Nothing ->
                    ( model, Cmd.none )

                Just scene ->
                    ( { model
                        | scenes =
                            Array.set
                                sceneIndex
                                { scene
                                    | aspects =
                                      Array.set
                                          aspectIndex
                                          aspect
                                          scene.aspects
                                }
                                model.scenes
                      }
                    , Cmd.none
                    )

        AddNewAspect sceneIndex title ->
            case Array.get sceneIndex model.scenes of
                Nothing ->
                    ( model, Cmd.none )

                Just scene ->
                    ( { model
                        | scenes =
                            Array.set
                                sceneIndex
                                { scene
                                    | aspects =
                                        Array.push
                                            (Aspect title 0)
                                            scene.aspects
                                }
                                model.scenes
                      }
                    , Cmd.none
                    )

        RemoveAspect sceneIndex aspectIndex ->
            case Array.get sceneIndex model.scenes of
                Nothing ->
                    ( model, Cmd.none )

                Just scene ->
                    ( { model
                        | scenes =
                            Array.set
                                sceneIndex
                                { scene
                                    | aspects =
                                        removeIndexFromArray
                                            aspectIndex
                                            scene.aspects
                                }
                                model.scenes
                      }
                    , Cmd.none
                    )
