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
