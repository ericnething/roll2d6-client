module Fate.GameAspectSheet.Update exposing (update)

import Array exposing (Array)
import Fate.GameAspectSheet.Types exposing (..)
import List.Extra exposing (stableSortWith)
import Util exposing (removeIndexFromArray)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
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
