module Fate exposing (..)

import Fate.CharacterSheet.Types as CharacterSheet
    exposing (CharacterSheet)
import Fate.GameAspectSheet.Types as GameAspectSheet
    exposing (GameAspectSheet)

type Sheet
    = CharacterSheet CharacterSheetModel
    | GameAspectSheet GameAspectSheetModel

type alias Index = Int

type Msg
    = CharacterSheetMsg CharacterSheet.Msg
    | GameAspectSheetMsg GameAspectSheet.Msg

update : Msg -> Sheet -> (Sheet, Cmd Msg)
update msg model
    case (msg, model) of
        (CharacterSheetMsg submsg, CharacterSheet sheet) ->
            let
                (updatedSheet, cmd) =
                    CharacterSheet.update submsg sheet
            in
                ( sheet
                , Cmd.map CharacterSheetMsg cmd
                )

        (GameAspectSheetMsg submsg, GameAspectSheet sheet) ->
            let
                (updatedSheet, cmd) =
                    GameAspectSheet.update submsg sheet
            in
                ( sheet
                , Cmd.map GameAspectSheetMsg cmd
                )

        _ ->
            (model, Cmd.none)
