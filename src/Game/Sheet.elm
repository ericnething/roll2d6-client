module Game.Sheet exposing (..)

import Html.Styled exposing (Html)
import Game.Types exposing (GameType(..))
import Game.Sheet.Types exposing (SheetMsg(..), SheetModel(..))
import Fate
import Fate.CharacterSheet.Template

updateSheet : SheetMsg
            -> SheetModel
            -> (SheetModel, Cmd (SheetMsg))
updateSheet sheetMsg sheetModel =
    case (sheetMsg, sheetModel) of
        (FateMsg msg, FateSheet model) ->
            let
                (updatedModel, cmd) = Fate.update msg model
            in
                ( FateSheet updatedModel
                , Cmd.map FateMsg cmd
                )

view : SheetModel -> Html SheetMsg
view sheetModel =
    case sheetModel of
        FateSheet model ->
            Fate.view model
                |> Html.Styled.map FateMsg

editView : SheetModel -> Html SheetMsg
editView sheetModel =
    case sheetModel of
        FateSheet model ->
            Fate.editView model
                |> Html.Styled.map FateMsg

blank : GameType -> SheetModel
blank gameType =
    case gameType of
        Fate -> FateSheet Fate.blankCharacterSheet
