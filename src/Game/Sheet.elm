module Game.Sheet exposing (..)

import Array exposing (Array)
import Html.Styled exposing (Html)
import Game.Types exposing (GameType(..))
import Game.Sheet.Types exposing (SheetMsg(..), SheetModel(..))

import Fate
import Fate.CharacterSheet.Template

import WorldOfDungeons

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

        (WorldOfDungeonsMsg msg, WorldOfDungeonsSheet model) ->
            let
                (updatedModel, cmd) = WorldOfDungeons.update msg model
            in
                ( WorldOfDungeonsSheet updatedModel
                , Cmd.map WorldOfDungeonsMsg cmd
                )

        _ ->
            (sheetModel, Cmd.none)

compactView : SheetModel -> Html SheetMsg
compactView sheetModel =
    case sheetModel of
        FateSheet model ->
            Fate.compactView model
                |> Html.Styled.map FateMsg

        WorldOfDungeonsSheet model ->
            WorldOfDungeons.view model
                |> Html.Styled.map WorldOfDungeonsMsg


view : SheetModel -> Html SheetMsg
view sheetModel =
    case sheetModel of
        FateSheet model ->
            Fate.view model
                |> Html.Styled.map FateMsg

        WorldOfDungeonsSheet model ->
            WorldOfDungeons.view model
                |> Html.Styled.map WorldOfDungeonsMsg

editView : SheetModel -> Html SheetMsg
editView sheetModel =
    case sheetModel of
        FateSheet model ->
            Fate.editView model
                |> Html.Styled.map FateMsg

        WorldOfDungeonsSheet model ->
            WorldOfDungeons.editView model
                |> Html.Styled.map WorldOfDungeonsMsg

blank : GameType -> SheetModel
blank gameType =
    case gameType of
        Fate ->
            FateSheet Fate.blankCharacterSheet

        WorldOfDungeons ->
            WorldOfDungeonsSheet WorldOfDungeons.blankCharacterSheet


initialModel : GameType -> Array SheetModel
initialModel gameType =
    case gameType of
        Fate ->
            Array.fromList
                [ FateSheet Fate.blankGameAspectSheet ]

        WorldOfDungeons ->
            Array.fromList []
