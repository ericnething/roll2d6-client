module Fate exposing
    ( view
    , editView
    , compactView
    , update
    , Sheet(..)
    , Msg(..)
    , blankCharacterSheet
    , blankGameAspectSheet
    )

import Html.Styled exposing (Html)

import Fate.CharacterSheet as CharacterSheet
import Fate.GameAspectSheet as GameAspectSheet
import Fate.CharacterSheet.Template

type Sheet
    = CharacterSheet CharacterSheet.Model
    | GameAspectSheet GameAspectSheet.Model

type alias Index = Int

type Msg
    = CharacterSheetMsg CharacterSheet.Msg
    | GameAspectSheetMsg GameAspectSheet.Msg

update : Msg -> Sheet -> (Sheet, Cmd Msg)
update msg model =
    case (msg, model) of
        (CharacterSheetMsg submsg, CharacterSheet sheet) ->
            let
                (updatedSheet, cmd) =
                    CharacterSheet.update submsg sheet
            in
                ( CharacterSheet updatedSheet
                , Cmd.map CharacterSheetMsg cmd
                )

        (GameAspectSheetMsg submsg, GameAspectSheet sheet) ->
            let
                (updatedSheet, cmd) =
                    GameAspectSheet.update submsg sheet
            in
                ( GameAspectSheet updatedSheet
                , Cmd.map GameAspectSheetMsg cmd
                )

        _ ->
            (model, Cmd.none)

compactView : Sheet -> Html Msg
compactView sheet =
    case sheet of
        CharacterSheet model ->
            CharacterSheet.compactView model
                |> Html.Styled.map CharacterSheetMsg
        GameAspectSheet model ->
            GameAspectSheet.view model
                |> Html.Styled.map GameAspectSheetMsg

view : Sheet -> Html Msg
view sheet =
    case sheet of
        CharacterSheet model ->
            CharacterSheet.view model
                |> Html.Styled.map CharacterSheetMsg
        GameAspectSheet model ->
            GameAspectSheet.view model
                |> Html.Styled.map GameAspectSheetMsg

editView : Sheet -> Html Msg
editView sheet =
    case sheet of
        CharacterSheet model ->
            CharacterSheet.editView model
                |> Html.Styled.map CharacterSheetMsg
        GameAspectSheet model ->
            GameAspectSheet.editView model
                |> Html.Styled.map GameAspectSheetMsg


blankCharacterSheet =
    CharacterSheet Fate.CharacterSheet.Template.blank

blankGameAspectSheet =
    GameAspectSheet GameAspectSheet.blank
