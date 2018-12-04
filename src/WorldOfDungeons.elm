module WorldOfDungeons exposing
    ( view
    , editView
    , update
    , Sheet(..)
    , Msg(..)
    , blankCharacterSheet
    )

import Html.Styled exposing (Html)

import WorldOfDungeons.CharacterSheet as CharacterSheet

type Sheet = CharacterSheet CharacterSheet.Model

type alias Index = Int

type Msg
    = CharacterSheetMsg CharacterSheet.Msg

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

view : Sheet -> Html Msg
view sheet =
    case sheet of
        CharacterSheet model ->
            CharacterSheet.view model
                |> Html.Styled.map CharacterSheetMsg

editView : Sheet -> Html Msg
editView sheet =
    case sheet of
        CharacterSheet model ->
            CharacterSheet.editView model
                |> Html.Styled.map CharacterSheetMsg

blankCharacterSheet =
    CharacterSheet CharacterSheet.blank
