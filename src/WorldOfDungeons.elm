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
