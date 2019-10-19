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


module Game.Sheet exposing (blank, compactView, editView, initialModel, updateSheet, view)

import Array exposing (Array)
import Fate
import Game.GameType exposing (GameType(..))
import Game.Sheet.Types exposing (SheetModel(..), SheetMsg(..))
import Html.Styled exposing (Html)
import RedMarkets
import WorldOfDungeons


updateSheet :
    SheetMsg
    -> SheetModel
    -> ( SheetModel, Cmd SheetMsg )
updateSheet sheetMsg sheetModel =
    case ( sheetMsg, sheetModel ) of
        ( FateMsg msg, FateSheet model ) ->
            let
                ( updatedModel, cmd ) =
                    Fate.update msg model
            in
            ( FateSheet updatedModel
            , Cmd.map FateMsg cmd
            )

        ( WorldOfDungeonsMsg msg, WorldOfDungeonsSheet model ) ->
            let
                ( updatedModel, cmd ) =
                    WorldOfDungeons.update msg model
            in
            ( WorldOfDungeonsSheet updatedModel
            , Cmd.map WorldOfDungeonsMsg cmd
            )

        ( RedMarketsMsg msg, RedMarketsSheet model ) ->
            let
                ( updatedModel, cmd ) =
                    RedMarkets.update msg model
            in
            ( RedMarketsSheet updatedModel
            , Cmd.map RedMarketsMsg cmd
            )

        _ ->
            ( sheetModel, Cmd.none )


compactView : SheetModel -> Html SheetMsg
compactView sheetModel =
    case sheetModel of
        FateSheet model ->
            Fate.compactView model
                |> Html.Styled.map FateMsg

        WorldOfDungeonsSheet model ->
            WorldOfDungeons.view model
                |> Html.Styled.map WorldOfDungeonsMsg

        RedMarketsSheet model ->
            RedMarkets.compactView model
                |> Html.Styled.map RedMarketsMsg


view : SheetModel -> Html SheetMsg
view sheetModel =
    case sheetModel of
        FateSheet model ->
            Fate.view model
                |> Html.Styled.map FateMsg

        WorldOfDungeonsSheet model ->
            WorldOfDungeons.view model
                |> Html.Styled.map WorldOfDungeonsMsg

        RedMarketsSheet model ->
            RedMarkets.view model
                |> Html.Styled.map RedMarketsMsg


editView : SheetModel -> Html SheetMsg
editView sheetModel =
    case sheetModel of
        FateSheet model ->
            Fate.editView model
                |> Html.Styled.map FateMsg

        WorldOfDungeonsSheet model ->
            WorldOfDungeons.editView model
                |> Html.Styled.map WorldOfDungeonsMsg

        RedMarketsSheet model ->
            RedMarkets.editView model
                |> Html.Styled.map RedMarketsMsg


blank : GameType -> List ( String, SheetModel )
blank gameType =
    case gameType of
        Fate ->
            [ ( "Character Sheet"
              , FateSheet Fate.blankCharacterSheet
              )
            , ( "Game Aspect Sheet"
              , FateSheet Fate.blankGameAspectSheet
              )
            ]

        WorldOfDungeons ->
            [ ( "Character Sheet"
              , WorldOfDungeonsSheet
                    WorldOfDungeons.blankCharacterSheet
              )
            ]

        RedMarkets ->
            [ ( "Character Sheet"
              , RedMarketsSheet
                    RedMarkets.blankCharacterSheet
              )
            ]


initialModel : GameType -> Array SheetModel
initialModel gameType =
    case gameType of
        Fate ->
            Array.fromList []

        WorldOfDungeons ->
            Array.fromList []

        RedMarkets ->
            Array.fromList []
