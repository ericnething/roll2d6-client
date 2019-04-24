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

module Game.Encode
    exposing
    ( encodeGame
    , encodeSheet
    , encodeGameData
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Game.Types as Game
import Game.GameType as Game
import Game.Sheet.Types as Sheet
import Game.Sheets.Types as Sheets
import Lobby.Types as Lobby
import Json.Encode exposing (..)
import Maybe

import Fate
import Fate.CharacterSheet.Encode as Fate
import Fate.GameAspectSheet.Encode as Fate

import WorldOfDungeons
import WorldOfDungeons.CharacterSheet.Encode as WorldOfDungeons

import RedMarkets
import RedMarkets.CharacterSheet.Encode as RedMarkets

--------------------------------------------------
-- Game Data
--------------------------------------------------

encodeGame : Game.Model -> Value
encodeGame game =
    object
        [ ( "_id", string "game" )
        , ( "title", string game.title )
        , ( "gameType", encodeGameType game.gameType )
        , ( "sheetsOrdering", array string game.sheetsOrdering )
        , ( "sheetPermissions" , dict identity encodeSheetPermission game.sheetPermissions)
        ]


encodeGameData : Game.GameData -> Value
encodeGameData game =
    object
        [ ( "_id", string "game" )
        , ( "title", string game.title )
        , ( "gameType", encodeGameType game.gameType )
        , ( "sheetsOrdering", array string game.sheetsOrdering )
        , ( "sheetPermissions" , dict identity encodeSheetPermission game.sheetPermissions)
        ]


encodeSheets : Dict Sheets.SheetId Sheet.SheetModel -> Value
encodeSheets sheets =
    dict identity encodeSheet sheets


encodeSheet : Sheet.SheetModel -> Value
encodeSheet sheetModel =
    case sheetModel of
        Sheet.FateSheet (Fate.CharacterSheet sheet) ->
            Fate.encodeCharacterSheet sheet
        Sheet.FateSheet (Fate.GameAspectSheet sheet) ->
            Fate.encodeGameAspectSheet sheet

        Sheet.WorldOfDungeonsSheet
            (WorldOfDungeons.CharacterSheet sheet) ->
                WorldOfDungeons.encodeCharacterSheet sheet

        Sheet.RedMarketsSheet
            (RedMarkets.CharacterSheet sheet) ->
                RedMarkets.encodeCharacterSheet sheet


encodeGameType : Game.GameType -> Value
encodeGameType gameType =
    case gameType of
        Game.Fate ->
            string "fate"

        Game.WorldOfDungeons ->
            string "world-of-dungeons"

        Game.RedMarkets ->
            string "red-markets"


encodeNewGameSettings : Lobby.NewGameFormModel -> Value
encodeNewGameSettings { title, gameType } =
    object
        [ ( "title", string title )
        , ( "gameType", encodeGameType gameType )
        ]

encodeSheetPermission : Sheets.SheetPermission -> Value
encodeSheetPermission permission =
    case permission of
        Sheets.SomePlayers playerIds ->
            object [ ( "somePlayers", list string playerIds) ]
            
        Sheets.AllPlayers ->
            object [ ( "allPlayers", bool True ) ]
