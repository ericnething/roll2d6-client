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

module Game.Encode
    exposing
    ( encodeGame
    , encodeGameData
    , encodeChatMessage
    )

import Array exposing (Array)
import Game.Types as Game
import Game.GameType as Game
import Game.Sheet.Types as Sheet
import Lobby.Types as Lobby
import Json.Encode exposing (..)
import Maybe

import Fate
import Fate.CharacterSheet.Encode as Fate
import Fate.GameAspectSheet.Encode as Fate

import WorldOfDungeons
import WorldOfDungeons.CharacterSheet.Encode as WorldOfDungeons

--------------------------------------------------
-- Chat Messages and Dice Rolls
--------------------------------------------------

encodeChatMessage : Game.NewChatMessage -> Value
encodeChatMessage message =
    case message of
        Game.NewChatMessage body ->
            object
                [ ( "ctor", string "ChatMessage" )
                , ( "body", string body )
                ]
        Game.NewDiceRollMessage roll ->
            object
                [ ( "ctor", string "DiceRollMessage" )
                , ( "result", encodeDiceRoll roll )
                ]

encodeDiceRoll : Game.DiceRoll -> Value
encodeDiceRoll (Game.DiceRoll roll) =
    object
        [ ( "type", encodeDiceType roll.type_ )
        , ( "request", string roll.request )
        , ( "results", list encodeDiceResult roll.results )
        , ( "modifier", Maybe.withDefault null (Maybe.map int roll.modifier) )
        , ( "total", int roll.total )
        ]

encodeDiceType : Game.DiceType -> Value
encodeDiceType type_ =
    case type_ of
        Game.DFate -> string "fate"
        Game.D20 -> string "d20"
        Game.D6 -> string "d6"
        Game.DOther n -> string ("d" ++ String.fromInt n)

encodeDiceResult : Game.DiceResult -> Value
encodeDiceResult result =
    case result of
        Game.DFateResult face ->
            object
            [ ( "ctor", string "DFateResult" )
            , ( "face", encodeDFateFace face )
            ]
        Game.D20Result n ->
            object
            [ ( "ctor", string "D20Result" )
            , ( "face", int n )
            ]
        Game.D6Result n ->
            object
            [ ( "ctor", string "D6Result" )
            , ( "face", int n )
            ]
        Game.DOtherResult sides n ->
            object
            [ ( "ctor", string "DOtherResult" )
            , ( "sides", int sides )
            , ( "face", int n )
            ]

encodeDFateFace : Game.DFateFace -> Value
encodeDFateFace face =
    case face of
        Game.DFatePlus -> string "+"
        Game.DFateBlank -> string "b"
        Game.DFateMinus -> string "-"

--------------------------------------------------
-- Game Data
--------------------------------------------------

encodeGame : Game.Model -> Value
encodeGame game =
    object
        [ ( "_id", string "game" )
        , ( "title", string game.title )
        , ( "gameType", encodeGameType game.gameType )
        , ( "sheets", array encodeSheet game.sheets )
        ]


encodeGameData : Game.GameData -> Value
encodeGameData game =
    object
        [ ( "_id", string "game" )
        , ( "title", string game.title )
        , ( "gameType", encodeGameType game.gameType )
        , ( "sheets", array encodeSheet game.sheets )
        ]

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

encodeGameType : Game.GameType -> Value
encodeGameType gameType =
    case gameType of
        Game.Fate ->
            string "fate"

        Game.WorldOfDungeons ->
            string "world-of-dungeons"


encodeNewGameSettings : Lobby.NewGameSettingsModel -> Value
encodeNewGameSettings { title, gameType } =
    object
        [ ( "title", string title )
        , ( "gameType", encodeGameType gameType )
        ]
