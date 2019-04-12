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

module Game.Decode exposing
    ( decodeGame
    , decodeGameData
    , decodeGameId
    , decodeGameList
    , gameIdDecoder
    , gameListDecoder
    , playerListDecoder
    , playerDecoder
    , decodePlayerList
    , scrollDecoder
    , decodeSheetUpdate
    , decodeChanges
    )

import Array exposing (Array)
import Game.Types as Game exposing (GameSummary)
import Game.GameType as Game
import Game.Player as Game
import Game.Sheets.Types as Sheets
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import RemoteData
import Time
import Game.Sheet.Types as Sheet

import Fate
import Fate.CharacterSheet.Decode as Fate
import Fate.GameAspectSheet.Decode as Fate

import WorldOfDungeons
import WorldOfDungeons.CharacterSheet.Decode as WorldOfDungeons

import RedMarkets
import RedMarkets.CharacterSheet.Decode as RedMarkets


--------------------------------------------------
-- Scroll events
--------------------------------------------------

scrollDecoder : (Int -> msg) -> Decoder msg
scrollDecoder toMsg =
    map toMsg (at ["target", "scrollLeft"] int)

--------------------------------------------------
-- Player List
--------------------------------------------------

decodePlayerList : Value -> Result Error (List Game.Player)
decodePlayerList value =
    decodeValue playerListDecoder value

playerListDecoder : Decoder (List Game.Player)
playerListDecoder =
    list playerDecoder

playerDecoder : Decoder Game.Player
playerDecoder =
    succeed Game.Player
        |> required "id" string
        |> required "access" roleDecoder
        |> required "username" string
        |> hardcoded Game.Online
        -- |> required "role" roleDecoder
        -- |> required "displayName" string
        -- |> required "presence" presenceDecoder

roleDecoder : Decoder Game.Role
roleDecoder =
    string |> andThen (\role ->
        case String.toLower role of
            "owner" ->
                succeed Game.OwnerRole
            "game master" ->
                succeed Game.GameMasterRole
            "player" ->
                succeed Game.PlayerRole
            _ ->
                fail "Not a valid Role"
    )

presenceDecoder : Decoder Game.Presence
presenceDecoder =
    string |> andThen (\presence ->
        case String.toLower presence of
            "online" ->
                succeed Game.Online
            "offline" ->
                succeed Game.Offline
            _ ->
                fail "Not a valid Presence"
    )

--------------------------------------------------
-- Game Metadata List
--------------------------------------------------

decodeGameList : Value -> Result Error (List GameSummary)
decodeGameList value =
    decodeValue gameListDecoder value


gameListDecoder : Decoder (List GameSummary)
gameListDecoder =
    list gameSummaryDecoder


gameSummaryDecoder : Decoder GameSummary
gameSummaryDecoder =
    succeed GameSummary
        |> required "id" string
        |> required "title" string
        |> required "gameType" gameTypeDecoder


--------------------------------------------------
-- Game
--------------------------------------------------

type alias ToGame =
    Game.Player -> List Game.Player -> Game.Model

decodeGameId : Value -> Result Error Game.GameId
decodeGameId value =
    decodeValue gameIdDecoder value


gameIdDecoder : Decoder Game.GameId
gameIdDecoder =
    string


decodeGame : Value -> Result Error ToGame
decodeGame value =
    decodeValue gameDecoder value


gameDecoder : Decoder ToGame
gameDecoder =
    at [ "game", "gameType" ] gameTypeDecoder
        |> andThen
           (\gameType ->
                map4 (\ref id game sheets ->
                          Game.emptyGameModel
                              { ref = ref
                              , gameId = id
                              , gameData = game
                              , sheets = sheets
                              }
                     )
                (field "ref" value)
                (field "id" string)
                (field "game" gameDataDecoder)
                (field "sheets" (dict (sheetDecoder gameType)))
           )


decodeGameData : Value -> Result Error (Game.GameData)
decodeGameData value =
    decodeValue gameDataDecoder value


gameDataDecoder : Decoder (Game.GameData)
gameDataDecoder =
    map4 Game.GameData
        (field "title" string)
        (field "gameType" gameTypeDecoder)
        (field "sheetsOrdering" (array string))
        (field "sheetPermissions" (dict sheetPermissionDecoder))


decodeSheetUpdate : Game.GameType
                  -> Value
                  -> Result Error { id : Sheets.SheetId
                                  , sheet : Sheet.SheetModel
                                  }
decodeSheetUpdate gameType value =
    decodeValue (sheetUpdateDecoder gameType) value

sheetUpdateDecoder : Game.GameType
                   -> Decoder { id : Sheets.SheetId
                              , sheet : Sheet.SheetModel
                              }
sheetUpdateDecoder gameType =
    map2 (\id sheet -> { id = id, sheet = sheet })
        (field "id" string)
        (field "sheet" (sheetDecoder gameType))


sheetPermissionDecoder : Decoder Sheets.SheetPermission
sheetPermissionDecoder =
    oneOf
        [ (field "somePlayers" (list string))
              |> Decode.map Sheets.SomePlayers
        , (field "allPlayers" bool)
              |> Decode.map (always Sheets.AllPlayers)
        ]


sheetDecoder : Game.GameType -> Decoder (Sheet.SheetModel)
sheetDecoder gameType =
    case gameType of
        Game.Fate ->
            fateSheetDecoder
                
        Game.WorldOfDungeons ->
            worldOfDungeonsSheetDecoder

        Game.RedMarkets ->
            redMarketsSheetDecoder


fateSheetDecoder : Decoder (Sheet.SheetModel)
fateSheetDecoder =
    oneOf
        [ Fate.decodeCharacterSheet
            |> Decode.map
               (Sheet.FateSheet << Fate.CharacterSheet)
        , Fate.decodeGameAspectSheet
            |> Decode.map
               (Sheet.FateSheet << Fate.GameAspectSheet)
        ]

worldOfDungeonsSheetDecoder : Decoder (Sheet.SheetModel)
worldOfDungeonsSheetDecoder =
    WorldOfDungeons.decodeCharacterSheet
        |> Decode.map
           (Sheet.WorldOfDungeonsSheet <<
                WorldOfDungeons.CharacterSheet)

redMarketsSheetDecoder : Decoder (Sheet.SheetModel)
redMarketsSheetDecoder =
    RedMarkets.decodeCharacterSheet
        |> Decode.map
           (Sheet.RedMarketsSheet <<
                RedMarkets.CharacterSheet)

gameTypeDecoder : Decoder Game.GameType
gameTypeDecoder =
    let
        toGameType gameType =
            case gameType of
                "fate" ->
                    succeed Game.Fate
                        
                "world-of-dungeons" ->
                    succeed Game.WorldOfDungeons

                "red-markets" ->
                    succeed Game.RedMarkets
            
                _ ->
                    fail "Not a valid GameType"
    in
        string |> andThen toGameType


decodeChanges : Game.GameType
              -> Value
              -> Result Error Game.GameUpdate
decodeChanges gameType value =
    decodeValue (changesDecoder gameType) value
        
changesDecoder : Game.GameType -> Decoder Game.GameUpdate
changesDecoder gameType =
    map2 Game.GameUpdate
        (field "game" (maybe gameDataDecoder))
        (field "sheets" (dict (sheetDecoder gameType)))
