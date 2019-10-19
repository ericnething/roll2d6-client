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


module Game.Types exposing (GameData, GameId, GameSummary, GameUpdate, Index, ManagePlayersModel(..), Model, Msg(..), Overlay(..), SettingsTab(..), defaultAddPlayerModel, defaultRemovePlayerModel, emptyGameData, emptyGameModel, mergeGameData, switchToAddPlayer, switchToRemovePlayer)

import Array exposing (Array)
import Chat.Types as Chat
import Debouncer.Messages as Debouncer
    exposing
        ( Debouncer
        , debounce
        , fromSeconds
        , toDebouncer
        )
import Dict exposing (Dict)
import Game.GameType exposing (GameType(..))
import Game.Player exposing (..)
import Game.Sheet.Types
    exposing
        ( SheetModel
        , SheetMsg
        )
import Game.Sheets.Types as Sheets
    exposing
        ( FullSheet
        , MovingSheet
        , SheetId
        , SheetPermission
        )
import Http
import Json.Decode exposing (Value)
import Ports exposing (PouchDBRef, XMPPClient)
import RemoteData exposing (WebData)
import Time


type alias Index =
    Int


type Overlay
    = ShowOverlay
    | HideOverlay


type SettingsTab
    = ManagePlayersTab ManagePlayersModel
    | ChangeGameTitleTab
    | LeaveGameTab


type ManagePlayersModel
    = AddPlayerScreen { input : String }
    | RemovePlayerScreen { displayName : String, input : String }


defaultAddPlayerModel =
    AddPlayerScreen { input = "" }


defaultRemovePlayerModel displayName =
    RemovePlayerScreen { displayName = displayName, input = "" }


switchToAddPlayer =
    SwitchSettingsTab (ManagePlayersTab defaultAddPlayerModel)


switchToRemovePlayer displayName =
    SwitchSettingsTab (ManagePlayersTab (defaultRemovePlayerModel displayName))


type alias Model =
    { ref : PouchDBRef
    , debouncer : Debouncer Msg
    , overlay : Overlay

    -- Game Info
    , gameType : GameType
    , id : GameId
    , title : String
    , settingsTab : SettingsTab

    -- Players
    , players : List Player
    , myPlayer : Player

    -- Sheets
    , sheets : Dict SheetId SheetModel
    , fullSheet : Maybe FullSheet
    , sheetsViewportX : Float
    , sheetsOrdering : Array SheetId
    , movingSheet : MovingSheet
    , sheetPermissions : Dict SheetId SheetPermission
    }


emptyGameModel :
    { ref : PouchDBRef
    , gameId : GameId
    , gameData : GameData
    , sheets : Dict SheetId SheetModel
    }
    -> Player
    -> List Player
    -> Model
emptyGameModel { ref, gameId, gameData, sheets } myPlayer players =
    { ref = ref
    , debouncer = debounce (fromSeconds 1) |> toDebouncer
    , overlay = HideOverlay

    -- Game Info
    , gameType = gameData.gameType
    , id = gameId
    , title = gameData.title
    , settingsTab = ManagePlayersTab defaultAddPlayerModel

    -- Players
    , players = players
    , myPlayer = myPlayer

    -- Sheets
    , sheets = sheets
    , fullSheet = Nothing
    , sheetsViewportX = 0
    , sheetsOrdering = gameData.sheetsOrdering
    , movingSheet = Sheets.NotMoving
    , sheetPermissions = gameData.sheetPermissions
    }


type alias GameData =
    { title : String
    , gameType : GameType
    , sheetsOrdering : Array SheetId
    , sheetPermissions : Dict SheetId SheetPermission
    }


type alias GameSummary =
    { id : GameId
    , title : String
    , gameType : GameType
    }


type alias GameId =
    String


mergeGameData : Model -> GameData -> Model
mergeGameData model gameData =
    { model
        | title = gameData.title
        , sheetsOrdering = gameData.sheetsOrdering
        , sheetPermissions = gameData.sheetPermissions
    }


emptyGameData : String -> GameType -> GameData
emptyGameData title gameType =
    { title = title
    , gameType = gameType
    , sheetsOrdering = Array.fromList []
    , sheetPermissions = Dict.empty
    }



-- Update


type alias GameUpdate =
    { maybeGame : Maybe GameData
    , sheets : Dict SheetId SheetModel
    }


type Msg
    = NoOp
    | DebounceMsg (Debouncer.Msg Msg)
    | WriteGameToPouchDB PouchDBRef String GameData
    | WriteSheetToPouchDB PouchDBRef SheetId GameType SheetModel
    | SheetsMsg Sheets.Msg
    | UpdateGameTitle String
    | UpdateGameTitleInDB
    | GameTitleUpdated
    | OpenOverlay
    | CloseOverlay
    | ChangesReceived Value
    | ExitToLobby
    | SwitchToLobby
    | RemovePlayer PlayerId
    | PlayerRemoved GameId PlayerId (Result Http.Error String)
    | PlayerRemovedSuccess
    | ChatMsg Chat.Msg
    | SwitchTab
    | SwitchSettingsTab SettingsTab
