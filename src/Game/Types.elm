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

module Game.Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Debouncer.Messages as Debouncer
    exposing
    ( Debouncer
    , debounce
    , toDebouncer
    , fromSeconds
    )
import Ports exposing (PouchDBRef, XMPPClientRef)
import RemoteData exposing (WebData)
import Http
import Time
import Game.Sheet.Types
    exposing
    ( SheetMsg
    , SheetModel
    )
import Game.Sheets.Types as Sheets
    exposing
    ( FullSheet
    , SheetId
    , MovingSheet
    , SheetPermission
    )
import Game.GameType exposing (GameType(..))
import Game.Person exposing (..)

import Chat.Types as Chat

type alias Index = Int

type Overlay
    = EditGameSettings
    | InstantInvite (WebData String)
    | ManagePlayers
    | ManageSheetPermissions SheetId
    | OverlayNone

type alias Model =
    { ref : PouchDBRef
    , debouncer : Debouncer Msg
    , gameType : GameType
    , id : GameId
    , title : String
    , sheets : Dict SheetId SheetModel
    , fullSheet : Maybe FullSheet
    , overlay : Overlay
    , players : List Person
    , sheetsViewportX : Float
    , myPlayerInfo : Person
    , sheetsOrdering : Array SheetId
    , movingSheet : MovingSheet
    , sheetPermissions : Dict SheetId SheetPermission
    }


emptyGameModel : { ref : PouchDBRef
                 , gameId : GameId
                 , gameData : GameData
                 , sheets : Dict SheetId SheetModel
                 }
               -> Person
               -> List Person
               -> Model
emptyGameModel { ref, gameId, gameData, sheets }
               myPlayerInfo players =
    { ref = ref
    , debouncer =
        debounce (fromSeconds 1)
            |> toDebouncer
    , gameType = gameData.gameType
    , id = gameId
    , title = gameData.title
    , sheets = sheets
    , fullSheet = Nothing
    , overlay = OverlayNone
    , players = players
    , sheetsViewportX = 0
    , myPlayerInfo = myPlayerInfo
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


type alias GameId =
    String


mergeGameData : Model -> GameData -> Model
mergeGameData model gameData =
    { model
        | title = gameData.title
        , sheetsOrdering = gameData.sheetsOrdering
        , sheetPermissions = gameData.sheetPermissions
    }


emptyGameData : GameType -> GameData
emptyGameData gameType =
    { title = "New Game"
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

    | WriteSheetToPouchDB
          PouchDBRef
          SheetId
          GameType
          SheetModel

    | SheetsMsg Sheets.Msg
    | UpdateGameTitle String
    | UpdateGameTitleInDB
    | GameTitleUpdated
    | OpenOverlay Overlay
    | CloseOverlay
    | ChangesReceived Value
    | ExitToLobby
    | CreateInvite
    | InviteCreated (WebData String)
    | RemovePlayer PersonId
    | PlayerRemoved GameId PersonId (Result Http.Error String)
    | PlayerRemovedSuccess
    | ChatMsg Chat.Msg
