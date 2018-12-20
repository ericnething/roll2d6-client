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

module Game.Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Ports exposing (PouchDBRef)
import RemoteData exposing (WebData)
import Http
import Time
import Game.Sheet.Types exposing (SheetMsg, SheetModel)
import Game.Sheets.Types as Sheets exposing (FullSheet, SheetId, MovingSheet)
import Game.GameType exposing (GameType(..))
import Game.Person exposing (..)

type alias Index = Int

type Overlay
    = EditSheet Int
    | EditGameSettings
    | InstantInvite (WebData String)
    | ManagePlayers
    | OverlayNone

type alias EventSourceRef = Value

type alias Model =
    { ref : PouchDBRef
    , eventSource : EventSourceRef
    , gameType : GameType
    , id : GameId
    , title : String
    , sheets : Dict SheetId SheetModel
    , fullSheet : Maybe FullSheet
    , overlay : Overlay
    , players : WebData (List Person)
    , chatInput : String
    , chatMessages : List ChatMessage
    , sheetsViewportX : Float
    , myPlayerInfo : Person
    , sheetsOrdering : Array SheetId
    , movingSheet : MovingSheet
    }


emptyGameModel : PouchDBRef
               -> GameId
               -> GameData
               -> EventSourceRef
               -> Person
               -> Model
emptyGameModel ref id gameData eventSource playerInfo =
    { ref = ref
    , eventSource = eventSource
    , gameType = gameData.gameType
    , id = id
    , title = gameData.title
    , sheets = gameData.sheets
    , fullSheet = Nothing
    , overlay = OverlayNone
    , players = RemoteData.Loading
    , chatInput = ""
    , chatMessages = []
    , sheetsViewportX = 0
    , myPlayerInfo = playerInfo
    , sheetsOrdering = gameData.sheetsOrdering
    , movingSheet = Sheets.NotMoving
    }


type alias GameData =
    { title : String
    , gameType : GameType
    , sheets : Dict SheetId SheetModel
    , sheetsOrdering : Array SheetId
    }


type alias GameId =
    String


mergeGameData : Model -> GameData -> Model
mergeGameData model gameData =
    { model
        | title = gameData.title
        , sheets = gameData.sheets
        , sheetsOrdering = gameData.sheetsOrdering
    }


emptyGameData : GameType -> GameData
emptyGameData gameType =
    { title = "New Game"
    , gameType = gameType
    , sheets = Dict.empty
    , sheetsOrdering = Array.fromList []
    }

type ServerEvent
    = PlayerListUpdated
        (Result Json.Decode.Error (List Person))
    | PlayerPresenceUpdated
        (Result Json.Decode.Error (List PlayerPresence))
    | ChatMessagesReceived
        (Result Json.Decode.Error (List ChatMessage))


type NewChatMessage
    = NewChatMessage String
    | NewDiceRollMessage DiceRoll


type ChatMessage
    = ChatMessage
      { timestamp : Time.Posix
      , playerId : Int
      , playerName : String
      , body : String
      }
    | DiceRollMessage
      { timestamp : Time.Posix
      , playerId : Int
      , playerName : String
      , result : DiceRoll
      }

type DiceType
    = DFate
    | D20
    | D6
    | DOther Int

type DiceResult
    = DFateResult DFateFace
    | D20Result Int
    | D6Result Int
    | DOtherResult Int Int

type DFateFace
    = DFatePlus
    | DFateBlank
    | DFateMinus

type DiceRoll =
    DiceRoll
    { type_ : DiceType
    , request : String
    , results : List DiceResult
    , modifier : Maybe Int
    , total : Int
    }

type DiceRollRequest =
    DiceRollRequest
    { size : Int
    , type_ : DiceType
    , modifier : Maybe Int
    }

-- Update

type Msg
    = NoOp
    | SheetsMsg Sheets.Msg
    | UpdateGameTitle String
    | OpenOverlay Overlay
    | CloseOverlay
    | UpdateCurrentGame Value
    | ChangesReceived
    | ExitToLobby
    | CreateInvite
    | InviteCreated (WebData String)
    | PlayerList GameId (WebData (List Person))
    | RemovePlayer Int
    | PlayerRemoved GameId (Result Http.Error String)
    | ServerEventReceived ServerEvent
    | Ping
    | Pong
    | UpdateChatInput String
    | ResetChatInput
    | SendChatMessage NewChatMessage
    | KeyPressChatInput
    | DiceRollResult DiceRoll
    | ChatLogReceived (Result Http.Error (List ChatMessage))

