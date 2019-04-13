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

module App.Types exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Url exposing (Url)
import Game.Types as Game exposing (GameId)
import Game.GameType as Game
import Game.Player as Game
import Game.Sheets.Types as Sheets
import Game.Sheet.Types as Sheet
import Json.Decode
import Lobby.Types as Lobby
import Ports exposing (XMPPClient)
import Route exposing (Route)
import Http
import Invite
import Chat.Types as Chat exposing (JID, Person)
import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { games : WebData (List Game.GameSummary)
    , overlay : Lobby.Overlay
    , me : Person
    , activeGame : ActiveGame
    , rooms : Dict Chat.RoomId Chat.Room
    }


type ActiveGame
    = ActiveGame Game.Model
    | LoadingGame GameId LoadingProgress
    | NoGame

type alias LoadingProgress =
    { myPlayerInfo : Maybe Game.Player
    , toGameModel : Maybe (Game.Player
                          -> List Game.Player
                          -> Game.Model)
    , players : Maybe (List Game.Player)
    }

emptyLoadingProgress =
    { myPlayerInfo = Nothing
    , toGameModel = Nothing
    , players = Nothing
    }


type Msg
    = GameMsg Game.Msg
    | ChatMsg Chat.Msg
    | LobbyMsg Lobby.Msg
    | GameLoaded Json.Decode.Value
    | GameLoadFailed
    | MyPlayerInfoLoaded (Result Http.Error Game.Player)
    | PlayerListLoaded (Result Http.Error (List Game.Player))
    | LoadGameScreen
      { toGameModel : Game.Player
                    -> List Game.Player
                    -> Game.Model
      , myPlayerInfo : Game.Player
      , players : List Game.Player
      }
    | AuthFailed
