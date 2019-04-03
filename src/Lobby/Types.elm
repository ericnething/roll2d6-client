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

module Lobby.Types exposing (..)

import Game.Types as Game
import Game.GameType as Game
import RemoteData exposing (RemoteData(..), WebData)
import Http


type alias Model =
    { games : WebData (List GameMetadata)
    , overlay : Overlay
    }


initialModel : Model
initialModel =
    { games = NotAsked
    , overlay = OverlayNone
    }


type alias GameMetadata =
    { id : Game.GameId
    , title : String
    }

type Overlay
    = OverlayNone
    | NewGameSettings NewGameSettingsModel

type alias NewGameSettingsModel =
    { title : String
    , gameType : Game.GameType
    }

type Msg
    = NewGame
    | NewGameResponse (WebData Game.GameId)
    | UpdateNewGameTitle String
    | UpdateNewGameType Game.GameType
    | GetGameList
    | SetGameList (WebData (List GameMetadata))
    | LoadGame Game.GameId
    | Logout
    | LogoutResponse (Result Http.Error String)
    | OpenOverlay Overlay
    | CloseOverlay
