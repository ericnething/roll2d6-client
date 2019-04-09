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
import Url exposing (Url)
import Game.Types as Game exposing (GameId)
import Game.GameType as Game
import Game.Person as Game
import Game.Sheets.Types as Sheets
import Game.Sheet.Types as Sheet
import Json.Decode
import Lobby.Types as Lobby
import Ports exposing (PouchDBRef)
import Route exposing (Route)
import Http
import Invite
import Chat.Types as Chat

type alias Flags =
    { windowSize : { width : Int
                   , height : Int
                   }
    , credentials : { jid : Maybe String
                    , username : Maybe String
                    , password : Maybe String
                    }
    }

type alias Model =
    { screen : Screen
    , chat : Chat.Model
    -- , xmppClientRef : XMPPClientRef
    -- , friends : Dict BareJID Person
    -- , conversations : Dict BareJID Conversation
    -- , games : Dict GameId GameInfo
    -- , me : Person
    -- , activeTab : Tab
    -- , currentGame : CurrentGame
    }

-- type CurrentGame
--     = CurrentGame Game
--     | LoadingGame GameId
--     | NoGame

-- type Tab
--     = ChatTab
--     | GameTab

-- type alias Conversation =
--     { with : JID
--     , messages : List Message
--     , input : String
--     }

-- type alias GameInfo =
--     { id : GameId
--     , room : JID
--     , gameType : GameType
--     , players : List Person
--     }

-- type alias Person =
--     { jid : JID
--     , displayName : String
--     , presence : PresenceStatus
--     }

type alias LoadingProgress =
    { myPlayerInfo : Maybe Game.Person
    , toGameModel : Maybe (Game.Person
                          -> List Game.Person
                          -> Game.Model)
    , players : Maybe (List Game.Person)
    }

emptyLoadingProgress =
    { myPlayerInfo = Nothing
    , toGameModel = Nothing
    , players = Nothing
    }    

type Screen
    = LobbyScreen Lobby.Model
    | LoadingScreen LoadingProgress
    | GameScreen Game.Model
    | InviteScreen Invite.Model


type Msg
    = GameMsg Game.Msg
    | ChatMsg Chat.Msg
    | LobbyMsg Lobby.Msg
    | GameLoaded Json.Decode.Value
    | GameLoadFailed
    | MyPlayerInfoLoaded (Result Http.Error Game.Person)
    | PlayerListLoaded (Result Http.Error (List Game.Person))
    | LoadGameScreen
      { toGameModel : Game.Person
                    -> List Game.Person
                    -> Game.Model
      , myPlayerInfo : Game.Person
      , players : List Game.Person
      }
    | AuthFailed
    | InviteMsg Invite.Msg
