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

port module Ports exposing (..)

import Json.Decode
import Json.Encode exposing (Value)


port loadGame : String -> Cmd msg
port gameLoaded : (Value -> msg) -> Sub msg
port gameLoadFailed : (Value -> msg) -> Sub msg
port authFailed : (Value -> msg) -> Sub msg
port changesReceived : (Value -> msg) -> Sub msg
port put : ( PouchDBRef, String, Value ) -> Cmd msg
port remove : ( PouchDBRef, String ) -> Cmd msg
port getGameListResponse : (Value -> msg) -> Sub msg

-- port sse_playerListUpdated : (Value -> msg) -> Sub msg

type alias PouchDBRef = Value

port dragstart : Value -> Cmd msg


type alias XMPPClientRef = Value

port xmpp_send : Value -> Cmd msg
port xmpp_received : (Value -> msg) -> Sub msg
port createChatClient : Value -> Cmd msg
port chatClientCreated : (Value -> msg) -> Sub msg
port destroyChatClient : XMPPClientRef -> Cmd msg
port chatClientConnected : (Value -> msg) -> Sub msg
