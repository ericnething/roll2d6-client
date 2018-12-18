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

port module Ports exposing (..)

import Json.Decode
import Json.Encode exposing (Value)


port loadGame : ( Value, String ) -> Cmd msg
port gameLoaded : (Value -> msg) -> Sub msg
port gameLoadFailed : (Value -> msg) -> Sub msg
port authFailed : (Value -> msg) -> Sub msg
port changesReceived : (Value -> msg) -> Sub msg
port put : ( PouchDBRef, Value ) -> Cmd msg
port get : PouchDBRef -> Cmd msg
port getResponse : (Value -> msg) -> Sub msg
-- port allDocs : PouchDBRef -> Cmd msg
port getGameListResponse : (Value -> msg) -> Sub msg
port sse_playerListUpdated : (Value -> msg) -> Sub msg
port sse_playerPresenceUpdated : (Value -> msg) -> Sub msg
port sse_chatMessageReceived : (Value -> msg) -> Sub msg
port closeEventStream : Value -> Cmd msg
-- port sse_playerAdded : (Value -> msg) -> Sub msg
-- port sse_playerRemoved : (Value -> msg) -> Sub msg

type alias PouchDBRef =
    Value


port dragstart : Value -> Cmd msg
