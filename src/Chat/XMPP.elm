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

module Chat.XMPP exposing (..)

import Json.Decode
import Ports exposing (XMPPClient)
import Chat.Types exposing (RoomConn, NewMessage)
import Chat.Encode as Chat
import Json.Encode as JE exposing (Value)


type Command
    = JoinRoom RoomConn
    | LeaveRoom RoomConn
    | SendMessage NewMessage
    | Connect
    | Disconnect


encodeSend : XMPPClient -> Command -> Value
encodeSend ref command =
    JE.object
    [ ("client", ref)
    , ("command", JE.string (showCommand command))
    , ("data", encodeCommand command)
    ]

showCommand : Command -> String
showCommand command =
    case command of
        JoinRoom _ -> "joinRoom"
        LeaveRoom _ -> "leaveRoom"
        SendMessage _ -> "sendMessage"
        Connect -> "connect"
        Disconnect -> "disconnect"

encodeCommand : Command -> Value
encodeCommand command =
    case command of
        JoinRoom data -> Chat.encodeRoomConn data
        LeaveRoom data -> Chat.encodeRoomConn data
        SendMessage data -> Chat.encodeMessage data
        Connect -> JE.null
        Disconnect -> JE.null


xmpp_send : XMPPClient -> Command -> Cmd msg
xmpp_send client command =
    Ports.xmpp_send (encodeSend client command)

connect : XMPPClient -> Cmd msg
connect client =
    xmpp_send client Connect

disconnect : XMPPClient -> Cmd msg
disconnect client =
    xmpp_send client Disconnect

joinRoom : XMPPClient -> RoomConn -> Cmd msg
joinRoom client roomConn =
    xmpp_send client (JoinRoom roomConn)

leaveRoom : XMPPClient -> RoomConn -> Cmd msg
leaveRoom client roomConn =
    xmpp_send client (LeaveRoom roomConn)

sendMessage : XMPPClient -> NewMessage -> Cmd msg
sendMessage client message =
    xmpp_send client (SendMessage message)
