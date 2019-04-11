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

module Chat.Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Browser.Dom as Dom
import Json.Decode
import Json.Encode
import Ports exposing (XMPPClientRef)
import Time

type alias Model r =
    { r |
      xmppClientRef : XMPPClientRef
    , rooms : Dict BareJID Room
    , me : Person
    -- , friends : Dict PersonId Person
    }

------------------------------------------------------------
--| Person
------------------------------------------------------------

type alias PersonId = String

type alias Person =
    { id : PersonId
    , displayName : String
    , presence : Presence
    }

type Presence
    = Online
    | Offline

personToJID : Person -> JID
personToJID { id, displayName } =
    { full = id ++ "/" ++ displayName
    , bare = id
    , resource = displayName
    }

------------------------------------------------------------
--| Conversation
------------------------------------------------------------

type alias Conversation =
    { with : PersonId
    , messages : List Message
    , input : String
    }

------------------------------------------------------------
--| Room
------------------------------------------------------------

type alias RoomConn =
    { room : String
    , displayName : String
    }

type alias RoomId = String

type alias Room =
    { id : RoomId
    , input : String
    , messages : List Message
    , roster : Dict PersonId Person
    }

------------------------------------------------------------
--| JID
------------------------------------------------------------

type alias JID =
    { full : String
    , bare : String
    , resource : String
    }

type alias BareJID = String

------------------------------------------------------------
--| Messages
------------------------------------------------------------

type alias NewMessage =
    { to : BareJID
    , body : String
    }

type alias Message =
    { from : JID
    , body : String
    , timestamp : Time.Posix
    }

------------------------------------------------------------
--| Stanzas
------------------------------------------------------------

type Stanza
    = MessageStanza Message
    | PresenceStanza (JID, Presence)
    | StanzaDecodeError Json.Decode.Error

------------------------------------------------------------
--| Dice Rolling
------------------------------------------------------------

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

------------------------------------------------------------
--| Msg
------------------------------------------------------------
type Msg
    = StanzaReceived Stanza
    | UpdateChatInput BareJID String
    | SendMessage NewMessage
    | EnterKeyPressed BareJID String
    -- | DiceRollResult DiceRoll
    | NoOp
    | ClientConnected
    | JoinRoom RoomId
    | LeaveRoom RoomId
