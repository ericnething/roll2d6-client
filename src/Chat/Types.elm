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
import Game.Person exposing (Person)

type alias Model =
    { ref : XMPPClientRef
    , room : JID
    , input : String
    , messages : List Message
    , roster : Dict String JID
    }

newModel : String -> Person -> Model
newModel roomName me =
    { ref = Json.Encode.null
    , room = { full = roomName ++ "@muc.localhost"
             , bare = roomName ++ "@muc.localhost"
             , resource = ""
             }
    , input = ""
    , messages = []
    , roster = Dict.fromList [(me.id, personToJID me)]
    }

type alias ConnectionInfo =
    { jid : String
    , password : String
    , room : String
    , username : String
    }

personToJID : Person -> JID
personToJID { id, username } =
    { full = id ++ "/" ++ username
    , bare = id
    , resource = username
    }

type alias JID =
    { full : String
    , bare : String
    , resource : String
    }

type alias NewMessage =
    { to : JID
    , body : String
    }

type Stanza
    = StanzaMessage Message
    | StanzaPresence Presence
    | StanzaDecodeError Json.Decode.Error

type alias Message =
    { from : JID
    , body : String
    , timestamp : Time.Posix
    }

type PresenceStatus
    = Online
    | Offline

type alias Presence =
    { from : JID
    , presence : PresenceStatus
    }


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
    | UpdateChatInput String
    | ResetChatInput
    | SendMessage NewMessage
    | KeyPressChatInput
    -- | DiceRollResult DiceRoll
    | NoOp
    | ClientConnected XMPPClientRef
