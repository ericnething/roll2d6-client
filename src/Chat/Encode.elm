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

module Chat.Encode
    exposing
    ( encodeMessage
    , encodeConnectionInfo
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode exposing (..)
import Maybe
import Chat.Types exposing (..)


encodeConnectionInfo : ConnectionInfo -> Value
encodeConnectionInfo { jid, password, room, username } =
    object
        [ ("jid", string jid)
        , ("password", string password)
        , ("room", string (room ++ "@muc.localhost"))
        , ("username", string username)
        ]

--------------------------------------------------
-- Chat Messages and Dice Rolls
--------------------------------------------------

encodeMessage : NewMessage -> Value
encodeMessage { to, body } =
    object
        [ ("to", encodeJID to)
        , ("body", string body)
        ]

encodeJID : JID -> Value
encodeJID { bare } =
    string bare

-- encodeDiceRoll : DiceRoll -> Value
-- encodeDiceRoll (DiceRoll roll) =
--     object
--         [ ( "type", encodeDiceType roll.type_ )
--         , ( "request", string roll.request )
--         , ( "results", list encodeDiceResult roll.results )
--         , ( "modifier", Maybe.withDefault null (Maybe.map int roll.modifier) )
--         , ( "total", int roll.total )
--         ]

-- encodeDiceType : DiceType -> Value
-- encodeDiceType type_ =
--     case type_ of
--         DFate -> string "fate"
--         D20 -> string "d20"
--         D6 -> string "d6"
--         DOther n -> string ("d" ++ String.fromInt n)

-- encodeDiceResult : DiceResult -> Value
-- encodeDiceResult result =
--     case result of
--         DFateResult face ->
--             object
--             [ ( "ctor", string "DFateResult" )
--             , ( "face", encodeDFateFace face )
--             ]
--         D20Result n ->
--             object
--             [ ( "ctor", string "D20Result" )
--             , ( "face", int n )
--             ]
--         D6Result n ->
--             object
--             [ ( "ctor", string "D6Result" )
--             , ( "face", int n )
--             ]
--         DOtherResult sides n ->
--             object
--             [ ( "ctor", string "DOtherResult" )
--             , ( "sides", int sides )
--             , ( "face", int n )
--             ]

-- encodeDFateFace : DFateFace -> Value
-- encodeDFateFace face =
--     case face of
--         DFatePlus -> string "+"
--         DFateBlank -> string "b"
--         DFateMinus -> string "-"

