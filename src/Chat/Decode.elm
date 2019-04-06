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

module Chat.Decode
    exposing ( decodeStanza )

import Array exposing (Array)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time
import Chat.Types exposing (..)

------------------------------------------------------------
--| Messages and Presence
------------------------------------------------------------

decodeStanza : Value -> Stanza
decodeStanza value =
    case decodeValue stanzaDecoder value of
        Ok result -> result
        Err e -> StanzaDecodeError e

stanzaDecoder : Decoder Stanza
stanzaDecoder =
    field "stanzaType" string
        |> andThen
           (\type_ ->
                case type_ of
                    "message" -> map StanzaMessage messageDecoder
                    "presence" -> map StanzaPresence presenceDecoder
                    _ ->
                        fail ("Invalid Stanza type: " ++ type_)
           )

messageDecoder : Decoder Message
messageDecoder =
    map3 Message
    (field "from" jidDecoder)
    (field "body" string)
    (field "timestamp" timestampDecoder)

presenceDecoder : Decoder Presence
presenceDecoder =
    map2 Presence
    (field "from" jidDecoder)
    (field "type" presenceStatusDecoder)

jidDecoder : Decoder JID
jidDecoder =
    map3 JID
    (field "full" string)
    (field "bare" string)
    (field "resource" string)
    
timestampDecoder : Decoder Time.Posix
timestampDecoder =
    int |> andThen (succeed << Time.millisToPosix)

presenceStatusDecoder : Decoder PresenceStatus
presenceStatusDecoder =
    string
        |> andThen
           (\type_ ->
                case type_ of
                    "available" -> succeed Online
                    "unavailable" -> succeed Offline
                    _ ->
                        fail ("Invalid PresenceStatus: " ++ type_)
                       
           )
           
------------------------------------------------------------
--| Dice Rolls
------------------------------------------------------------

-- diceRollDecoder : Decoder DiceRoll
-- diceRollDecoder =
--     succeed (\a b c d e ->
--                  DiceRoll
--                  { type_ = a
--                  , request = b
--                  , results = c
--                  , modifier = d
--                  , total = e
--                  }
--             )
--         |> required "type" (string |> andThen diceTypeDecoder)
--         |> required "request" string
--         |> required "results" (list diceResultDecoder)
--         |> optional "modifier" (map Just int) Nothing
--         |> required "total" int

-- diceTypeDecoder : String -> Decoder Game.DiceType
-- diceTypeDecoder type_ =
--     case type_ of
--         "fate" -> succeed Game.DFate
--         "d20" -> succeed Game.D20
--         "d6" -> succeed Game.D6
--         _ ->
--             if String.startsWith "d" type_
--             then
--                 case String.toInt (String.dropLeft 1 type_) of
--                     Nothing ->
--                         fail ("Not a valid dice type: " ++ type_)
--                     Just sides ->
--                         succeed (Game.DOther sides)
--             else
--                 fail ("Not a valid dice type: " ++ type_)

-- diceResultDecoder : Decoder Game.DiceResult
-- diceResultDecoder =
--     field "ctor" string
--         |> andThen
--            (\ctor ->
--                 case ctor of
--                     "DFateResult" ->
--                         succeed Game.DFateResult
--                             |> required "face"
--                                (string |> andThen dFateFaceDecoder)
--                     "D20Result" ->
--                         succeed Game.D20Result
--                             |> required "face" int
--                     "D6Result" ->
--                         succeed Game.D6Result
--                             |> required "face" int
--                     "DOtherResult" ->
--                         succeed Game.DOtherResult
--                             |> required "sides" int
--                             |> required "face" int
--                     _ ->
--                         fail ("Not a valid DiceResult constructor: " ++ ctor)
--            )


-- dFateFaceDecoder : String -> Decoder Game.DFateFace
-- dFateFaceDecoder face =
--     case face of
--         "+" -> succeed Game.DFatePlus
--         "b" -> succeed Game.DFateBlank
--         "-" -> succeed Game.DFateMinus
--         _ ->
--             fail ("Not a valid DFateFace value: " ++ face)
