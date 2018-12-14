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

module Chat.Parser
    exposing
    ( parseDiceRollRequest
    , isDiceRollRequest
    )

import Game.Types exposing (DiceRollRequest(..), DiceType(..))
import Parser
    exposing
    ( Parser
    , run
    , DeadEnd
    , oneOf
    , (|=)
    , (|.)
    , succeed
    , andThen
    , int
    , symbol
    )
import Char

parseDiceRollRequest : String
                     -> Result (List DeadEnd) DiceRollRequest
parseDiceRollRequest raw =
    let
        cleanInput =
            String.toLower
            >> String.filter
                (\char ->
                     Char.isAlphaNum char
                     || '+' == char
                     || '-' == char
                )
    in               
        run diceRollRequestParser (cleanInput raw)


diceRollRequestParser : Parser DiceRollRequest
diceRollRequestParser =
    succeed (\size type_ modifier ->
                 DiceRollRequest
                 { size = size
                 , type_ = type_
                 , modifier = modifier
                 }
            )
        |= dicePoolSizeParser
        |= diceTypeParser
        |= diceModifierParser

dicePoolSizeParser : Parser Int
dicePoolSizeParser =
    oneOf
    [ succeed identity
        |= int
        |. symbol "d"
    , succeed 1
        |. symbol "d"
    ]

diceTypeParser : Parser DiceType
diceTypeParser =
    oneOf
        [ succeed DFate
            |. symbol "f"
        , int |> andThen
              (\n ->
                   case n of
                       20 -> succeed D20
                       6 -> succeed D6
                       _ -> succeed (DOther n)
              )
        ]

diceModifierParser : Parser (Maybe Int)
diceModifierParser =
  oneOf
    [ succeed (Just << negate)
        |. symbol "-"
        |= int
    , succeed Just
        |. symbol "+"
        |= int
    , succeed Nothing
    ]



isDiceRollRequest : String -> Maybe String
isDiceRollRequest raw =
    if String.startsWith "/roll " (String.trim raw)
    then Just (String.dropLeft 6 raw)
    else Nothing
