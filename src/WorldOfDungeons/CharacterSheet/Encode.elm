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


module WorldOfDungeons.CharacterSheet.Encode exposing (encodeCharacterSheet)

import Array exposing (Array)
import Json.Encode exposing (..)
import WorldOfDungeons.CharacterSheet.Types exposing (..)


encodeCharacterSheet : CharacterSheet -> Value
encodeCharacterSheet sheet =
    object
        [ ( "name", string sheet.name )
        , ( "class", string sheet.class )
        , ( "level", int sheet.level )
        , ( "str", int sheet.str )
        , ( "dex", int sheet.dex )
        , ( "con", int sheet.con )
        , ( "int", int sheet.int )
        , ( "wis", int sheet.wis )
        , ( "cha", int sheet.cha )
        , ( "skills", string sheet.skills )
        , ( "abilities", array encodeAbility sheet.abilities )
        , ( "weapons", string sheet.weapons )
        , ( "equipment", string sheet.equipment )
        , ( "armor", encodeArmor sheet.armor )
        , ( "shield", encodeShield sheet.shield )
        , ( "bonusArmor", int sheet.bonusArmor )
        , ( "hitDice", int sheet.hitDice )
        , ( "hitPoints", maybeInt sheet.hitPoints )
        , ( "coin", maybeInt sheet.coin )
        , ( "xp", maybeInt sheet.xp )
        , ( "notes", string sheet.notes )
        ]


encodeAbility : Ability -> Value
encodeAbility (Ability title description) =
    object
        [ ( "title", string title )
        , ( "description", string description )
        ]


encodeArmor : Armor -> Value
encodeArmor armor =
    case armor of
        NoArmor ->
            string "NoArmor"

        LightArmor ->
            string "LightArmor"

        FullArmor ->
            string "FullArmor"


encodeShield : Shield -> Value
encodeShield shield =
    case shield of
        NoShield ->
            string "NoShield"

        Shield ->
            string "Shield"


maybeInt : Maybe Int -> Value
maybeInt mInt =
    case mInt of
        Just n ->
            int n

        Nothing ->
            null
