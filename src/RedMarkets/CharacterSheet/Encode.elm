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

module RedMarkets.CharacterSheet.Encode exposing
    ( encodeCharacterSheet )

import Array exposing (Array)
import RedMarkets.CharacterSheet.Types exposing (..)
import Json.Encode exposing (..)


encodeCharacterSheet : CharacterSheet -> Value
encodeCharacterSheet model =
    object
        [ ( "name", string model.name )
        , ( "description", string model.description )
        , ( "crew", string model.crew )
        , ( "weakSpot", string model.weakSpot )
        , ( "softSpot", string model.softSpot )
        , ( "toughSpot", string model.toughSpot )
        , ( "str", encodePotential model.str )
        , ( "spd", encodePotential model.spd )
        , ( "adp", encodePotential model.adp )
        , ( "int", encodePotential model.int )
        , ( "cha", encodePotential model.cha )
        , ( "wil", encodePotential model.wil )
        , ( "dependents", array encodeRelation model.dependents )
        , ( "references", array encodeRelation model.references )
        , ( "detachment", array encodeThreat model.detachment )
        , ( "stress", array encodeThreat model.stress )
        , ( "trauma", array encodeThreat model.trauma )
        , ( "rightLegWounds", array encodeWound model.rightLegWounds )
        , ( "leftLegWounds", array encodeWound model.leftLegWounds )
        , ( "rightArmWounds", array encodeWound model.rightArmWounds )
        , ( "leftArmWounds", array encodeWound model.leftArmWounds )
        , ( "torsoWounds", array encodeWound model.torsoWounds )
        , ( "headWounds", array encodeWound model.headWounds )
        , ( "gear", array encodeGear model.gear )
        , ( "notes", string model.notes )
        ]

encodePotential : Potential -> Value
encodePotential (Potential rating skills) =
    object
        [ ( "rating", int rating )
        , ( "skills", array encodeSkill skills )
        ]

encodeSkill : Skill -> Value
encodeSkill (Skill title rating) =
    object
        [ ( "title", string title )
        , ( "rating", int rating )
        ]

encodeRelation : Relationship -> Value
encodeRelation (Relationship person status) =
    object
        [ ( "person", string person )
        , ( "status", encodeRelationStatus status )
        ]

encodeRelationStatus : RelationStatus -> Value
encodeRelationStatus status =
    string <|
        case status of
            Normal ->
                "Normal"

            Needy ->
                "Needy"

            Strained ->
                "Strained"

            Severed ->
                "Severed"


encodeThreat : Threat -> Value
encodeThreat (Threat isMarked) =
    bool isMarked        


encodeWound : Wound -> Value
encodeWound wound =
    string <|
        case wound of
            NoWound ->
                "NoWound"

            Stun ->
                "Stun"

            Kill ->
                "Kill"


encodeGear : Gear -> Value
encodeGear gear =
    object
        [ ( "title", string gear.title )
        , ( "charges", array encodeCharge gear.charges )
        , ( "upkeep", int gear.upkeep )
        , ( "effect", string gear.effect )
        , ( "qualities", array encodeGearQuality gear.qualities )
        , ( "upgrades", array encodeGearUpgrade gear.upgrades )
        ]


encodeCharge : Charge -> Value
encodeCharge charge =
    case charge of
        Charge ->
            string "Charge"

        NoCharge ->
            string "NoCharge"


encodeGearQuality : GearQuality -> Value
encodeGearQuality { title, description } =
    object
        [ ( "title", string title )
        , ( "description", string description )
        ]


encodeGearUpgrade : GearUpgrade -> Value
encodeGearUpgrade { title, description, purchased } =
    object
        [ ( "title", string title )
        , ( "description", string description )
        , ( "purchased", bool purchased )
        ]
