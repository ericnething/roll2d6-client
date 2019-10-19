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


module RedMarkets.CharacterSheet.Decode exposing (decodeCharacterSheet)

import Array exposing (Array)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import RedMarkets.CharacterSheet.Types exposing (..)


decodeCharacterSheet : Decoder CharacterSheet
decodeCharacterSheet =
    Decode.succeed CharacterSheet
        |> required "name" string
        |> required "description" string
        |> required "crew" string
        |> required "weakSpot" string
        |> required "softSpot" string
        |> required "toughSpot" string
        |> required "str" decodePotential
        |> required "spd" decodePotential
        |> required "adp" decodePotential
        |> required "int" decodePotential
        |> required "cha" decodePotential
        |> required "wil" decodePotential
        |> required "dependents" (array decodeRelation)
        |> required "references" (array decodeRelation)
        |> required "detachment" (array decodeThreat)
        |> required "stress" (array decodeThreat)
        |> required "trauma" (array decodeThreat)
        |> required "rightLegWounds" (array decodeWound)
        |> required "leftLegWounds" (array decodeWound)
        |> required "rightArmWounds" (array decodeWound)
        |> required "leftArmWounds" (array decodeWound)
        |> required "torsoWounds" (array decodeWound)
        |> required "headWounds" (array decodeWound)
        |> required "gear" (array decodeGear)
        |> required "notes" string


decodePotential : Decoder Potential
decodePotential =
    Decode.succeed Potential
        |> required "rating" int
        |> required "skills" (array decodeSkill)


decodeSkill : Decoder Skill
decodeSkill =
    Decode.succeed Skill
        |> required "title" string
        |> required "rating" int


decodeRelation : Decoder Relationship
decodeRelation =
    Decode.succeed Relationship
        |> required "person" string
        |> required "status" decodeRelationStatus


decodeRelationStatus : Decoder RelationStatus
decodeRelationStatus =
    string
        |> andThen
            (\status ->
                case status of
                    "Normal" ->
                        succeed Normal

                    "Needy" ->
                        succeed Needy

                    "Strained" ->
                        succeed Strained

                    "Severed" ->
                        succeed Severed

                    err ->
                        fail (err ++ " is not a valid RelationStatus")
            )


decodeThreat : Decoder Threat
decodeThreat =
    bool |> andThen (succeed << Threat)


decodeWound : Decoder Wound
decodeWound =
    string
        |> andThen
            (\wound ->
                case wound of
                    "NoWound" ->
                        succeed NoWound

                    "Stun" ->
                        succeed Stun

                    "Kill" ->
                        succeed Kill

                    err ->
                        fail (err ++ " is not a valid Wound")
            )


decodeGear : Decoder Gear
decodeGear =
    Decode.succeed
        (\title charges upkeep effect qualities ->
            { title = title
            , charges = charges
            , upkeep = upkeep
            , effect = effect
            , qualities = qualities
            }
        )
        |> required "title" string
        |> required "charges" (array decodeCharge)
        |> required "upkeep" int
        |> required "effect" string
        |> required "qualities" (array decodeGearQuality)


decodeGearQuality : Decoder GearQuality
decodeGearQuality =
    Decode.succeed
        (\title description ->
            { title = title
            , description = description
            }
        )
        |> required "title" string
        |> required "description" string


decodeCharge : Decoder Charge
decodeCharge =
    string
        |> andThen
            (\charge ->
                case charge of
                    "Charge" ->
                        succeed Charge

                    "NoCharge" ->
                        succeed NoCharge

                    err ->
                        fail (err ++ " is not a valid Charge")
            )
