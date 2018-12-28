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

module RedMarkets.CharacterSheet.Types exposing (..)

import Array exposing (Array)


type alias Model = CharacterSheet

type alias CharacterSheet =
    { name           : String
    , description    : String
    , crew           : String
    , weakSpot       : String
    , softSpot       : String
    , toughSpot      : String
    , str            : Potential
    , spd            : Potential
    , adp            : Potential
    , int            : Potential
    , cha            : Potential
    , wil            : Potential
    , dependants     : Array Relationship
    , references     : Array Relationship
    , detachment     : Threat
    , stress         : Threat
    , trauma         : Threat
    , rightLegWounds : Array Wound
    , leftLegWounds  : Array Wound
    , rightArmWounds : Array Wound
    , leftArmWounds  : Array Wound
    , torsoWounds    : Array Wound
    , headWounds     : Array Wound
    , gear           : Array Gear
    , notes          : String
    }

type Potential
    = Potential Int (Array Skill)

type PotentialType
    = Strength
    | Speed
    | Adaptability
    | Intelligence
    | Charm
    | Will

type Skill
    = Skill String Int

type Relationship
    = Relationship String RelationStatus

type RelationStatus
    = Normal
    | Needy
    | Strained
    | Severed

type Threat
    = Threat Int

showRelationStatus : RelationStatus -> String
showRelationStatus status =
    case status of
        Normal ->
            "Normal"

        Needy ->
            "Needy"

        Strained ->
            "Strained"

        Severed ->
            "Severed"


relationStatusSucc : RelationStatus -> RelationStatus
relationStatusSucc status =
    case status of
        Normal ->
            Needy

        Needy ->
            Strained

        Strained ->
            Severed

        Severed ->
            Severed

relationStatusPred : RelationStatus -> RelationStatus
relationStatusPred status =
    case status of
        Severed ->
            Strained

        Strained ->
            Needy

        Needy ->
            Normal

        Normal ->
            Normal


type Wound
    = NoWound
    | Stun
    | Kill


woundSucc : Wound -> Wound
woundSucc wound =
    case wound of
        NoWound ->
            Stun

        Stun ->
            Kill

        Kill ->
            NoWound

type WoundLocation
    = RightLeg
    | LeftLeg
    | RightArm
    | LeftArm
    | Torso
    | Head

showWoundLocation : WoundLocation -> String
showWoundLocation location =
    case location of
        RightLeg ->
            "Right Leg (1-2)"

        LeftLeg ->
            "Left Leg (3-4)"

        RightArm ->
            "Right Arm (5)"

        LeftArm ->
            "Left Arm (6)"

        Torso ->
            "Torso (7-9)"

        Head ->
            "Head (10)"

type alias Gear =
    { title : String
    , charges : Int
    , upkeep : Int
    , effect : String
    , qualities : String
    , upgrades : String
    }

type alias Index =
    Int


type Msg
    = UpdateName String
    | UpdateDescription String
    | UpdateCrew String
    | UpdateWeakSpot String
    | UpdateSoftSpot String
    | UpdateToughSpot String
      -- Potentials
    | UpdatePotential PotentialType Int
      -- Skills
    | UpdateSkill PotentialType Index Skill
    | AddNewSkill PotentialType String
    | RemoveSkill PotentialType Index
      -- Dependants
    | UpdateDependant Index Relationship
    | AddNewDependant Relationship
    | RemoveDependant Index
      -- References
    | UpdateReference Index Relationship
    | AddNewReference Relationship
    | RemoveReference Index
      -- Threats
    | UpdateDetachment Int
    | UpdateStress Int
    | UpdateTrauma Int
      -- Wounds
    | UpdateWound WoundLocation Index Wound
      -- Gear
    | UpdateGear Index Gear
    | AddNewGear Gear
    | RemoveGear Index
      -- Notes
    | UpdateNotes String
