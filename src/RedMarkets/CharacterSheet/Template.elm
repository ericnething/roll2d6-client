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

module RedMarkets.CharacterSheet.Template exposing (blank)

import Array exposing (Array)
import RedMarkets.CharacterSheet.Types exposing (..)

emptyPotential : Potential
emptyPotential =
    Potential 1 (Array.fromList [])

blank : CharacterSheet
blank =
    { name = ""
    , description = ""
    , crew = ""
    , weakSpot = ""
    , softSpot = ""
    , toughSpot = ""
    , str = emptyPotential
    , spd = emptyPotential
    , adp = emptyPotential
    , int = emptyPotential
    , cha = emptyPotential
    , wil = emptyPotential
    , dependents = Array.fromList []
    , references = Array.fromList []
    , detachment = Array.repeat 15 (Threat False)
    , stress     = Array.repeat 15 (Threat False)
    , trauma     = Array.repeat 15 (Threat False)
    , rightLegWounds = Array.repeat 10 NoWound
    , leftLegWounds  = Array.repeat 10 NoWound
    , rightArmWounds = Array.repeat 10 NoWound
    , leftArmWounds  = Array.repeat 10 NoWound
    , torsoWounds    = Array.repeat 20 NoWound
    , headWounds     = Array.repeat 10 NoWound
    , gear = Array.fromList []
    , notes = ""
    }
