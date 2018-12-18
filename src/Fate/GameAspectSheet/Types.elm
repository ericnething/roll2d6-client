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

module Fate.GameAspectSheet.Types exposing (..)

import Array exposing (Array)
import Fate.CharacterSheet.Types exposing (Aspect(..))

type alias Model = GameAspectSheet

type alias GameAspectSheet =
    { scenes : Array Scene }

emptyGameAspectSheet : GameAspectSheet
emptyGameAspectSheet =
    { scenes = Array.fromList
          [
           { title = "Game Aspects"
           , aspects =
               Array.fromList
                   [ Aspect "First game aspect" 0
                   , Aspect "Second game aspect" 0
                   ]
           }
          ]
    }

type alias Scene =
    { title : String
    , aspects : Array Aspect
    }

type alias Index = Int

type Msg
    = UpdateSceneTitle Index String
    | UpdateAspect Index Index Aspect
    | AddNewAspect Index String
    | RemoveAspect Index Index
    | AddNewScene
    | RemoveScene Index
        
