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

module Fate.GameAspectSheet.Encode exposing
    (encodeGameAspectSheet)

import Array exposing (Array)
import Fate.GameAspectSheet.Types exposing (..)
import Fate.CharacterSheet.Encode exposing (encodeAspect)
import Json.Encode exposing (..)

encodeGameAspectSheet : GameAspectSheet -> Value
encodeGameAspectSheet sheet =
    object
        [ ( "scenes", array encodeScene sheet.scenes )
        ]

encodeScene : Scene -> Value
encodeScene scene =
    object
        [ ( "title", string scene.title )
        , ( "aspects", array encodeAspect scene.aspects )
        ]
