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

module Game.Person exposing (..)

type alias PersonId = String

type alias Person =
    { id : PersonId
    , accessLevel : AccessLevel
    , username : String
    }

type Presence
    = Online
    | Offline

type alias PlayerPresence =
    { id : String
    , presence : Presence
    }

type AccessLevel
    = Owner
    | GameMaster
    | Player
