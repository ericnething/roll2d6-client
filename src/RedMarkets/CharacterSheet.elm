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

module RedMarkets.CharacterSheet
    exposing
    ( Model
    , Msg
    , editView
    , view
    , compactView
    , update
    )

import RedMarkets.CharacterSheet.Types as Types
import RedMarkets.CharacterSheet.View as View
import RedMarkets.CharacterSheet.Update as Update

type alias Msg =
    Types.Msg

type alias Model =
    Types.Model

editView =
    View.editView

view =
    View.view

compactView =
    View.compactView

update =
    Update.update

