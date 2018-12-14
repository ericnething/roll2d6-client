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

module Common exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)


defaultButton =
    styled button
        [ whiteSpace noWrap
        , padding2 (Css.em 0.1) (Css.em 0.5)
        , backgroundColor (hex "fff")
        , border3 (px 1) solid (hex "ccc")
        , borderRadius (px 4)
        , cursor pointer
        , hover
            [ backgroundColor (hex "eee") ]
        ]

inputStyles =
    batch
        [ Css.width (pct 100)
        , border3 (px 1) solid (hex "888")
        , borderRadius (px 4)
        , padding (Css.em 0.25)
        , flex (int 1)
        ]
