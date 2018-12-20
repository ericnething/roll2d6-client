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

module Game.Sheets.Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Game.GameType exposing (GameType)
import Game.Sheet.Types exposing (SheetMsg, SheetModel)
import Game.Person exposing (..)
import Browser.Dom as Dom
import Http
import Json.Decode

type alias Index = Int

type FullSheet = FullSheet SheetId Bool

type alias SheetId = String

type alias GameId = String

type alias Model r =
    { r |
      sheets : Dict SheetId SheetModel
    , fullSheet : Maybe FullSheet
    , sheetsViewportX : Float
    , gameType : GameType
    , myPlayerInfo : Person
    , id : GameId
    , sheetsOrdering : Array SheetId
    , movingSheet : MovingSheet
    }

type MovingSheet
    = NotMoving
    | MovingSheet SheetId (Maybe SheetId)

type VisualShift
    = NoShift
    | ShiftLeft (Array SheetId)
    | ShiftRight (Array SheetId)

type Msg
    = SheetMsg SheetId SheetMsg
    | GenerateNewSheetId SheetModel
    | NewSheetId SheetModel (Result Http.Error SheetId)
    | AddSheet SheetId SheetModel
    | RemoveSheet SheetId
    | SheetRemoved (Result Http.Error SheetId)
    | OnScroll Int
    | OpenFullSheet SheetId Bool
    | CloseFullSheet
    | ToggleFullSheetEdit
    | RestoreScrollX (Result Dom.Error ())
    | UpdateSheetsOrdering (Array SheetId)
    | DragStart SheetId Json.Decode.Value
    | DragEnd
    | DragEnter SheetId
    | DragOver SheetId
    | Drop SheetId
