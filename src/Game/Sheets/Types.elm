module Game.Sheets.Types exposing (..)

import Array exposing (Array)
import Game.GameType exposing (GameType)
import Game.Sheet.Types exposing (SheetMsg, SheetModel)
import Browser.Dom as Dom

type alias Index = Int

type FullSheet = FullSheet Index Bool

type alias Model r =
    { r |
      sheets : Array SheetModel
    , fullSheet : Maybe FullSheet
    , sheetsViewportX : Float
    , gameType : GameType
    }

type Msg
    = SheetMsg Index SheetMsg
    | AddSheet SheetModel
    | RemoveSheet Index
    | OnScroll Int
    | OpenFullSheet Index
    | CloseFullSheet
    | ToggleFullSheetEdit
    | RestoreScrollX (Result Dom.Error ())

