module Game.Sheet.Types exposing (..)

import Fate

type SheetMsg
    = FateMsg Fate.Msg

type SheetModel
    = FateSheet Fate.Sheet
