module Game.Sheet.Types exposing (..)

import Fate
import WorldOfDungeons

type SheetMsg
    = FateMsg Fate.Msg
    | WorldOfDungeonsMsg WorldOfDungeons.Msg

type SheetModel
    = FateSheet Fate.Sheet
    | WorldOfDungeonsSheet WorldOfDungeons.Sheet

