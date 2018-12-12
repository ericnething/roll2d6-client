module Game.GameType exposing (..)

type GameType
    = Fate
    | WorldOfDungeons

gameTypeOptions : List GameType
gameTypeOptions =
    [ Fate
    , WorldOfDungeons
    ]

showGameType : GameType -> String
showGameType gameType =
    case gameType of
        Fate -> "Fate"
        WorldOfDungeons -> "World of Dungeons"
