module Lobby.Types exposing (..)

import Game.Types as Game
import RemoteData exposing (RemoteData(..), WebData)
import Http


type alias Model =
    { games : WebData (List GameMetadata)
    , overlay : Overlay
    }


initialModel : Model
initialModel =
    { games = NotAsked
    , overlay = OverlayNone
    }


type alias GameMetadata =
    { id : Game.GameId
    , title : String
    }

type Overlay
    = OverlayNone
    | NewGameSettings NewGameSettingsModel

type alias NewGameSettingsModel =
    { title : String
    , gameType : Game.GameType
    }

type Msg
    = NewGame
    | NewGameResponse (WebData Game.GameId)
    | UpdateNewGameTitle String
    | UpdateNewGameType Game.GameType
    | GetGameList
    | SetGameList (WebData (List GameMetadata))
    | LoadGame Game.GameId
    | Logout
    | LogoutResponse (Result Http.Error String)
    | OpenOverlay Overlay
    | CloseOverlay
