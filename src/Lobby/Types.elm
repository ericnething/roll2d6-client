module Lobby.Types exposing (..)

import Game.Types as Game
import RemoteData exposing (WebData, RemoteData(..))

type alias Model =
    { games : WebData (List GameMetadata)
    }

initialModel : Model
initialModel =
    { games = NotAsked }

type alias GameMetadata =
    { id : Game.GameId
    , title : String
    }

type ConsumerMsg
    = LoadGame Game.GameId
    | LocalMsg Msg

type Msg
    = NewGame
    | GetGameList
    | SetGameList (WebData (List GameMetadata))
