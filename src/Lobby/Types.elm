module Lobby.Types exposing (ConsumerMsg(..), GameMetadata, Model, Msg(..), initialModel)

import Game.Types as Game
import RemoteData exposing (RemoteData(..), WebData)
import Http


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
    = LocalMsg Msg


type Msg
    = NewGame
    | GetGameList
    | SetGameList (WebData (List GameMetadata))
    | LoadGame Game.GameId
    | Logout
    | LogoutResponse (Result Http.Error String)
