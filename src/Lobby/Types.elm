module Lobby.Types
    exposing
    ( GameMetadata
    , Model
    , Msg(..)
    , initialModel
    )

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


type Msg
    = NewGame
    | GetGameList
    | SetGameList (WebData (List GameMetadata))
    | LoadGame Game.GameId
    | Logout
    | LogoutResponse (Result Http.Error String)
