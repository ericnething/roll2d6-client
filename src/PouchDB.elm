port module PouchDB
    exposing
    ( PouchDBRef
    , changesReceived
    , gameLoaded
    , get
    , getGameListResponse
    , getResponse
    , loadGame
    , put
    )

import Json.Decode
import Json.Encode exposing (Value)


port loadGame : ( Value, String ) -> Cmd msg


port gameLoaded : (Value -> msg) -> Sub msg


port changesReceived : (Value -> msg) -> Sub msg


port put : ( PouchDBRef, Value ) -> Cmd msg


port get : PouchDBRef -> Cmd msg


port getResponse : (Value -> msg) -> Sub msg


-- port allDocs : PouchDBRef -> Cmd msg


port getGameListResponse : (Value -> msg) -> Sub msg


type alias PouchDBRef =
    Value
