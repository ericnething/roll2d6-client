port module PouchDB exposing (..)

import Json.Decode
import Json.Encode exposing (Value)


port loadGame : ( Value, String ) -> Cmd msg
port gameLoaded : (Value -> msg) -> Sub msg
port gameLoadFailed : (Value -> msg) -> Sub msg
port authFailed : (Value -> msg) -> Sub msg
port changesReceived : (Value -> msg) -> Sub msg
port put : ( PouchDBRef, Value ) -> Cmd msg
port get : PouchDBRef -> Cmd msg
port getResponse : (Value -> msg) -> Sub msg
-- port allDocs : PouchDBRef -> Cmd msg
port getGameListResponse : (Value -> msg) -> Sub msg
port sse_playerListUpdated : (Value -> msg) -> Sub msg
-- port sse_playerAdded : (Value -> msg) -> Sub msg
-- port sse_playerRemoved : (Value -> msg) -> Sub msg

type alias PouchDBRef =
    Value
