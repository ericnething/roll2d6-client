port module PouchDB exposing (..)

import Json.Encode exposing (Value)

port put : Value -> Cmd msg
port get : String -> Cmd msg
port getResponse : (Value -> msg) -> Sub msg
port allDocs : () -> Cmd msg
port getGameListResponse : (Value -> msg) -> Sub msg
