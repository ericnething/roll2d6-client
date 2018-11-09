module Login.Types exposing (Auth, ConsumerMsg(..), Model, Msg(..), Registration, Tab(..), encodeAuth, encodeRegistration, initialModel)

import Http
import Json.Decode
import Json.Encode exposing (Value, object, string)


type alias Model =
    { tab : Tab
    , username : String
    , email : String
    , password : String
    }


initialModel : Model
initialModel =
    { tab = LoginTab
    , username = ""
    , email = ""
    , password = ""
    }


type Tab
    = LoginTab
    | RegisterTab


type ConsumerMsg
    = LoadLobby
    | LocalMsg Msg


type Msg
    = Login
    | LoginResponse (Result Http.Error Int)
    | Register
    | RegisterResponse (Result Http.Error Int)
    | UpdateEmail String
    | UpdatePassword String
    | UpdateUsername String
    | ChangeTab Tab


type alias Auth =
    { email : String
    , password : String
    }


type alias Registration =
    { username : String
    , email : String
    , password : String
    }


encodeAuth : Auth -> Value
encodeAuth auth =
    object
        [ ( "email", string auth.email )
        , ( "password", string auth.password )
        ]


encodeRegistration : Registration -> Value
encodeRegistration reg =
    object
        [ ( "username", string reg.username )
        , ( "email", string reg.email )
        , ( "password", string reg.password )
        ]
