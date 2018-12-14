-- Roll2d6 Virtual Tabletop Project
--
-- Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>
--
-- This program is free software: you can redistribute it
-- and/or modify it under the terms of the GNU Affero
-- General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR A
-- PARTICULAR PURPOSE.  See the GNU Affero General Public
-- License for more details.
--
-- You should have received a copy of the GNU Affero General
-- Public License along with this program. If not, see
-- <https://www.gnu.org/licenses/>.

module Login.Types exposing (init, Auth, ConsumerMsg(..), Model, Msg(..), Registration, Tab(..), encodeAuth, encodeRegistration, initialModel)

import Http
import Json.Decode
import Json.Encode exposing (Value, object, string)


type alias Model =
    { tab : Tab
    , username : String
    , email : String
    , password : String
    }


init : (Model, Cmd ConsumerMsg)
init =
    (initialModel, Cmd.none)

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
    = LocalMsg Msg


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
