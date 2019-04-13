{-
Roll2d6 Virtual Tabletop Project

Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program. If not, see
<https://www.gnu.org/licenses/>.
-}

module Main.Types exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Url exposing (Url)
import Json.Decode
import Login.Types as Login
import Chat.Types exposing (Person)
import Route exposing (Route)
import Http
import App.Types as App
import Invite

type alias Flags =
    { windowSize : (Int, Int)
    , xmppClient : Json.Decode.Value
    }

type alias Model =
    { screen : Screen
    , navkey : Navigation.Key
    , viewportSize : (Int, Int)
    , xmppClient : Json.Decode.Value
    }

type Screen
    = LoginScreen Login.Model
    | AppScreen App.Model
    | LoadingScreen LoadingProgress
    | InviteScreen Invite.Model

type alias LoadingProgress =
    { me : Maybe Person
    }

type alias LoadingProgressComplete =
    { me : Person
    }

emptyLoadingProgress : LoadingProgress
emptyLoadingProgress =
    { me = Nothing
    }

type Msg
    = NavigateToUrl UrlRequest
    | UrlChanged Url
    | RouteChanged (Maybe Route)
    | LoginMsg Login.Msg
    | AppMsg App.Msg
    | InviteMsg Invite.Msg
    | WindowResized Int Int
    | AppLoaded LoadingProgressComplete
    | MyPersonLoaded Json.Decode.Value
