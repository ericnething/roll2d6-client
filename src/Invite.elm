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


module Invite exposing
    ( InviteId
    , Model
    , Msg(..)
    , initialModel
    , update
    , view
    )

import Browser.Navigation as Navigation
import Css exposing (..)
import Game.Types as Game
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Http
import Route


type Model
    = Loading
    | Failed String


initialModel : Model
initialModel =
    Loading


type alias InviteId =
    String


type Msg
    = JoinGame InviteId (Result Http.Error Game.GameId)


update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update navkey msg model =
    case msg of
        JoinGame inviteId eGameId ->
            case eGameId of
                Ok gameId ->
                    ( model
                    , Navigation.replaceUrl
                        navkey
                        (Route.toUrlString (Route.Game gameId))
                    )

                Err (Http.BadStatus err) ->
                    case err.status.code of
                        401 ->
                            ( Failed "Not Authorized"
                            , Navigation.replaceUrl
                                navkey
                                (Route.toUrlString Route.Auth)
                            )

                        404 ->
                            ( Failed "This invite is no longer valid."
                            , Cmd.none
                            )

                        _ ->
                            ( Failed "Something went wrong with your request."
                            , Cmd.none
                            )

                Err err ->
                    ( Failed "Something went wrong with your request."
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [] [ text "Finding your game." ]

        Failed message ->
            div [] [ text message ]
