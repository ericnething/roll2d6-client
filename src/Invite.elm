module Invite
    exposing
    ( initialModel
    , Msg(..)
    , Model
    , update
    , view
    )

import Game.Types as Game
import Route
import Browser.Navigation as Navigation
import Http
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)


type Model
    = Loading
    | Failed String

initialModel : Model
initialModel = Loading

type alias InviteId = String

type Msg
    = JoinGame InviteId (Result Http.Error Game.GameId)

update : Navigation.Key -> Msg -> Model -> (Model, Cmd Msg)
update navkey msg model =
    case msg of
        JoinGame inviteId eGameId ->
            case eGameId of
                Ok gameId ->
                    (model
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
