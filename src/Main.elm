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


module Main exposing (main)

import API
import App
import App.Types as App
import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Navigation
import Chat.Decode as Chat
import Chat.Types as Chat
import Chat.XMPP as XMPP
import Dict exposing (Dict)
import Game.Types exposing (GameId)
import Html
import Html.Styled exposing (..)
import Html.Styled.Lazy exposing (lazy)
import Http
import Invite
import Json.Decode
import Json.Encode
import Login
import Login.Types as Login
import Main.Types exposing (..)
import Ports exposing (XMPPClient)
import Route exposing (Route)
import Task
import Url exposing (Url)
import Util exposing (toCmd)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view =
            view
                >> toUnstyled
                >> (\html ->
                        { title = "Roll2d6"
                        , body = [ html ]
                        }
                   )
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = NavigateToUrl
        , onUrlChange = UrlChanged
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map AppMsg App.subscriptions
        , Browser.Events.onResize WindowResized
        ]


initialModel : Flags -> Navigation.Key -> Model
initialModel { windowSize, xmppClient } key =
    { screen = LoginScreen Login.initialModel
    , navkey = key
    , viewportSize = windowSize
    , xmppClient = xmppClient
    }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    changeRouteTo
        (Route.fromUrl url)
        (initialModel flags key)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "DEBUG: " msg
    in
    case msg of
        NavigateToUrl urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Navigation.replaceUrl model.navkey (Url.toString url)
                    )

                External url ->
                    ( model
                    , Navigation.load url
                    )

        UrlChanged url ->
            changeRouteTo (Route.fromUrl url) model

        RouteChanged route ->
            changeRouteTo route model

        AppMsg localmsg ->
            updateAppScreen model.xmppClient localmsg model

        LoginMsg localmsg ->
            updateLoginScreen localmsg model

        InviteMsg localmsg ->
            updateInviteScreen localmsg model

        WindowResized width height ->
            ( { model | viewportSize = ( width, height ) }, Cmd.none )

        AppLoaded { me } ->
            let
                ( app, cmd ) =
                    App.init me
            in
            ( { model | screen = AppScreen app }
            , Cmd.batch
                [ Cmd.map (AppMsg << App.LobbyMsg) API.getAllGames
                , Cmd.map AppMsg cmd
                ]
            )

        AppLoadedWithGame { gameId, me } ->
            let
                ( app, cmd ) =
                    App.init me

                ( newApp, gameCmd ) =
                    App.routeToGame app gameId
            in
            ( { model | screen = AppScreen newApp }
            , Cmd.batch
                [ Cmd.map AppMsg cmd
                , Cmd.map AppMsg gameCmd
                ]
            )

        MyPersonLoaded meJson ->
            updateLoadingProgress model
                (\progress ->
                    case Chat.decodePerson meJson of
                        Ok me ->
                            { progress | me = Just me }

                        Err e ->
                            Debug.log "Person loading failed" progress
                )


loadAppIfComplete : LoadingProgress -> Cmd Msg
loadAppIfComplete { me } =
    case me of
        Just myPerson ->
            toCmd <|
                AppLoaded
                    { me = myPerson
                    }

        _ ->
            Cmd.none


updateLoadingProgress : Model -> (LoadingProgress -> LoadingProgress) -> ( Model, Cmd Msg )
updateLoadingProgress model toProgress =
    case model.screen of
        LoadingScreen progress ->
            let
                newProgress =
                    toProgress progress
            in
            ( { model | screen = LoadingScreen newProgress }
            , loadAppIfComplete newProgress
            )

        _ ->
            ( model, Cmd.none )


updateLoginScreen : Login.Msg -> Model -> ( Model, Cmd Msg )
updateLoginScreen localmsg model =
    case model.screen of
        LoginScreen login ->
            let
                ( newLogin, cmd ) =
                    Login.update model.navkey localmsg login
            in
            ( { model | screen = LoginScreen newLogin }
            , Cmd.map LoginMsg cmd
            )

        _ ->
            ( model, Cmd.none )


updateInviteScreen : Invite.Msg -> Model -> ( Model, Cmd Msg )
updateInviteScreen localmsg model =
    case model.screen of
        InviteScreen invite ->
            let
                ( newInvite, cmd ) =
                    Invite.update model.navkey localmsg invite
            in
            ( { model | screen = InviteScreen newInvite }
            , Cmd.map InviteMsg cmd
            )

        _ ->
            ( model, Cmd.none )


updateAppScreen : XMPPClient -> App.Msg -> Model -> ( Model, Cmd Msg )
updateAppScreen xmppClient localmsg model =
    case model.screen of
        AppScreen app ->
            let
                ( newApp, cmd ) =
                    App.update xmppClient model.navkey localmsg app
            in
            ( { model | screen = AppScreen newApp }
            , Cmd.map AppMsg cmd
            )

        _ ->
            ( model, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        Just Route.Auth ->
            let
                ( login, cmd ) =
                    Login.init
            in
            ( { model | screen = LoginScreen login }
            , Cmd.map LoginMsg cmd
            )

        Just Route.Lobby ->
            case model.screen of
                AppScreen app ->
                    let
                        ( newApp, cmd ) =
                            App.routeToLobby app
                    in
                    ( { model | screen = AppScreen newApp }
                    , Cmd.map AppMsg cmd
                    )

                _ ->
                    initApp model

        Just (Route.Game gameId) ->
            case model.screen of
                AppScreen app ->
                    let
                        ( newApp, cmd ) =
                            App.routeToGame app gameId
                    in
                    ( { model | screen = AppScreen newApp }
                    , Cmd.map AppMsg cmd
                    )

                _ ->
                    initAppWithGame gameId model

        Nothing ->
            ( model, Cmd.none )


initApp : Model -> ( Model, Cmd Msg )
initApp model =
    ( { model | screen = LoadingScreen emptyLoadingProgress }
    , Cmd.batch
        [ toCmd <|
            AppLoaded
                { me =
                    { id = "welkin@localhost"
                    , displayName = "Welkin"
                    , presence = Chat.Online
                    }
                }
        , XMPP.connect model.xmppClient
        ]
    )


initAppWithGame : GameId -> Model -> ( Model, Cmd Msg )
initAppWithGame gameId model =
    ( { model | screen = LoadingScreen emptyLoadingProgress }
    , Cmd.batch
        [ toCmd <|
            AppLoadedWithGame
                { gameId = gameId
                , me =
                    { id = "welkin@localhost"
                    , displayName = "Welkin"
                    , presence = Chat.Online
                    }
                }
        , XMPP.connect model.xmppClient
        ]
    )


view : Model -> Html Msg
view model =
    case model.screen of
        LoginScreen login ->
            Login.view login
                |> Html.Styled.map LoginMsg

        LoadingScreen _ ->
            div [] [ text "Loading Application..." ]

        AppScreen app ->
            App.view model.viewportSize app
                |> Html.Styled.map AppMsg

        InviteScreen invite ->
            Invite.view invite
                |> Html.Styled.map InviteMsg
