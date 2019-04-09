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

import Main.Types exposing (..)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Browser.Events
import Url exposing (Url)
import Array exposing (Array)
import Dict exposing (Dict)
import Game
import Game.Types as Game exposing (GameId)
import Game.GameType as Game
import Game.Sheets.Types as Sheets
import Game.Sheet.Types as Sheet
import Html
import Html.Styled exposing (..)
import Html.Styled.Lazy exposing (lazy)
import Json.Decode
import Lobby
import Lobby.Types as Lobby
import Login
import Login.Types as Login
import Ports exposing (PouchDBRef)
import Game.Decode
    exposing
        ( decodeGame
        , decodeGameData
        , decodeGameList
        )
import Chat
import Chat.Types as Chat
import Task
import Route exposing (Route)
import Http
import API
import Invite
import Game.Person
import Util exposing (toCmd)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view >> toUnstyled >>
                 \html -> { title = "Fate RPG"
                          , body = [ html ]
                          }
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = NavigateToUrl
        , onUrlChange = UrlChanged
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.gameLoaded GameLoaded
        , Ports.gameLoadFailed (always GameLoadFailed)
        , Ports.authFailed (always AuthFailed)

        -- We must subscribe to XMPP Stanzas here to guarantee that
        -- they will arrive when needed, otherwise a race condition
        -- occurs where this subscription may get overriden. This is
        -- caused by a bug in the compiler.
        , Sub.map ChatMsg Chat.subscriptions

        -- We will do the same for all other subscriptions to avoid
        -- any issues in the future. Registering all subscriptions on
        -- initial page load will only be a problem if we need the
        -- model for a specific part of the application to initialize
        -- a subscription.
        , Sub.map GameMsg Game.subscriptions

        , Browser.Events.onResize WindowResized
        ]


initialModel : Flags -> Navigation.Key -> Model
initialModel { windowSize, credentials } key =
    { screen = LoginScreen Login.initialModel
    , navkey = key
    , viewportSize = (windowSize.width, windowSize.height)
    , chat =
        Chat.newModel
            "test"
            { id = Maybe.withDefault "" credentials.jid
            , accessLevel = Game.Person.Owner
            , username = Maybe.withDefault "" credentials.username
            }
    }


init : Flags -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags url key =
    case (flags.credentials.jid, flags.credentials.password) of
        (Just jid, Just password) ->
            let (model, cmd) =
                    changeRouteTo
                    (Route.fromUrl url)
                    (initialModel flags key)
            in
                ( model
                , Cmd.batch
                    [ Chat.connectClient
                          { jid = jid
                          , password = password
                          }
                    , cmd
                    ]
                )
        _ ->
            changeRouteTo
            (Just Route.Auth)
            (initialModel flags key)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let _ = Debug.log "DEBUG: " msg in
    case msg of
        NavigateToUrl urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Navigation.replaceUrl model.navkey (Url.toString url))
                    
                External url ->
                    ( model
                    , Navigation.load url)

        UrlChanged url ->
            changeRouteTo (Route.fromUrl url) model

        RouteChanged route ->
            changeRouteTo route model


        GameLoaded value ->
            case decodeGame value of
                Ok toGameModel ->
                    case model.screen of
                        LoadingScreen progress ->
                            let
                                updatedProgress =
                                    { progress
                                        | toGameModel = Just toGameModel
                                    }
                            in
                                ({ model
                                     | screen =
                                         LoadingScreen updatedProgress
                                 }
                                , loadGameScreenIfDone updatedProgress
                                )

                        _ ->
                            (model, Cmd.none)

                Err err ->
                    let _ = Debug.log "Game Load Failed" err in
                    ( model
                    , toCmd GameLoadFailed)

        GameLoadFailed ->
            ( model
            , Navigation.replaceUrl
                model.navkey
                (Route.toUrlString Route.Lobby)
            )

        MyPlayerInfoLoaded result ->
            case result of
                Ok playerInfo ->
                    case model.screen of
                        LoadingScreen progress ->
                            let
                                updatedProgress =
                                    { progress
                                        | myPlayerInfo = Just playerInfo
                                    }
                            in
                                ({ model
                                     | screen =
                                       LoadingScreen updatedProgress
                                 }
                                , loadGameScreenIfDone updatedProgress
                                )

                        _ ->
                            (model, Cmd.none)

                Err err ->
                    ( model
                    , Navigation.replaceUrl
                        model.navkey
                            (Route.toUrlString Route.Lobby)
                    )

        PlayerListLoaded result ->
            case result of
                Ok players ->
                    case model.screen of
                        LoadingScreen progress ->
                            let
                                updatedProgress =
                                    { progress
                                        | players = Just players
                                    }
                            in
                                ({ model
                                     | screen =
                                       LoadingScreen updatedProgress
                                 }
                                , loadGameScreenIfDone updatedProgress
                                )

                        _ ->
                            (model, Cmd.none)

                Err err ->
                    ( model
                    , Navigation.replaceUrl
                        model.navkey
                            (Route.toUrlString Route.Lobby)
                    )

        LoadGameScreen { toGameModel, myPlayerInfo, players } ->
            let
                game = toGameModel myPlayerInfo players
            in
                ({ model
                     | screen = GameScreen game
                 }
                , Chat.joinRoom
                    model.chat.ref
                    { room = game.id
                    , username = myPlayerInfo.username
                    }
                )

        AuthFailed ->
            ( model
            , Navigation.replaceUrl
                model.navkey
                (Route.toUrlString Route.Auth)
            )

        GameMsg (Game.ChatMsg chatmsg) ->
            (model, toCmd (ChatMsg chatmsg))

        GameMsg localmsg ->
            updateGameScreen localmsg model

        LobbyMsg localmsg ->
            updateLobbyScreen localmsg model

        LoginMsg localmsg ->
            updateLoginScreen localmsg model

        InviteMsg localmsg ->
            updateInviteScreen localmsg model

        ChatMsg localmsg ->
            let
                (newChat, cmd) = Chat.update localmsg model.chat
            in
                ({ model | chat = newChat }
                , Cmd.map ChatMsg cmd)

        WindowResized width height ->
            ({ model | viewportSize = (width, height) }, Cmd.none)


updateGameScreen : Game.Msg -> Model -> (Model, Cmd Msg)
updateGameScreen localmsg model =
    case model.screen of
        GameScreen game ->
            let
                (newGame, cmd) = Game.update model.navkey localmsg game
            in
                ({ model | screen = GameScreen newGame }
                , Cmd.batch
                    [ Cmd.map GameMsg
                          (Game.maybeWriteToPouchDB localmsg newGame)
                    , Cmd.map GameMsg cmd
                    ]
                )
                
        _ ->
            (model, Cmd.none)


updateLobbyScreen : Lobby.Msg -> Model -> (Model, Cmd Msg)
updateLobbyScreen localmsg model =
    case model.screen of
        LobbyScreen lobby ->
            let
                (newLobby, cmd) = Lobby.update model.navkey localmsg lobby
            in
                ({ model | screen = LobbyScreen newLobby }
                , Cmd.map LobbyMsg cmd)

        _ ->
            (model, Cmd.none)


updateLoginScreen : Login.Msg -> Model -> (Model, Cmd Msg)
updateLoginScreen localmsg model =
    case model.screen of
        LoginScreen login ->
            let
                (newLogin, cmd) = Login.update model.navkey localmsg login
            in
                ({ model | screen = LoginScreen newLogin }
                , Cmd.map LoginMsg cmd)

        _ ->
            (model, Cmd.none)


updateInviteScreen : Invite.Msg -> Model -> (Model, Cmd Msg)
updateInviteScreen localmsg model =
    case model.screen of
        InviteScreen invite ->
            let
                (newInvite, cmd) = Invite.update model.navkey localmsg invite
            in
                ({ model | screen = InviteScreen newInvite }
                , Cmd.map InviteMsg cmd)

        _ ->
            (model, Cmd.none)
                        
                        
changeRouteTo : Maybe Route -> Model -> (Model, Cmd Msg)
changeRouteTo route model =
    case route of
        Just Route.Auth ->
            let
                (login, cmd) = Login.init
            in
                ({ model | screen = LoginScreen login }
                , Cmd.map LoginMsg cmd)
                
        Just Route.Lobby ->
            let
                (lobby, cmd) = Lobby.init
            in
                ({ model | screen = LobbyScreen lobby }
                , Cmd.map LobbyMsg cmd)
                
        Just (Route.Game gameId) ->
            ( { model | screen = LoadingScreen emptyLoadingProgress }
            , Cmd.batch
                [ Ports.loadGame gameId
                , API.getMyPlayerInfo gameId
                , API.getPlayers gameId
                ]
            )

        Just (Route.Invite inviteId) ->
            ( { model | screen = InviteScreen Invite.initialModel }
            , API.joinGame inviteId
                  |> Cmd.map InviteMsg
            )
            
        Nothing ->
            (model, Cmd.none)



loadGameScreenIfDone : LoadingProgress -> Cmd Msg
loadGameScreenIfDone { toGameModel, myPlayerInfo, players } =
    case (toGameModel, myPlayerInfo, players) of
        (Just toGameModel_, Just myPlayerInfo_, Just players_) ->
            toCmd <|
                LoadGameScreen
                { toGameModel = toGameModel_
                , myPlayerInfo = myPlayerInfo_
                , players = players_
                }
        _ ->
            Cmd.none


view : Model -> Html Msg
view model =
    case model.screen of
        LoginScreen submodel ->
            Login.view submodel
                |> Html.Styled.map LoginMsg

        LobbyScreen submodel ->
            Lobby.view submodel
                |> Html.Styled.map LobbyMsg

        LoadingScreen _ ->
            div [] [ text "Loading game..." ]

        GameScreen game ->
            Game.view model.viewportSize model.chat game
                |> Html.Styled.map GameMsg

        InviteScreen invite ->
            Invite.view invite
                |> Html.Styled.map InviteMsg
