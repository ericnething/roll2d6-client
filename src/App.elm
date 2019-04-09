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

module App
    exposing
    ( view
    , update
    , subscriptions
    )

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
        ]


initialModel : Model
initialModel key =
    { screen = LobbyScreen Lobby.initialModel
    , chat =
        Chat.newModel
            "test"
            { id = "welkin@localhost"
            , accessLevel = Game.Person.Owner
            , username = "welkin"
            }
    }


init : (Model, Cmd Msg)
init =
    ( model
    , Cmd.batch
        [ Chat.connectClient
              { jid = "welkin@localhost"
              , password = "foobar"
              }
        ]
    )

update : Navigation.Key -> Msg -> Model -> (Model, Cmd Msg)
update navkey msg model =
    case msg of
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
                    (model, toCmd GameLoadFailed)

        GameLoadFailed ->
            ( model
            , Navigation.replaceUrl
                navkey
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
                        navkey
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
                        navkey
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
                navkey
                (Route.toUrlString Route.Auth)
            )

        GameMsg (Game.ChatMsg chatmsg) ->
            (model, toCmd (ChatMsg chatmsg))

        GameMsg localmsg ->
            updateGameScreen localmsg model

        LobbyMsg localmsg ->
            updateLobbyScreen localmsg model

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
                (newGame, cmd) = Game.update navkey localmsg game
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
                (newLobby, cmd) = Lobby.update navkey localmsg lobby
            in
                ({ model | screen = LobbyScreen newLobby }
                , Cmd.map LobbyMsg cmd)

        _ ->
            (model, Cmd.none)


updateInviteScreen : Invite.Msg -> Model -> (Model, Cmd Msg)
updateInviteScreen localmsg model =
    case model.screen of
        InviteScreen invite ->
            let
                (newInvite, cmd) = Invite.update navkey localmsg invite
            in
                ({ model | screen = InviteScreen newInvite }
                , Cmd.map InviteMsg cmd)

        _ ->
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
