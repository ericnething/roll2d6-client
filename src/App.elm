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


module App exposing
    ( init
    , routeToGame
    , routeToLobby
    , subscriptions
    , update
    , view
    )

import API
import App.Types exposing (..)
import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Navigation
import Chat
import Chat.Types as Chat exposing (Person)
import Dict exposing (Dict)
import Game
import Game.Decode
    exposing
        ( decodeGame
        , decodeGameData
        , decodeGameList
        )
import Game.GameType as Game
import Game.Player
import Game.Sheet.Types as Sheet
import Game.Sheets.Types as Sheets
import Game.Types as Game exposing (GameId)
import Html
import Html.Styled exposing (..)
import Html.Styled.Lazy exposing (lazy)
import Http
import Json.Decode
import Lobby
import Lobby.Types as Lobby
import Ports exposing (PouchDBRef, XMPPClient)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Task
import Url exposing (Url)
import Util exposing (toCmd)


subscriptions : Sub Msg
subscriptions =
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


initialModel : Person -> Model
initialModel me =
    { games = NotAsked
    , newGameForm = Lobby.NewGameFormNone
    , lobbyTab = Lobby.GamesTab
    , me = me
    , activeGame = NoGame
    , rooms = Dict.empty
    , showLobbyOrGame = ShowLobby
    }


init : Person -> ( Model, Cmd Msg )
init me =
    ( initialModel me
    , Cmd.none
    )


update : XMPPClient -> Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update xmppClient navkey msg model =
    case msg of
        GameLoaded value ->
            case decodeGame value of
                Ok toGameModel ->
                    case model.activeGame of
                        LoadingGame id progress ->
                            let
                                updatedProgress =
                                    { progress
                                        | toGameModel = Just toGameModel
                                    }
                            in
                            ( { model
                                | activeGame =
                                    LoadingGame id updatedProgress
                                , showLobbyOrGame = ShowGame
                              }
                            , loadGameScreenIfDone updatedProgress
                            )

                        _ ->
                            ( model, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "Game Load Failed" err
                    in
                    ( model, toCmd GameLoadFailed )

        GameLoadFailed ->
            ( model
            , Navigation.replaceUrl
                navkey
                (Route.toUrlString Route.Lobby)
            )

        MyPlayerInfoLoaded result ->
            case result of
                Ok playerInfo ->
                    case model.activeGame of
                        LoadingGame id progress ->
                            let
                                updatedProgress =
                                    { progress
                                        | myPlayerInfo = Just playerInfo
                                    }
                            in
                            ( { model
                                | activeGame =
                                    LoadingGame id updatedProgress
                              }
                            , loadGameScreenIfDone updatedProgress
                            )

                        _ ->
                            ( model, Cmd.none )

                Err err ->
                    ( model
                    , Navigation.replaceUrl
                        navkey
                        (Route.toUrlString Route.Lobby)
                    )

        PlayerListLoaded result ->
            case result of
                Ok players ->
                    case model.activeGame of
                        LoadingGame id progress ->
                            let
                                updatedProgress =
                                    { progress
                                        | players = Just players
                                    }
                            in
                            ( { model
                                | activeGame =
                                    LoadingGame id updatedProgress
                              }
                            , loadGameScreenIfDone updatedProgress
                            )

                        _ ->
                            ( model, Cmd.none )

                Err err ->
                    ( model
                    , Navigation.replaceUrl
                        navkey
                        (Route.toUrlString Route.Lobby)
                    )

        LoadGameScreen { toGameModel, myPlayerInfo, players } ->
            let
                game =
                    toGameModel myPlayerInfo players
            in
            ( { model
                | activeGame = ActiveGame game
                , rooms =
                    Dict.insert (game.id ++ "@muc.localhost")
                        { id = game.id ++ "@muc.localhost"
                        , input = ""
                        , messages = []
                        , roster = Dict.empty
                        }
                        model.rooms
              }
            , Cmd.map ChatMsg <| toCmd (Chat.JoinRoom game.id)
            )

        AuthFailed ->
            ( model
            , Navigation.replaceUrl
                navkey
                (Route.toUrlString Route.Auth)
            )

        GameMsg (Game.ChatMsg chatmsg) ->
            ( model, toCmd (ChatMsg chatmsg) )

        GameMsg Game.SwitchToLobby ->
            ( { model
                | showLobbyOrGame = ShowLobby
                , lobbyTab = Lobby.GamesTab
              }
            , Cmd.map LobbyMsg Lobby.init
            )

        GameMsg localmsg ->
            updateGame navkey localmsg model

        LobbyMsg Lobby.ResumeGame ->
            ( { model | showLobbyOrGame = ShowGame }, Cmd.none )

        LobbyMsg localmsg ->
            let
                ( newModel, cmd ) =
                    Lobby.update navkey localmsg model
            in
            ( newModel, Cmd.map LobbyMsg cmd )

        ChatMsg Chat.OpenOverlay ->
            ( model, toCmd (GameMsg Game.OpenOverlay) )

        ChatMsg localmsg ->
            let
                ( newModel, cmd ) =
                    Chat.update xmppClient localmsg model
            in
            ( newModel, Cmd.map ChatMsg cmd )


updateGame : Navigation.Key -> Game.Msg -> Model -> ( Model, Cmd Msg )
updateGame navkey localmsg model =
    case model.activeGame of
        ActiveGame game ->
            let
                ( newGame, cmd ) =
                    Game.update navkey localmsg game
            in
            ( { model | activeGame = ActiveGame newGame }
            , Cmd.batch
                [ Cmd.map GameMsg
                    (Game.maybeWriteToPouchDB localmsg newGame)
                , Cmd.map GameMsg cmd
                ]
            )

        _ ->
            ( model, Cmd.none )


loadGameScreenIfDone : LoadingProgress -> Cmd Msg
loadGameScreenIfDone { toGameModel, myPlayerInfo, players } =
    case ( toGameModel, myPlayerInfo, players ) of
        ( Just toGameModel_, Just myPlayerInfo_, Just players_ ) ->
            toCmd <|
                LoadGameScreen
                    { toGameModel = toGameModel_
                    , myPlayerInfo = myPlayerInfo_
                    , players = players_
                    }

        _ ->
            Cmd.none


routeToGame : Model -> GameId -> ( Model, Cmd Msg )
routeToGame model gameId =
    ( { model
        | activeGame = LoadingGame gameId emptyLoadingProgress
      }
    , Cmd.batch
        [ Ports.loadGame gameId
        , API.getMyPlayerInfo gameId
        , API.getPlayers gameId
        ]
    )


routeToLobby : Model -> ( Model, Cmd Msg )
routeToLobby model =
    ( { model | activeGame = NoGame }
    , Cmd.map LobbyMsg Lobby.init
    )


view : ( Int, Int ) -> Model -> Html Msg
view viewportSize model =
    case model.activeGame of
        LoadingGame _ _ ->
            div [] [ text "Loading game..." ]

        ActiveGame game ->
            case model.showLobbyOrGame of
                ShowLobby ->
                    Lobby.view True model
                        |> Html.Styled.map LobbyMsg

                ShowGame ->
                    Game.view
                        viewportSize
                        (Dict.get (game.id ++ "@muc.localhost") model.rooms)
                        game
                        |> Html.Styled.map GameMsg

        NoGame ->
            Lobby.view False model
                |> Html.Styled.map LobbyMsg
