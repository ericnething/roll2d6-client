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
    ( init
    , view
    , update
    , subscriptions
    , routeToGame
    , routeToLobby
    )

import App.Types exposing (..)
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
import Ports exposing (PouchDBRef, XMPPClientRef)
import Game.Decode
    exposing
        ( decodeGame
        , decodeGameData
        , decodeGameList
        )
import Chat
import Chat.Types as Chat exposing (Person)
import Task
import Route exposing (Route)
import Http
import API
import Game.Player
import Util exposing (toCmd)
import RemoteData exposing (RemoteData(..))


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


initialModel : XMPPClientRef -> Person -> Model
initialModel ref me =
    { xmppClientRef = ref
    , games = NotAsked
    , overlay = Lobby.OverlayNone
    , me = me
    , activeGame = NoGame
    , rooms = Dict.empty
    , tab = LobbyTab
    }

init : XMPPClientRef -> Person -> (Model, Cmd Msg)
init ref me =
    ( initialModel ref me
    , Cmd.none
    )

update : Navigation.Key -> Msg -> Model -> (Model, Cmd Msg)
update navkey msg model =
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
                                ({ model
                                     | activeGame =
                                         LoadingGame id updatedProgress
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
                    case model.activeGame of
                        LoadingGame id progress ->
                            let
                                updatedProgress =
                                    { progress
                                        | myPlayerInfo = Just playerInfo
                                    }
                            in
                                ({ model
                                     | activeGame =
                                       LoadingGame id updatedProgress
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
                    case model.activeGame of
                        LoadingGame id progress ->
                            let
                                updatedProgress =
                                    { progress
                                        | players = Just players
                                    }
                            in
                                ({ model
                                     | activeGame =
                                       LoadingGame id updatedProgress
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
                     | activeGame = ActiveGame game
                 }
                , Chat.joinRoom
                    model.xmppClientRef
                    { room = game.id
                    , displayName = myPlayerInfo.displayName
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
            updateGame navkey localmsg model

        LobbyMsg localmsg ->
            let
                (newModel, cmd) = Lobby.update navkey localmsg model
            in
                (newModel, Cmd.map LobbyMsg cmd)

        ChatMsg localmsg ->
            let
                (newModel, cmd) = Chat.update localmsg model
            in
                (newModel, Cmd.map ChatMsg cmd)


updateGame : Navigation.Key -> Game.Msg -> Model -> (Model, Cmd Msg)
updateGame navkey localmsg model =
    case model.activeGame of
        ActiveGame game ->
            let
                (newGame, cmd) = Game.update navkey localmsg game
            in
                ({ model | activeGame = ActiveGame newGame }
                , Cmd.batch
                    [ Cmd.map GameMsg
                          (Game.maybeWriteToPouchDB localmsg newGame)
                    , Cmd.map GameMsg cmd
                    ]
                )

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


routeToGame : Model -> GameId -> (Model, Cmd Msg)
routeToGame model gameId =
    case model.activeGame of
        NoGame ->
            ({ model
                 | activeGame = LoadingGame gameId emptyLoadingProgress
                 , tab = GameTab
             }
            , Cmd.batch
                [ Ports.loadGame gameId
                , API.getMyPlayerInfo gameId
                , API.getPlayers gameId
                ]
            )

        LoadingGame id progress ->
            if
                gameId == id
            then
                (model, Cmd.none)
            else
                ({ model
                     | activeGame = LoadingGame id emptyLoadingProgress
                     , tab = GameTab
                 }
                , Cmd.batch
                    [ Ports.loadGame gameId
                    , API.getMyPlayerInfo gameId
                    , API.getPlayers gameId
                    ]
                )

        ActiveGame game ->
            if
                gameId == game.id
            then
                (model, Cmd.none)
            else
                ({ model
                     | activeGame = LoadingGame gameId emptyLoadingProgress
                     , tab = GameTab
                 }
                , Cmd.batch
                    [ Ports.loadGame gameId
                    , API.getMyPlayerInfo gameId
                    , API.getPlayers gameId
                    ]
                )


routeToLobby : Model -> (Model, Cmd Msg)
routeToLobby model =
    ({ model | tab = LobbyTab }
    , Cmd.map LobbyMsg Lobby.init)
        
        
view : (Int, Int) -> Model -> Html Msg
view viewportSize model =
    case model.tab of
        LobbyTab ->
            Lobby.view model
                |> Html.Styled.map LobbyMsg 

        GameTab ->
            case model.activeGame of
                LoadingGame _ _ ->
                    div [] [ text "Loading game..." ]

                ActiveGame game ->
                    Game.view viewportSize (Dict.get game.id model.rooms) game
                        |> Html.Styled.map GameMsg

                NoGame ->
                    div [] [ text "No game." ]

