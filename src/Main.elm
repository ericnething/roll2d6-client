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

module Main exposing (main)

import Main.Types exposing (..)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Browser.Events
import Url exposing (Url)
import Array exposing (Array)
import Debouncer.Messages as Debouncer
    exposing
        ( Debouncer
        , debounce
        , provideInput
        , toDebouncer
        , fromSeconds
        )
import Game
import Game.Types as Game exposing (GameId)
import Game.GameType as Game
import Game.Sheets.Types as Sheets
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
import Game.Encode exposing (encodeGame, encodeGameData)
import Task
import Util exposing (removeIndexFromArray)
import Route exposing (Route)
import Http
import API
import Invite


main : Program (Int, Int) Model Msg
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
        , case model.screen of
            GameScreen game ->
                Sub.map GameMsg (Game.subscriptions game)

            _ ->
                Sub.none
        , Browser.Events.onResize WindowResized
        ]


initialModel : (Int, Int) -> Navigation.Key -> Model
initialModel viewportSize key =
    { screen = LoginScreen Login.initialModel
    , debouncer =
        debounce (fromSeconds 1)
            |> toDebouncer
    , navkey = key
    , viewportSize = viewportSize
    }


init : (Int, Int) -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init viewportSize url key =
    changeRouteTo
    (Route.fromUrl url)
    (initialModel viewportSize key)


updateDebouncer : Debouncer.UpdateConfig Msg Model
updateDebouncer =
    { mapMsg = DebounceMsg
    , getDebouncer = .debouncer
    , setDebouncer =
        \debouncer model ->
            { model | debouncer = debouncer }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- let _ = Debug.log "DEBUG: " msg
    -- in
      case msg of
        NavigateToUrl urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Navigation.replaceUrl
                        model.navkey
                        (Url.toString url)
                    )
                    
                External url ->
                    ( model
                    , Navigation.load url
                    )

        UrlChanged url ->
            changeRouteTo (Route.fromUrl url) model

        RouteChanged route ->
            changeRouteTo route model

        DebounceMsg submsg ->
            Debouncer.update update updateDebouncer submsg model

        WriteToPouchDB ref game ->
            ( model, Ports.put ( ref, encodeGameData game ) )

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
                            ( model, Cmd.none )

                Err err ->
                    -- let
                    --     _ = Debug.log "GameLoad Error:" err
                    -- in
                    (model, Cmd.none)

        GameLoadFailed ->
            ( model
            , Navigation.replaceUrl
                model.navkey
                (Route.toUrlString Route.Lobby)
            )

        PlayerInfoLoaded result ->
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
                            ( model, Cmd.none )

                Err (Http.BadStatus err) ->
                    case err.status.code of
                        401 ->
                            ( model
                            , Navigation.replaceUrl
                                model.navkey
                                (Route.toUrlString Route.Auth)
                            )

                        _ ->
                            -- let
                            --     _ = Debug.log "GameLoad Error:" err
                            -- in
                                (model, Cmd.none)

                Err err ->
                    -- let
                    --     _ = Debug.log "GameLoad Error:" err
                    -- in
                    (model, Cmd.none)

        LoadGameScreen { toGameModel, myPlayerInfo } ->
            let
                game = toGameModel myPlayerInfo
            in
                ({ model
                     | screen = GameScreen game
                 }
                , Cmd.batch
                    [ API.getPlayers game.id
                          |> Cmd.map GameMsg
                    , API.setPresenceOnline game.id
                          |> Cmd.map GameMsg
                    , API.getChatLog game.id
                          |> Cmd.map GameMsg
                    ]
                )

        AuthFailed ->
            ( model
            , Navigation.replaceUrl
                model.navkey
                (Route.toUrlString Route.Auth)
            )

        GameMsg localmsg ->
            case model.screen of
                GameScreen game ->
                    let
                        ( newGame, cmd ) =
                            Game.update
                                model.navkey
                                localmsg
                                game
                    in
                        ( { model
                              | screen = GameScreen newGame
                          }
                        , Cmd.batch
                            [ maybeWriteToPouchDB
                                  localmsg
                                  newGame
                            , Cmd.map GameMsg cmd
                            ]
                        )

                _ ->
                    ( model, Cmd.none )

        LobbyMsg localmsg ->
            case model.screen of
                LobbyScreen lobby ->
                    let
                        ( newLobby, cmd ) =
                            Lobby.update
                                model.navkey
                                localmsg
                                lobby
                    in
                        ( { model
                              | screen = LobbyScreen newLobby
                          }
                        , Cmd.map LobbyMsg cmd
                        )

                _ ->
                    ( model, Cmd.none )

        LoginMsg submsg ->
            case submsg of
                Login.LocalMsg localmsg ->
                    case model.screen of
                        LoginScreen login ->
                            let
                                ( newLogin, cmd ) =
                                    Login.update
                                        model.navkey
                                        localmsg
                                        login
                            in
                            ( { model
                                | screen = LoginScreen newLogin
                              }
                            , Cmd.map
                                LoginMsg
                                cmd
                            )

                        _ ->
                            ( model, Cmd.none )

        InviteMsg localmsg ->
            case model.screen of
                InviteScreen invite ->
                    let
                        ( newInvite, cmd ) =
                            Invite.update
                                model.navkey
                                localmsg
                                invite
                    in
                        ( { model
                              | screen = InviteScreen newInvite
                          }
                        , Cmd.map InviteMsg cmd
                        )

                _ ->
                    ( model, Cmd.none )

        WindowResized width height ->
            ({ model | viewportSize = (width, height) }, Cmd.none)


changeRouteTo : Maybe Route -> Model -> (Model, Cmd Msg)
changeRouteTo route model =
    case route of
        Just Route.Auth ->
            let
                (login, cmd) = Login.init
            in
                ({ model
                     | screen = LoginScreen login
                 }
                , Cmd.map LoginMsg cmd
                )
                
        Just Route.Lobby ->
            let
                (lobby, cmd) = Lobby.init
            in
                ({ model
                     | screen = LobbyScreen lobby
                 }
                , Cmd.map LobbyMsg cmd
                )
                
        Just (Route.Game gameId) ->
            ( { model | screen = LoadingScreen emptyLoadingProgress }
            , Cmd.batch
                [ Ports.loadGame
                      ( encodeGameData (Game.emptyGameData Game.Fate)
                      , gameId )
                , API.getMyPlayerInfo gameId
                ]
            )

        Just (Route.Invite inviteId) ->
            ( { model | screen = InviteScreen Invite.initialModel }
            , API.joinGame inviteId
                  |> Cmd.map InviteMsg
            )
            
        Nothing ->
            (model, Cmd.none)


maybeWriteToPouchDB : Game.Msg -> Game.Model -> Cmd Msg
maybeWriteToPouchDB msg newGame =
    case msg of
        Game.SheetsMsg (Sheets.SheetMsg _ _) ->
            debouncedWriteToPouchDB newGame

        Game.SheetsMsg (Sheets.AddSheet _ _) ->
            debouncedWriteToPouchDB newGame

        Game.SheetsMsg (Sheets.SheetRemoved _) ->
            debouncedWriteToPouchDB newGame

        Game.SheetsMsg (Sheets.UpdateSheetsOrdering _) ->
            debouncedWriteToPouchDB newGame

        Game.SheetsMsg (Sheets.UpdateSheetPermissions _ _) ->
            debouncedWriteToPouchDB newGame

        Game.PlayerRemoved _ _ _ ->
            debouncedWriteToPouchDB newGame

        Game.UpdateGameTitle _ ->
            debouncedWriteToPouchDB newGame

        _ ->
            Cmd.none

debouncedWriteToPouchDB : Game.Model -> Cmd Msg
debouncedWriteToPouchDB { ref
                        , title
                        , gameType
                        , sheets
                        , sheetsOrdering
                        , sheetPermissions
                        } =
    Task.perform identity
        (Task.succeed
            (WriteToPouchDB ref
                 { title = title
                 , gameType = gameType
                 , sheets = sheets
                 , sheetsOrdering = sheetsOrdering
                 , sheetPermissions = sheetPermissions
                 }
                |> provideInput
                |> DebounceMsg
            )
        )


loadGameScreenIfDone : LoadingProgress -> Cmd Msg
loadGameScreenIfDone { myPlayerInfo, toGameModel } =
    case (myPlayerInfo, toGameModel) of
        (Just pi, Just gm) ->
            Task.perform 
            LoadGameScreen
            (Task.succeed
                 { myPlayerInfo = pi
                 , toGameModel = gm
                 })
        _ ->
            Cmd.none
              
-- View


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
            Game.view model.viewportSize game
                |> Html.Styled.map GameMsg

        InviteScreen invite ->
            Invite.view invite
                |> Html.Styled.map InviteMsg
