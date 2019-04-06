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
import Game.Encode
    exposing
    ( encodeGame
    , encodeGameData
    , encodeSheet
    )
import Chat
import Chat.Types as Chat
import Chat.Decode as Chat
import Task
import Util exposing (removeIndexFromArray)
import Route exposing (Route)
import Http
import API
import Invite
-- import Game.Person


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
        -- , Sub.map ChatMsg
        --     (Ports.xmpp_received
        --          (Chat.StanzaReceived << Chat.decodeStanza))
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
    -- , chat = Chat.newModel "test" { id = ""
    --                               , accessLevel = Game.Person.Owner
    --                               , username = ""
    --                               }
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
    let _ = Debug.log "DEBUG: " msg
    in
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

        WriteGameToPouchDB ref docId game ->
            ( model
            , Ports.put
                ( ref
                , docId
                , encodeGameData game
                )
            )

        WriteSheetToPouchDB ref sheetId gameType sheet ->
            ( model
            , Ports.put
                ( ref
                , sheetId
                , encodeSheet sheet
                )
            )

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
                    let _ = Debug.log "Game Load Failed" err
                    in
                    ( model
                    , Task.perform
                        identity
                        (Task.succeed GameLoadFailed)
                    )

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
                            ( model, Cmd.none )

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
                            ( model, Cmd.none )

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
                , Chat.connectClient { jid = myPlayerInfo.id
                                     , password = "foobar"
                                     , room = game.id
                                     , username = myPlayerInfo.username
                                     }
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

        -- ChatMsg submsg ->
        --     let
        --         (submodel, cmd) = Chat.update submsg model.chat
        --     in
        --         ({ model | chat = submodel }
        --         , Cmd.map ChatMsg cmd)

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


maybeWriteToPouchDB : Game.Msg -> Game.Model -> Cmd Msg
maybeWriteToPouchDB msg newGame =
    case msg of
        Game.SheetsMsg (Sheets.SheetMsg sheetId _) ->
            debouncedWriteSheetToPouchDB
                sheetId
                { ref = newGame.ref
                , msheet = Dict.get sheetId newGame.sheets
                , gameType = newGame.gameType
                }

        -- Writing to pouchDB for adding and removing sheets is
        -- handled in Sheets.elm directly using ports

        Game.SheetsMsg Sheets.SheetsOrderingUpdated ->
            writeGameToPouchDB newGame

        Game.SheetsMsg Sheets.SheetPermissionsUpdated ->
            writeGameToPouchDB newGame

        Game.PlayerRemovedSuccess ->
            writeGameToPouchDB newGame

        Game.UpdateGameTitle _ ->
            debouncedWriteGameToPouchDB newGame

        _ ->
            Cmd.none

debouncedWriteSheetToPouchDB : Sheets.SheetId
                             -> { ref : PouchDBRef
                                , msheet : Maybe Sheet.SheetModel
                                , gameType : Game.GameType
                                }
                             -> Cmd Msg
debouncedWriteSheetToPouchDB sheetId { ref, msheet, gameType } =
    case msheet of
        Nothing ->
            Cmd.none

        Just sheet ->
            Task.perform identity
                (Task.succeed
                     (WriteSheetToPouchDB
                          ref
                          sheetId
                          gameType
                          sheet
                     |> provideInput
                     |> DebounceMsg
                     )
                )


debouncedWriteGameToPouchDB : Game.Model -> Cmd Msg
debouncedWriteGameToPouchDB { ref
                            , title
                            , gameType
                            , sheetsOrdering
                            , sheetPermissions
                            } =
    Task.perform identity
        (Task.succeed
            (WriteGameToPouchDB ref "game"
                 { title = title
                 , gameType = gameType
                 , sheetsOrdering = sheetsOrdering
                 , sheetPermissions = sheetPermissions
                 }
                |> provideInput
                |> DebounceMsg
            )
        )

writeGameToPouchDB : Game.Model -> Cmd Msg
writeGameToPouchDB { ref
                   , title
                   , gameType
                   , sheetsOrdering
                   , sheetPermissions
                   } =
    Task.perform identity
        (Task.succeed
            (WriteGameToPouchDB ref "game"
                 { title = title
                 , gameType = gameType
                 , sheetsOrdering = sheetsOrdering
                 , sheetPermissions = sheetPermissions
                 }
            )
        )

loadGameScreenIfDone : LoadingProgress -> Cmd Msg
loadGameScreenIfDone { toGameModel, myPlayerInfo, players } =
    case (toGameModel, myPlayerInfo, players) of
        (Just toGameModel_, Just myPlayerInfo_, Just players_) ->
            Task.perform 
            LoadGameScreen
            (Task.succeed
                 { toGameModel = toGameModel_
                 , myPlayerInfo = myPlayerInfo_
                 , players = players_
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
