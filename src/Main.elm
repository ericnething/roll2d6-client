module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
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
import Html
import Html.Styled exposing (..)
import Json.Decode
import Lobby
import Lobby.Types as Lobby
import Login
import Login.Types as Login
import PouchDB
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


main : Program () Model Msg
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
        [ PouchDB.gameLoaded GameLoaded
        , PouchDB.gameLoadFailed (always GameLoadFailed)
        , PouchDB.authFailed (always AuthFailed)
        , case model.screen of
            GameScreen game ->
                Sub.map GameMsg (Game.subscriptions game)

            _ ->
                Sub.none
        ]



-- Model


type alias Model =
    { screen : Screen
    , debouncer : Debouncer Msg
    , navkey : Navigation.Key
    }


type Screen
    = LoginScreen Login.Model
    | LobbyScreen Lobby.Model
    | LoadingScreen
    | GameScreen Game.Model
    | InviteScreen Invite.Model


initialModel : Navigation.Key -> Model
initialModel key =
    { screen = LoginScreen Login.initialModel
    , debouncer =
        debounce (fromSeconds 1)
            |> toDebouncer
    , navkey = key
    }


init : flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url) (initialModel key)



-- Update


type Msg
    = NavigateToUrl UrlRequest
    | UrlChanged Url
    | RouteChanged (Maybe Route)
    | GameMsg Game.Msg
    | LobbyMsg Lobby.Msg
    | LoginMsg Login.ConsumerMsg
    | WriteToPouchDB Game.Model
    | DebounceMsg (Debouncer.Msg Msg)
    | GameLoaded Json.Decode.Value
    | GameLoadFailed
    | AuthFailed
    | InviteMsg Invite.Msg


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
                    , Navigation.pushUrl
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

        WriteToPouchDB game ->
            ( model, PouchDB.put ( game.ref, encodeGame game ) )

        GameLoaded value ->
            case decodeGame value of
                Ok newGame ->
                    case model.screen of
                        LoadingScreen ->
                            ( { model
                                | screen = GameScreen newGame
                              }
                            , Cmd.batch
                                [ API.getPlayers newGame.id
                                    |> Cmd.map GameMsg
                                , API.setPresenceOnline newGame.id
                                    |> Cmd.map GameMsg
                                , API.getChatLog newGame.id
                                    |> Cmd.map GameMsg
                                ]
                            )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    (model, Cmd.none)

        GameLoadFailed ->
            ( model
            , Navigation.replaceUrl
                model.navkey
                (Route.toUrlString Route.Lobby)
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
            ( { model | screen = LoadingScreen }
            , PouchDB.loadGame
                ( encodeGameData (Game.emptyGameData Game.Fate)
                , gameId )
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
        Game.SheetMsg _ _ ->
            debouncedWriteToPouchDB
                newGame

        Game.AddSheet _ ->
            debouncedWriteToPouchDB
                newGame

        Game.RemoveSheet _ ->
            debouncedWriteToPouchDB
                newGame

        Game.UpdateGameTitle _ ->
            debouncedWriteToPouchDB
                newGame

        Game.OpenOverlay _ ->
            Cmd.none

        Game.CloseOverlay ->
            Cmd.none

        Game.UpdateCurrentGame _ ->
            Cmd.none

        Game.ChangesReceived ->
            Cmd.none

        Game.ExitToLobby ->
            Cmd.none

        Game.CreateInvite ->
            Cmd.none

        Game.InviteCreated _ ->
            Cmd.none

        Game.PlayerList _ _ ->
            Cmd.none

        Game.ServerEventReceived _ ->
            Cmd.none

        Game.Ping ->
            Cmd.none

        Game.Pong ->
            Cmd.none

        Game.NoOp ->
            Cmd.none

        _ ->
            Cmd.none

debouncedWriteToPouchDB : Game.Model -> Cmd Msg
debouncedWriteToPouchDB newGame =
    Task.perform identity
        (Task.succeed
            (WriteToPouchDB newGame
                |> provideInput
                |> DebounceMsg
            )
        )



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

        LoadingScreen ->
            div [] [ text "Loading game..." ]

        GameScreen game ->
            Game.view game
                |> Html.Styled.map GameMsg

        InviteScreen invite ->
            Invite.view invite
                |> Html.Styled.map InviteMsg
