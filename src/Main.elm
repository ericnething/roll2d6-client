module Main exposing (Model, Msg(..), Screen(..), debouncedWriteToPouchDB, init, initialModel, loadLobby, main, subscriptions, update, updateDebouncer, view)

import Browser
import Array exposing (Array)
import Css exposing (..)
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
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode
import Lobby
import Lobby.Types as Lobby
import Login
import Login.Types as Login
import PouchDB
import PouchDB.Decode
    exposing
        ( decodeGame
        , decodeGameData
        , decodeGameList
        )
import PouchDB.Encode exposing (encodeGame, encodeGameData)
import Task
import Util exposing (removeIndexFromArray)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view >> toUnstyled >>
                 \html -> { title = "Fate RPG"
                          , body = [ html ]
                          }
        , update = update
        , subscriptions = subscriptions
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PouchDB.gameLoaded GameLoaded
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
    }


type Screen
    = LoginScreen Login.Model
    | LobbyScreen Lobby.Model
    | LoadingGameScreen GameId
    | GameScreen Game.Model


initialModel : Model
initialModel =
    { screen = LoginScreen Login.initialModel
    , debouncer =
        debounce (fromSeconds 1)
            |> toDebouncer
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Task.perform identity
        (Task.succeed
            (LobbyMsg (Lobby.LocalMsg Lobby.GetGameList))
        )
    )



-- Update


type Msg
    = GameMsg Game.ConsumerMsg
    | LobbyMsg Lobby.ConsumerMsg
    | LoginMsg Login.ConsumerMsg
    | WriteToPouchDB Game.Model
    | DebounceMsg (Debouncer.Msg Msg)
    | GameLoaded Json.Decode.Value


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
    case msg of
        DebounceMsg submsg ->
            Debouncer.update update updateDebouncer submsg model

        WriteToPouchDB game ->
            ( model, PouchDB.put ( game.ref, encodeGame game ) )

        GameLoaded value ->
            case decodeGame value of
                Ok newGame ->
                    case model.screen of
                        LoadingGameScreen id ->
                            ( { model
                                | screen = GameScreen newGame
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        GameMsg submsg ->
            case submsg of
                Game.ExitToLobby ->
                    ( { model
                        | screen =
                            LobbyScreen
                                Lobby.initialModel
                      }
                    , Task.perform identity
                        (Task.succeed
                            (LobbyMsg
                                (Lobby.LocalMsg
                                    Lobby.GetGameList
                                )
                            )
                        )
                    )

                Game.LocalMsg localmsg ->
                    case model.screen of
                        GameScreen game ->
                            let
                                ( newGame, cmd ) =
                                    Game.update localmsg game
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

        LobbyMsg submsg ->
            case submsg of
                Lobby.LoadGame id ->
                    ( { model | screen = LoadingGameScreen id }
                    , PouchDB.loadGame
                        ( encodeGameData Game.emptyGameData, id )
                    )

                Lobby.LocalMsg localmsg ->
                    case model.screen of
                        LobbyScreen lobby ->
                            let
                                ( newLobby, cmd ) =
                                    Lobby.update localmsg lobby
                            in
                            ( { model
                                | screen = LobbyScreen newLobby
                              }
                            , Cmd.map
                                (LobbyMsg << Lobby.LocalMsg)
                                cmd
                            )

                        _ ->
                            ( model, Cmd.none )

        LoginMsg submsg ->
            case submsg of
                Login.LoadLobby ->
                    loadLobby model

                Login.LocalMsg localmsg ->
                    case model.screen of
                        LoginScreen login ->
                            let
                                ( newLogin, cmd ) =
                                    Login.update localmsg login
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


loadLobby : Model -> ( Model, Cmd Msg )
loadLobby model =
    let
        ( lobby, cmd ) =
            Lobby.init
    in
    ( { model | screen = LobbyScreen lobby }
    , Cmd.map LobbyMsg cmd
    )


maybeWriteToPouchDB : Game.Msg -> Game.Model -> Cmd Msg
maybeWriteToPouchDB msg newGame =
    case msg of
        Game.CharacterSheetMsg _ _ ->
            debouncedWriteToPouchDB
                newGame
        Game.AddCharacterSheet ->
            debouncedWriteToPouchDB
                newGame
        Game.RemoveCharacterSheet _ ->
            debouncedWriteToPouchDB
                newGame
        Game.UpdateGameTitle _ ->
            debouncedWriteToPouchDB
                newGame
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

        LoadingGameScreen _ ->
            div [] [ text "Loading game..." ]

        GameScreen game ->
            Game.view game
                |> Html.Styled.map GameMsg
