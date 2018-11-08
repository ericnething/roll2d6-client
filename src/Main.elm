module Main exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Array exposing (Array)
import Task
import Util exposing (removeIndexFromArray)
import Game
import Game.Types as Game exposing (GameId)
import Lobby
import Lobby.Types as Lobby
import Login
import Login.Types as Login
import PouchDB
import PouchDB.Encode exposing (encodeGame, encodeGameData)
import PouchDB.Decode exposing
    ( decodeGame
    , decodeGameData
    , decodeGameList
    )
import Debouncer.Messages as Debouncer exposing
    ( Debouncer
    , provideInput
    , debounce
    , toDebouncer
    )
import Time
import Json.Decode

main =
    Html.program
        { init = init
        , view = view >> toUnstyled
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
        debounce (1 * Time.second)
            |> toDebouncer
    }

init : (Model, Cmd Msg)
init = (initialModel
       , Task.perform identity
           (Task.succeed
                (LobbyMsg (Lobby.LocalMsg Lobby.GetGameList))))

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DebounceMsg submsg ->
            Debouncer.update update updateDebouncer submsg model

        WriteToPouchDB game ->
            (model, PouchDB.put (game.ref, (encodeGame game)))

        GameLoaded value ->
            let _ = Debug.log "Game Loaded in Elm" value
            in
              case decodeGame value of
                Ok newGame ->
                    case model.screen of
                        LoadingGameScreen id ->
                            ({ model
                                 | screen = GameScreen newGame
                             }
                            , Cmd.none)
                        _ ->
                            (model, Cmd.none)

                Err err ->
                    (model, Cmd.none)

        GameMsg submsg ->
            case submsg of
                Game.ExitToLobby ->
                    ({ model
                         | screen =
                             LobbyScreen
                                 Lobby.initialModel
                     }
                    , Task.perform identity
                        (Task.succeed
                             (LobbyMsg
                                  (Lobby.LocalMsg
                                       Lobby.GetGameList))))

                Game.LocalMsg msg ->
                    case model.screen of
                        GameScreen game ->
                            let
                                (newGame, cmd) =
                                    Game.update msg game
                            in
                                ({ model
                                     | screen = GameScreen newGame
                                 }
                                , Cmd.batch
                                    [ debouncedWriteToPouchDB
                                          newGame
                                    , Cmd.map GameMsg cmd
                                    ])
                        _ -> (model, Cmd.none)

        LobbyMsg submsg ->
            case submsg of
                Lobby.LoadGame id ->
                    ({ model | screen = LoadingGameScreen id }
                    , PouchDB.loadGame
                        (encodeGameData Game.emptyGameData, id))
                
                Lobby.LocalMsg msg ->
                    case model.screen of
                        LobbyScreen lobby ->
                            let
                                (newLobby, cmd) =
                                    Lobby.update msg lobby
                            in
                                ({ model
                                     | screen = LobbyScreen newLobby
                                 }
                                , Cmd.map
                                    (LobbyMsg << Lobby.LocalMsg)
                                        cmd)
                        _ -> (model, Cmd.none)

        LoginMsg submsg ->
            case submsg of
                Login.LoadLobby ->
                    loadLobby model
                Login.LocalMsg msg ->
                    case model.screen of
                        LoginScreen login ->
                            let
                                (newLogin, cmd) =
                                    Login.update msg login
                            in
                                ({ model
                                     | screen = LoginScreen newLogin
                                 }
                                , Cmd.map
                                    (LoginMsg)
                                        cmd)
                        _ -> (model, Cmd.none)


loadLobby : Model -> (Model, Cmd Msg)
loadLobby model =
    let
        (lobby, cmd) = Lobby.init
    in
        ({ model | screen = LobbyScreen lobby }
        , Cmd.map LobbyMsg cmd)

debouncedWriteToPouchDB : Game.Model -> Cmd Msg
debouncedWriteToPouchDB newGame =
    Task.perform identity
    (Task.succeed
         (WriteToPouchDB newGame
         |> provideInput
         |> DebounceMsg))


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

