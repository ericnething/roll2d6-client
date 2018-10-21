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
import Lobby
import PouchDB
import PouchDB.Encode exposing (encodeGame)
import PouchDB.Decode exposing (decodeGame, decodeGameList)
import Debouncer.Messages as Debouncer exposing
    ( Debouncer
    , provideInput
    , debounce
    , toDebouncer
    )
import Time

main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ PouchDB.getResponse
              (LobbyMsg << Lobby.DecodeGameResponse)
        , PouchDB.getGameListResponse
              (LobbyMsg << Lobby.DecodeGameListResponse)
        ]

-- Model

type alias Model =
    { game : Maybe Game.Model
    , gameList : List Lobby.GameMetadata
    , debouncer : Debouncer Msg
    }

initialModel : Model
initialModel =
    { game =
          Nothing
          -- Just Game.initialModel
    , gameList = []
    , debouncer =
        debounce (1 * Time.second)
            |> toDebouncer
    }

init : (Model, Cmd Msg)
init = (initialModel
       , Task.perform identity
           (Task.succeed
                (LobbyMsg Lobby.GetGameList)))

-- Update

type Msg
    = GameMsg Game.ConsumerMsg
    | LobbyMsg Lobby.ConsumerMsg
    | WriteToPouchDB Game.Model
    | DebounceMsg (Debouncer.Msg Msg)

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
            (model, PouchDB.put (encodeGame game))

        GameMsg submsg ->
            case submsg of
                Game.ExitToLobby ->
                    ({ model | game = Nothing }
                    , Task.perform identity
                        (Task.succeed
                             (LobbyMsg Lobby.GetGameList)))

                Game.LocalMsg msg ->
                    case model.game of
                        Nothing ->
                            (model, Cmd.none)
                        Just game ->
                            let
                                (newGame, cmd) =
                                    Game.update msg game
                            in
                                ({ model | game = Just newGame }
                                , Cmd.batch
                                    [ Task.perform identity
                                          (Task.succeed
                                               (WriteToPouchDB
                                                    newGame
                                               |> provideInput
                                               |> DebounceMsg))
                                    , Cmd.map GameMsg cmd
                                    ])

        LobbyMsg submsg ->
            case submsg of
                Lobby.EnterGame id ->
                    (model
                    , PouchDB.get id)

                Lobby.SetActiveGame game ->
                    ({ model | game = Just game }
                    , Cmd.none)

                Lobby.DecodeGameResponse value ->
                    case decodeGame value of
                        Ok game ->
                            (model
                            , Task.perform
                                (LobbyMsg << Lobby.SetActiveGame)
                                (Task.succeed game))
                        Err err ->
                            let _ = Debug.log
                                    "DecodeGameResponse"
                                    err
                            in
                                (model, Cmd.none)
                
                Lobby.CreateGame id ->
                    let
                        newGame = Game.emptyModel id
                        metadata =
                            Lobby.GameMetadata id newGame.title
                    in
                        ({ model
                             | gameList
                               = metadata :: model.gameList
                         }
                        , Task.perform
                            WriteToPouchDB
                            (Task.succeed newGame))

                Lobby.GetGameList ->
                    (model
                    , PouchDB.allDocs ())

                Lobby.SetGameList gameList ->
                    ({ model | gameList = gameList }
                    , Cmd.none)

                Lobby.DecodeGameListResponse value ->
                    case decodeGameList value of
                        Ok gameList ->
                            (model
                            , Task.perform
                                (LobbyMsg << Lobby.SetGameList)
                                (Task.succeed
                                     (List.reverse gameList)))
                        Err err ->
                            let _ = Debug.log
                                    "DecodeGameListResponse"
                                    err
                            in
                                (model, Cmd.none)


-- View

view : Model -> Html Msg
view model =
    case model.game of
        Nothing ->
            Lobby.view { games = model.gameList }
                |> Html.Styled.map LobbyMsg

        Just game ->
            Game.view game
                |> Html.Styled.map GameMsg

