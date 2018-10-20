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

main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- Model

type alias Model =
    { game : Maybe Game.Model
    , gamesdb : Array Game.Model
    }

initialModel : Model
initialModel =
    { game = Just Game.initialModel
    , gamesdb =
        Array.fromList
            [ Game.initialModel ]
    }

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

-- Update

type Msg
    = GameMsg Game.ConsumerMsg
    | LobbyMsg Lobby.ConsumerMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GameMsg submsg ->
            case submsg of
                Game.ExitToLobby ->
                    ({ model
                         | game = Nothing
                         , gamesdb
                           = Array.map
                             (replaceGame model.game)
                             model.gamesdb
                                      
                     }
                    , Cmd.none)

                Game.LocalMsg msg ->
                    case model.game of
                        Nothing ->
                            (model, Cmd.none)
                        Just game ->
                            let
                                (newModel, cmd) =
                                    Game.update msg game
                            in
                                ({ model | game = Just newModel }
                                , Cmd.map GameMsg cmd)

        LobbyMsg submsg ->
            case submsg of
                Lobby.EnterGame index ->
                    handleEnterGame model index

                Lobby.CreateGame ->
                    ({ model
                         | gamesdb
                           = Array.push
                           (Game.emptyModel
                                (Array.length model.gamesdb))
                             model.gamesdb
                     }
                    , Cmd.none)


replaceGame : Maybe Game.Model -> Game.Model -> Game.Model
replaceGame mCurrentGame game =
    case mCurrentGame of
        Nothing ->
            game
        Just currentGame ->
            if currentGame.id == game.id
            then currentGame
            else game

handleEnterGame : Model -> Int -> (Model, Cmd msg)
handleEnterGame model index =
    ({ model
         | game
           = Array.get index model.gamesdb
     }
    , Cmd.none)

-- View

view : Model -> Html Msg
view model =
    case model.game of
        Nothing ->
            Lobby.view { games = model.gamesdb }
                |> Html.Styled.map LobbyMsg
                     

        Just game ->
            Game.view game
                |> Html.Styled.map GameMsg

