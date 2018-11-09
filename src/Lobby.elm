module Lobby exposing (gamePreview, init, topNavigation, topToolbar, update, view)

import API
import Css exposing (..)
import Game.Types as Game
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Http
import Json.Encode exposing (Value)
import Lobby.Types exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Browser.Navigation as Navigation
import Route

init : ( Model, Cmd ConsumerMsg )
init =
    ( initialModel
    , Task.perform
        LocalMsg
        (Task.succeed GetGameList)
    )

update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update navkey msg model =
    case msg of
        NewGame ->
            ( model
            , API.newGame "New Game Title"
            )

        GetGameList ->
            ( model
            , API.getAllGames
            )

        SetGameList result ->
            case result of
                Failure (Http.BadStatus resp) ->
                    case resp.status.code of
                        401 ->
                            ( model
                            , Navigation.pushUrl
                                navkey
                                (Route.toUrlString Route.Auth)
                            )
                        _ ->
                            ( { model | games = result }
                            , Cmd.none
                            )
                _ ->
                    ( { model | games = result }
                    , Cmd.none
                    )

        LoadGame id ->
            (model
            , Navigation.pushUrl
                navkey
                (Route.toUrlString (Route.Game id))
            )

        Logout ->
            ( model
            , API.logout
            )

        LogoutResponse result ->
            case result of
                Ok _ ->
                    ( model
                    , Navigation.replaceUrl
                        navkey
                        (Route.toUrlString Route.Auth)
                    )
                Err status ->
                    (model, Cmd.none)


view : Model -> Html ConsumerMsg
view model =
    div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-rows" "2.2rem auto"
            , Css.property "grid-row-gap" "0.8rem"
            , backgroundColor (hex "36393f")
            , Css.minHeight (vh 100)
            ]
        ]
        [ topNavigation
        , div
            [ css
                [ color (hex "fff")
                , padding (Css.em 1)
                ]
            ]
          <|
            case model.games of
                NotAsked ->
                    [ text "Not Asked" ]

                Loading ->
                    [ text "Loading" ]

                Failure err ->
                    [ text "Request Failed" ]

                Success games ->
                    [ h1 [] [ text "My Games" ]
                    , div []
                        (games
                            |> List.reverse
                            |> List.map gamePreview
                        )
                    , button
                        [ type_ "button"
                        , onClick (LocalMsg NewGame)
                        ]
                        [ text "Create new game" ]
                    ]
        ]


gamePreview : GameMetadata -> Html ConsumerMsg
gamePreview { id, title } =
    div []
        [ span [] [ text title ]
        , button
            [ onClick (LocalMsg <| LoadGame id)
            ]
            [ text "Join Game" ]
        ]


topNavigation : Html ConsumerMsg
topNavigation =
    header
        [ css
            [ displayFlex
            , alignItems center
            , backgroundColor (rgba 0 0 0 0.15)
            , Css.height (Css.rem 3)
            , color (hex "fff")
            , padding2 (px 0) (Css.em 1)
            , position sticky
            , top (px 0)
            ]
        ]
        [ h1 [] [ text "Fate RPG" ]
        , button
              [ type_ "button"
              , onClick (LocalMsg Logout)
              ]
              [ text "Log out" ]
        ]


topToolbar : Html msg
topToolbar =
    div
        [ css
            [ displayFlex
            , alignItems center
            , backgroundColor (hex "0079bf")
            , Css.height (Css.rem 3)
            , color (hex "fff")
            , padding2 (px 0) (Css.em 1)
            ]
        ]
        [ text "Toolbar" ]
