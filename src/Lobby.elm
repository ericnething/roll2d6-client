module Lobby
    exposing
    ( init
    , update
    , view
    )

import API
import Css exposing (..)
import Game.Types as Game
import Game.GameType as Game
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
import Game.Sheet as Sheet

initialGameModel : String -> Game.GameType -> Game.GameData
initialGameModel title gameType =
    { title = title
    , gameType = gameType
    , sheets = Sheet.initialModel gameType
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Task.perform
        identity
        (Task.succeed GetGameList)
    )

update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update navkey msg model =
    case msg of
        NewGame ->
            case model.overlay of
                NewGameSettings { title, gameType } ->
                    ( model
                    , API.newGame
                        (initialGameModel title gameType)
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        NewGameResponse result ->
            case result of
                Success gameId ->
                    ({ model | overlay = OverlayNone }
                    , Navigation.replaceUrl
                        navkey
                        (Route.toUrlString (Route.Game gameId))
                    )
                _ ->
                    ( model, Cmd.none )

        UpdateNewGameTitle title ->
            case model.overlay of
                NewGameSettings settings ->
                    ({ model
                         | overlay =
                             NewGameSettings
                             { settings | title = title }
                     }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateNewGameType gameType ->
            case model.overlay of
                NewGameSettings settings ->
                    ({ model
                         | overlay =
                             NewGameSettings
                             { settings | gameType = gameType }
                     }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

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
                            , Navigation.replaceUrl
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
            , Navigation.replaceUrl
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

        OpenOverlay overlay_ ->
            ( { model | overlay = overlay_ }, Cmd.none )

        CloseOverlay ->
            ( { model | overlay = OverlayNone }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-rows" "3rem auto"
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
                    , div [ css
                            [ margin2 (Css.em 1) (Css.em 0) ]
                          ]
                        (games
                            |> List.reverse
                            |> List.map gamePreview
                        )
                    , defaultButton
                        [ type_ "button"
                        , onClick
                              (OpenOverlay
                                   emptyNewGameSettings)
                        ]
                        [ text "Create new game" ]
                    ]
        , overlayView model
        ]


defaultButton =
    styled button
        [ whiteSpace noWrap
        , padding2 (Css.em 0.1) (Css.em 0.5)
        , backgroundColor (hex "fff")
        , border3 (px 1) solid (hex "ccc")
        , borderRadius (px 4)
        , cursor pointer
        , hover
            [ backgroundColor (hex "eee") ]
        ]


gameButton =
    styled button
        [ whiteSpace noWrap
        , padding2 (Css.em 1) (Css.em 1)
        , backgroundColor (hex "aaa")
        , border3 (px 1) solid (hex "ccc")
        , borderRadius (px 4)
        , cursor pointer
        , hover
            [ backgroundColor (hex "eee") ]
        ]

gamePreview : GameMetadata -> Html Msg
gamePreview { id, title } =
    div []
        [ span
              [ css
                [ marginRight (Css.em 1) ]
              ]
              [ text title ]
        , defaultButton
            [ onClick (LoadGame id)
            ]
            [ text "Join Game" ]
        ]


topNavigation : Html Msg
topNavigation =
    header
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent spaceBetween
            , backgroundColor (rgba 0 0 0 0.15)
            , Css.height (Css.rem 3)
            , color (hex "fff")
            , padding2 (px 0) (Css.em 1)
            , position sticky
            , top (px 0)
            ]
        ]
        [ h1 [] [ text "Fate RPG" ]
        , defaultButton
              [ type_ "button"
              , onClick Logout
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


--------------------------------------------------
-- Overlay
--------------------------------------------------

overlay =
    styled div
        [ position fixed
        , top (px 0)
        , left (px 0)
        , Css.height (vh 100)
        , Css.width (vw 100)
        , Css.property "pointer-events" "all"
        , backgroundColor (rgba 0 0 0 0.5)
        , overflowY scroll
        ]

overlayView : Model -> Html Msg
overlayView model =
    case model.overlay of
        OverlayNone ->
            text ""
                
        NewGameSettings newGameSettings ->
            overlay [] [ newGameView newGameSettings ]


newGameView : NewGameSettingsModel -> Html Msg
newGameView { title, gameType } =
    div [ css
          [ margin2 (Css.em 4) auto
          , backgroundColor (hex "fff")
          , padding (Css.em 2)
          , Css.width (Css.em 32)
          , borderRadius (Css.em 0.2)
          ]
        ]
    [ sectionLabel "Game Title"
    , input
          [ type_ "text"
          , css [ inputStyles ]
          , onInput UpdateNewGameTitle
          , value title
          ]
          []
    , sectionLabel "Game Type"
    , gameTypeOptionListView gameType
    , defaultButton
          [ onClick NewGame ]
          [ text "Create Game" ]
    , defaultButton
          [ onClick CloseOverlay
          , css
                [ backgroundColor (hex "ff0000")
                , color (hex "fff")
                , hover
                      [ backgroundColor (hex "ee0000") ]
                ]
          ]
          [ text "Cancel" ]
    ]

gameTypeOptionListView : Game.GameType -> Html Msg
gameTypeOptionListView gameType =
    div []
        [ fieldset []
              (List.map
                   (gameTypeOptionView gameType)
                   Game.gameTypeOptions)
        ]

gameTypeOptionView : Game.GameType -> Game.GameType -> Html Msg
gameTypeOptionView gameType option =
    label [ css [ padding (px 20) ]
          ]
    [ input
          [ type_ "radio"
          , name "game-type-option"
          , onInput (always (UpdateNewGameType option))
          , HA.checked (gameType == option)
          ] []
    , text (Game.showGameType option)
    ]

emptyNewGameSettings : Overlay
emptyNewGameSettings =
    NewGameSettings
    { title = ""
    , gameType = Game.Fate
    }


inputStyles : Css.Style
inputStyles =
    batch
        [ Css.width (pct 100)
        , border3 (px 1) solid (hex "888")
        , borderRadius (px 4)
        , padding (Css.em 0.25)
        , flex (int 1)
        ]

sectionLabel : String -> Html msg
sectionLabel title =
    div
        [ css
            [ fontSize (Css.em 1)
            , color (hex "555")
            , Css.property "font-variant" "small-caps"
            , Css.property "letter-spacing" "0.1em"
            , fontWeight bold
            ]
        ]
        [ text title ]


