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

module Lobby
    exposing
    ( init
    , update
    , view
    )

import Array exposing (Array)
import Dict exposing (Dict)
import API
import Css exposing (..)
import Util.Css exposing (..)
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
    , sheets = Dict.empty --Sheet.initialModel gameType
    , sheetsOrdering = Array.fromList []
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
                    if String.length title > 0
                    then
                        ( model
                        , API.newGame
                            (initialGameModel title gameType)
                        )
                    else
                        ( model, Cmd.none )

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
          , padding2 (Css.em 2) (Css.em 1)
          , Css.width (Css.em 32)
          , borderRadius (Css.em 0.2)
          ]
        ]
    [ div [ css
            [ fontWeight bold
            , fontSize (Css.em 1.2)
            ]
          ]
          [ text "Create a new game" ]
    , sectionLabel "Select your game type"
    , radioInputList
          [ css
            [ Css.property "column-count" "2"
            ]
          ]
          { toMsg = UpdateNewGameType
          , options = Game.gameTypeOptions
          , selected = gameType
          , showOption = Game.showGameType
          }
    , sectionLabel "Enter your game title"
    , input
          [ type_ "text"
          , css [ inputStyles ]
          , onInput UpdateNewGameTitle
          , value title
          ]
          []
    , newGameViewButtons
    ]

newGameViewButtons : Html Msg
newGameViewButtons =
    div [ css
          [ margin3 (Css.em 2) (px 0) (px 0)
          ]
        ]
        [ defaultButton
              [ onClick NewGame
              , css [ marginRight (Css.em 1) ]
              ]
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
            , margin3 (Css.em 1) (px 0) (Css.em 0.65)
            ]
        ]
        [ text title ]


radioInputList : List (Attribute msg)
               -> { toMsg : a -> msg
                 , options : List a
                 , selected : a
                 , showOption : a -> String
                 }
               -> Html msg
radioInputList attrs { toMsg, options, selected, showOption } =
    let
        optionView option =
            label [ css
                    [ display block
                    , marginBottom (Css.em 0.25)
                    , fontSize (Css.em 1)
                    , padding2 (Css.em 0.1) (Css.em 0.6)
                    , borderRadius (Css.em 0.25)
                    , userSelect_none
                    , cursor pointer
                    , if (selected == option) then
                          batch
                          [ backgroundColor (hex "333")
                          , color (hex "fff")
                          ]
                      else
                          batch
                          [ hover
                            [ backgroundColor (hex "eee")
                            ]
                          ]
                    ]
                  ]
            [ input
                  [ type_ "radio"
                  , name "option"
                  , onInput (always (toMsg option))
                  , HA.checked (selected == option)
                  , css
                        [ position absolute
                        , appearance_none
                        , opacity (int 0)
                        , Css.height (px 0)
                        , Css.width (px 0)
                        ]
                  ] []
            , text (showOption option)
            ]
    in
        div attrs (List.map optionView options)
