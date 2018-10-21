module Lobby exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Task
import Game
import Json.Encode


type alias Model =
    { games : List GameMetadata
    }

type alias GameMetadata =
    { id : String
    , title : String
    }

type ConsumerMsg
    = EnterGame String
    | SetActiveGame Game.Model
    | DecodeGameResponse Json.Encode.Value
    | CreateGame String
    | GetGameList
    | DecodeGameListResponse Json.Encode.Value
    | SetGameList (List GameMetadata)

view : Model -> Html ConsumerMsg
view model =
    div [ css
          [ Css.property "display" "grid"
          , Css.property "grid-template-rows" "2.2rem auto"
          , Css.property "grid-row-gap" "0.8rem"
          , backgroundColor (hex "36393f")
          , Css.minHeight (vh 100)
          ]
        ]
    [ topNavigation
    , div [ css
            [ color (hex "fff")
            , padding (Css.em 1)
            ]
          ]
        [ h1 [] [ text "My Games" ]
        , div [] (model.games
                 |> List.reverse
                 |> List.map gamePreview)
        , button
              [ onClick
                    (CreateGame
                         (toString
                              (List.length model.games)))
              ]
              [ text "Create new game" ]
        ]
    ]

gamePreview : GameMetadata -> Html ConsumerMsg
gamePreview { id, title } =
    div []
        [ span [] [ text title ]
        , button
              [ onClick (EnterGame id)
              ]
              [ text "Join Game" ]
        ]

topNavigation : Html msg
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
