module Fate.GameAspectSheet.View
    exposing
    ( editView
    , view
    )

import Array exposing (Array)
import Fate.GameAspectSheet.Types exposing (..)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)


view : Model -> Html Msg
view model =
    div
        [ css
            [ padding3 (px 0) (Css.em 1) (Css.em 1)
            , overflowWrap breakWord
            ]
        ]
    (Array.toList (Array.indexedMap sceneView model.scenes))


sceneView : Index -> Scene -> Html Msg
sceneView index { title, aspects } =
    div []
    [ div [] [ text title ]
    ]

editSceneView : Index -> Scene -> Html Msg
editSceneView index { title, aspects } =
    div []
    [ input
          [ type_ "text"
          , onInput (UpdateSceneTitle index)
          , value title
          ]
          []
    ]

editView : Model -> Html Msg
editView model =
    div
        [ css
            [ padding3 (px 0) (Css.em 1) (Css.em 1)
            , overflowWrap breakWord
            ]
        ]
    (Array.toList (Array.indexedMap editSceneView model.scenes))

