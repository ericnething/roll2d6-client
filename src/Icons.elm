{-
Roll2d6 Virtual Tabletop Project

Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program. If not, see
<https://www.gnu.org/licenses/>.
-}

module Icons
    exposing
    ( addCharacterSheet
    , gameSettings
    , instantInvite
    , players
    , xCircle
    , diceDefs
    , dFatePlus
    , dFateBlank
    , dFateMinus
    )

import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Svg.Styled
    exposing
    ( Svg
    , svg
    , path
    , symbol
    , defs
    , use
    , circle
    , line
    )
import Svg.Styled.Attributes
    exposing
    ( viewBox
    , d
    , width
    , height
    , fill
    , display
    , visibility
    , xlinkHref
    , id
    , stroke
    , strokeWidth
    , strokeLinecap
    , strokeLinejoin
    , cx, cy, r
    , x1, x2, y1, y2
    , class
    )

import Css

--------------------------------------------------
-- Game Menu
--------------------------------------------------

addCharacterSheet : Html msg
addCharacterSheet =
    svg [ width "24"
        , height "24"
        , viewBox "0 0 24 24"
        , fill "#fff"
        , display "block"
        ]
    [ path
          [ d "M15 12h-2v-2c0-0.553-0.447-1-1-1s-1 0.447-1 1v2h-2c-0.553 0-1 0.447-1 1s0.447 1 1 1h2v2c0 0.553 0.447 1 1 1s1-0.447 1-1v-2h2c0.553 0 1-0.447 1-1s-0.447-1-1-1z" ] []
    , path
        [ d "M19.707 7.293l-4-4c-0.187-0.188-0.441-0.293-0.707-0.293h-8c-1.654 0-3 1.346-3 3v12c0 1.654 1.346 3 3 3h10c1.654 0 3-1.346 3-3v-10c0-0.266-0.105-0.52-0.293-0.707zM17.586 8h-1.086c-0.827 0-1.5-0.673-1.5-1.5v-1.086l2.586 2.586zM17 19h-10c-0.552 0-1-0.448-1-1v-12c0-0.552 0.448-1 1-1h7v1.5c0 1.379 1.121 2.5 2.5 2.5h1.5v9c0 0.552-0.448 1-1 1z" ] []
    ]

gameSettings : Html msg
gameSettings =
    svg [ width "24"
        , height "24"
        , viewBox "0 0 24 24"
        , fill "#fff"
        , display "block"
        ]
    [ path [ d "M13 5l0.855 3.42 3.389-0.971 1.501 2.6-2.535 2.449 2.535 2.451-1.5 2.6-3.39-0.971-0.855 3.422h-3l-0.855-3.422-3.39 0.971-1.501-2.6 2.535-2.451-2.534-2.449 1.5-2.6 3.39 0.971 0.855-3.42h3zM13 3h-3c-0.918 0-1.718 0.625-1.939 1.516l-0.354 1.412-1.4-0.4c-0.184-0.053-0.369-0.078-0.552-0.078-0.7 0-1.368 0.37-1.731 1l-1.5 2.6c-0.459 0.796-0.317 1.802 0.342 2.438l1.047 1.011-1.048 1.015c-0.66 0.637-0.802 1.643-0.343 2.438l1.502 2.6c0.363 0.631 1.031 1 1.731 1 0.183 0 0.368-0.025 0.552-0.076l1.399-0.401 0.354 1.415c0.222 0.885 1.022 1.51 1.94 1.51h3c0.918 0 1.718-0.625 1.939-1.516l0.354-1.414 1.399 0.4c0.184 0.053 0.369 0.077 0.552 0.077 0.7 0 1.368-0.37 1.731-1l1.5-2.6c0.459-0.796 0.317-1.8-0.342-2.438l-1.047-1.013 1.047-1.013c0.66-0.637 0.801-1.644 0.342-2.438l-1.5-2.6c-0.365-0.631-1.031-1-1.732-1-0.184 0-0.368 0.025-0.551 0.076l-1.4 0.401-0.354-1.413c-0.22-0.884-1.020-1.509-1.938-1.509z" ] []
    , path [ d "M11.5 10.5c1.104 0 2 0.895 2 2s-0.896 2-2 2-2-0.896-2-2c0-1.105 0.896-2 2-2zM11.5 9.5c-1.654 0-3 1.346-3 3s1.346 3 3 3 3-1.346 3-3-1.346-3-3-3z" ] []
    ]

instantInvite : Html msg
instantInvite =
    svg [ width "24"
        , height "24"
        , viewBox "0 0 24 24"
        , fill "#fff"
        , display "block"
        ]
    [ path [ d "M9 14c1.381 0 2.631-0.56 3.536-1.465 0.904-0.904 1.464-2.154 1.464-3.535s-0.56-2.631-1.464-3.535c-0.905-0.905-2.155-1.465-3.536-1.465s-2.631 0.56-3.536 1.465c-0.904 0.904-1.464 2.154-1.464 3.535s0.56 2.631 1.464 3.535c0.905 0.905 2.155 1.465 3.536 1.465z" ] []
    , path [ d "M9 21c3.518 0 6-1 6-2 0-2-2.354-4-6-4-3.75 0-6 2-6 4 0 1 2.25 2 6 2z" ] []
    , path [ d "M21 12h-2v-2c0-0.553-0.447-1-1-1s-1 0.447-1 1v2h-2c-0.553 0-1 0.447-1 1s0.447 1 1 1h2v2c0 0.553 0.447 1 1 1s1-0.447 1-1v-2h2c0.553 0 1-0.447 1-1s-0.447-1-1-1z" ] []
    ]

players : Html msg
players =
    svg [ width "24"
        , height "24"
        , viewBox "0 0 24 24"
        , fill "#fff"
        , display "block"
        ]
    [ path [ d "M17 9c0-1.381-0.56-2.631-1.464-3.535s-2.155-1.465-3.536-1.465-2.631 0.56-3.536 1.465c-0.904 0.904-1.464 2.154-1.464 3.535s0.56 2.631 1.464 3.535c0.905 0.905 2.155 1.465 3.536 1.465s2.631-0.56 3.536-1.465c0.904-0.904 1.464-2.154 1.464-3.535z"] []
    , path [ d "M6 19c0 1 2.25 2 6 2 3.518 0 6-1 6-2 0-2-2.354-4-6-4-3.75 0-6 2-6 4z"] []
    ]

xCircle : Html msg
xCircle =
    svg [ width "24"
        , height "24"
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-x-circle"
        , display "block"
        ]
    [ circle [ cx "12", cy "12", r "10" ] []
    , line [ x1"15", y1 "9", x2 "9", y2 "15" ] []
    , line [ x1 "9", y1 "9", x2 "15", y2 "15" ] []
    ]

--------------------------------------------------
-- Dice
--------------------------------------------------

diceDefs : Html msg
diceDefs =
    div [ css
          [ Css.position Css.absolute
          , Css.width (Css.px 0)
          , Css.height (Css.px 0)
          , Css.overflow Css.hidden
          ]
        ]
    [ svg []
          [ defs []
                [ sym_dFatePlus
                , sym_dFateBlank
                , sym_dFateMinus
                ]
          ]
    ]


dFatePlus : Html msg
dFatePlus =
    svg [ display "inline"
        , width "24"
        , height "28"
        ]
    [ use [ xlinkHref ("#" ++ id_dFatePlus) ] [] ]

id_dFatePlus = "icon-dice-fate-plus"

sym_dFatePlus : Svg msg
sym_dFatePlus =
    symbol [viewBox "0 0 24 28"
           , fill "#333"
           , id id_dFatePlus
           ]
    [ path [ d "M20 15v-2c0-0.547-0.453-1-1-1h-5v-5c0-0.547-0.453-1-1-1h-2c-0.547 0-1 0.453-1 1v5h-5c-0.547 0-1 0.453-1 1v2c0 0.547 0.453 1 1 1h5v5c0 0.547 0.453 1 1 1h2c0.547 0 1-0.453 1-1v-5h5c0.547 0 1-0.453 1-1zM24 6.5v15c0 2.484-2.016 4.5-4.5 4.5h-15c-2.484 0-4.5-2.016-4.5-4.5v-15c0-2.484 2.016-4.5 4.5-4.5h15c2.484 0 4.5 2.016 4.5 4.5z" ] []
    ]



dFateBlank : Html msg
dFateBlank =
    svg [ display "inline"
        , width "24"
        , height "28"
        ]
    [ use [ xlinkHref ("#" ++ id_dFateBlank) ] [] ]

id_dFateBlank = "icon-dice-fate-blank"

sym_dFateBlank : Svg msg
sym_dFateBlank =
    symbol [ viewBox "0 0 24 28"
           , fill "#333"
           , id id_dFateBlank
           ]
    [ path [ d "M24 6.5v15c0 2.484-2.016 4.5-4.5 4.5h-15c-2.484 0-4.5-2.016-4.5-4.5v-15c0-2.484 2.016-4.5 4.5-4.5h15c2.484 0 4.5 2.016 4.5 4.5z" ] []
    ]


dFateMinus : Html msg
dFateMinus =
    svg [ display "inline"
        , width "24"
        , height "28"
        ]
    [ use [ xlinkHref ("#" ++ id_dFateMinus) ] [] ]

id_dFateMinus = "icon-dice-fate-minus"

sym_dFateMinus : Svg msg
sym_dFateMinus =
    symbol [ viewBox "0 0 24 28"
           , fill "#333"
           , id id_dFateMinus
           ]
    [ path [ d "M20 15v-2c0-0.547-0.453-1-1-1h-14c-0.547 0-1 0.453-1 1v2c0 0.547 0.453 1 1 1h14c0.547 0 1-0.453 1-1zM24 6.5v15c0 2.484-2.016 4.5-4.5 4.5h-15c-2.484 0-4.5-2.016-4.5-4.5v-15c0-2.484 2.016-4.5 4.5-4.5h15c2.484 0 4.5 2.016 4.5 4.5z" ] []
    ]

