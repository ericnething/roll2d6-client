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
    ( -- General
      xCircle
    , xCircleBig
    , plusCircle

    -- Chat
    , settings
    , phone
    , phoneMissed
    , mic
    , micOff
    , headphones

    -- Dice
    , diceDefs
    , dFatePlus
    , dFateBlank
    , dFateMinus
    )

import Html.Styled exposing (Html, div)
import Html.Styled.Attributes as Html exposing (css)
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
    , css
    )

import Css

------------------------------------------------------------
-- Chat Icons
------------------------------------------------------------

xCircle : Html msg
xCircle =
    svg [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , display "block"
        , css
              [ Css.width (Css.rem 1.6)
              , Css.height (Css.rem 1.6)
              ]
        ]
    [ circle [ cx "12", cy "12", r "10" ] []
    , line [ x1"15", y1 "9", x2 "9", y2 "15" ] []
    , line [ x1 "9", y1 "9", x2 "15", y2 "15" ] []
    ]

xCircleBig : Html msg
xCircleBig =
    svg [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , display "block"
        , css
              [ Css.width (Css.rem 2.2)
              , Css.height (Css.rem 2.2)
              ]
        ]
    [ circle [ cx "12", cy "12", r "10" ] []
    , line [ x1"15", y1 "9", x2 "9", y2 "15" ] []
    , line [ x1 "9", y1 "9", x2 "15", y2 "15" ] []
    ]

phone : Html msg
phone =
    svg [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , display "block"
        , css
              [ Css.width (Css.rem 1.33)
              , Css.height (Css.rem 1.33)
              ]
        ]
    [ path [ d "M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z" ] []
    ]


phoneMissed : Html msg
phoneMissed =
    svg [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , display "block"
        , css
              [ Css.width (Css.rem 1.33)
              , Css.height (Css.rem 1.33)
              ]
        ]
    [ line [ x1 "23", y1 "1", x2 "17", y2 "7" ] []
    , line [ x1 "17", y1 "1", x2 "23", y2 "7" ] []
    , path [ d "M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z" ] []
    ]

mic : Html msg
mic =
    svg [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , display "block"
        , css
              [ Css.width (Css.rem 1.33)
              , Css.height (Css.rem 1.33)
              ]
        ]
    [ path [ d "M12 1a3 3 0 0 0-3 3v8a3 3 0 0 0 6 0V4a3 3 0 0 0-3-3z" ] []
    , path [ d "M19 10v2a7 7 0 0 1-14 0v-2" ] []
    , line [ x1 "12", y1 "19", x2 "12", y2 "23" ] []
    , line [ x1 "8", y1 "23", x2 "16", y2 "23" ] []
    ]

micOff : Html msg
micOff =
    svg [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , display "block"
        , css
              [ Css.width (Css.rem 1.33)
              , Css.height (Css.rem 1.33)
              ]
        ]
    [ line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
    , path [ d "M9 9v3a3 3 0 0 0 5.12 2.12M15 9.34V4a3 3 0 0 0-5.94-.6" ] []
    , path [ d "M17 16.95A7 7 0 0 1 5 12v-2m14 0v2a7 7 0 0 1-.11 1.23" ] []
    , line [ x1 "12", y1 "19", x2 "12", y2 "23" ] []
    , line [ x1 "8", y1 "23", x2 "16", y2 "23" ] []
    ]

settings : Html msg
settings =
    svg [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , display "block"
        , css
              [ Css.width (Css.rem 1.33)
              , Css.height (Css.rem 1.33)
              ]
        ]
    [ circle [ cx "12", cy "12", r "3" ] []
    , path [ d "M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z" ] []
        ]


headphones : Html msg
headphones =
    svg [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , display "block"
        , css
              [ Css.width (Css.rem 1.33)
              , Css.height (Css.rem 1.33)
              ]
        ]
    [ path [ d "M3 18v-6a9 9 0 0 1 18 0v6" ] []
    , path [ d "M21 19a2 2 0 0 1-2 2h-1a2 2 0 0 1-2-2v-3a2 2 0 0 1 2-2h3zM3 19a2 2 0 0 0 2 2h1a2 2 0 0 0 2-2v-3a2 2 0 0 0-2-2H3z"] []
    ]

plusCircle : Html msg
plusCircle =
    svg [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , display "block"
        , css
              [ Css.width (Css.rem 1.33)
              , Css.height (Css.rem 1.33)
              ]
        ]
    [ circle [ cx "12", cy "12", r "10" ] []
    , line [ x1 "12", y1 "8", x2 "12", y2 "16" ] []
    , line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
    ]
------------------------------------------------------------
-- Dice
------------------------------------------------------------

diceDefs : Html msg
diceDefs =
    div [ Html.css
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

