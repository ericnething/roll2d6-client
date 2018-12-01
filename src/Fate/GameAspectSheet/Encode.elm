module Fate.GameAspectSheet.Encode exposing
    (encodeGameAspectSheet)

import Array exposing (Array)
import Fate.GameAspectSheet.Types exposing (..)
import Fate.CharacterSheet.Encode exposing (encodeAspect)
import Json.Encode exposing (..)

encodeGameAspectSheet : GameAspectSheet -> Value
encodeGameAspectSheet sheet =
    object
        [ ( "scenes", array encodeScene sheet.scenes )
        ]

encodeScene : Scene -> Value
encodeScene scene =
    object
        [ ( "title", string scene.title )
        , ( "aspects", array encodeAspect scene.aspects )
        ]
