module Fate.GameAspectSheet.Decode exposing
    (decodeGameAspectSheet)

import Array exposing (Array)
import Fate.GameAspectSheet.Types exposing (..)
import Fate.CharacterSheet.Decode exposing (decodeAspect)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

decodeGameAspectSheet : Decoder GameAspectSheet
decodeGameAspectSheet =
    Decode.succeed GameAspectSheet
        |> required "scenes" (array decodeScene)

decodeScene : Decoder Scene
decodeScene =
    Decode.succeed Scene
        |> required "title" string
        |> required "aspects" (array decodeAspect)

