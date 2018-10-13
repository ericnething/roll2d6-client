module GrowableTextArea exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, at)
import Css exposing (..)

type TextArea
    = TextArea String Int

view : (TextArea -> msg) -> TextArea -> Html msg
view msg (TextArea currentValue scrollHeight) =
    textarea
    [ HA.map msg (on "input" inputDecoder)
    , css
          [ Css.height auto
          , Css.height (px (toFloat scrollHeight))
          , display block
          , resize none
          ]
    , value currentValue
    ]
    []

toString : TextArea -> String
toString (TextArea value _) = value

inputDecoder : Decoder TextArea
inputDecoder =
    Decode.map3
        (\a b c -> TextArea a (Basics.max b c))
        (at [ "target", "value" ] Decode.string)
        (at [ "target", "scrollHeight" ] Decode.int)
        (at [ "target", "clientHeight" ] Decode.int)
