module DragDrop exposing (..)

import Json.Decode as Json
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (attribute)
import Html.Styled.Events exposing (custom)

draggable : Attribute msg
draggable = attribute "draggable" "true"

onDragStart : (Json.Value -> msg) -> Attribute msg
onDragStart toMsg =
    custom "dragstart"
    (Json.value
         |> Json.andThen
            (\event -> Json.succeed
                 { message = toMsg event
                 , stopPropagation = True
                 , preventDefault = False
                 }
            )
    )

onDragEnd : msg -> Attribute msg
onDragEnd msg =
    custom "dragend"
    (Json.succeed
         { message = msg
         , stopPropagation = True
         , preventDefault = False
         }
    )

onDragEnter : msg -> Attribute msg
onDragEnter msg =
    custom "dragenter"
    (Json.succeed
         { message = msg
         , stopPropagation = True
         , preventDefault = True
         }
    )

onDragOver : msg -> Attribute msg
onDragOver msg =
    custom "dragover"
    (Json.succeed
         { message = msg
         , stopPropagation = False
         , preventDefault = True
         }
    )

onDrop : msg -> Attribute msg
onDrop msg =
    custom "drop"
    (Json.succeed
         { message = msg
         , stopPropagation = True
         , preventDefault = True
         }
    )
