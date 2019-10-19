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


module DragDrop exposing (draggable, onDragEnd, onDragEnter, onDragOver, onDragStart, onDrop)

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (attribute)
import Html.Styled.Events exposing (custom)
import Json.Decode as Json


draggable : Attribute msg
draggable =
    attribute "draggable" "true"


onDragStart : (Json.Value -> msg) -> Attribute msg
onDragStart toMsg =
    custom "dragstart"
        (Json.value
            |> Json.andThen
                (\event ->
                    Json.succeed
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
