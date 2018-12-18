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

module Game.Sheets exposing
    ( view
    , update
    )


import Array exposing (Array)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Lazy exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Common exposing (defaultButton, inputStyles)

import Game.Sheet as Sheet
import Game.Sheet.Types exposing (SheetModel, SheetMsg)
import Game.Sheets.Types exposing (..)
import Game.GameType exposing (GameType)
import Task
import Util exposing (removeIndexFromArray, findArrayIndexOf, swapArray)
import Game.Decode exposing (scrollDecoder)
import Browser.Dom as Dom
import Time
import API exposing (generateNewSheetId)
import Ports
import DragDrop

update : Msg -> Model r -> (Model r, Cmd Msg)
update msg model =
    case msg of
        SheetMsg id submsg ->
            case Dict.get id model.sheets of
                Nothing ->
                    ( model, Cmd.none )

                Just sheet ->
                    let
                        ( newSheet, cmd ) =
                            Sheet.updateSheet submsg sheet
                    in
                    ( { model
                        | sheets =
                            Dict.insert
                                id
                                newSheet
                                model.sheets
                      }
                    , Cmd.map (SheetMsg id) cmd
                    )

        GenerateNewSheetId withSheet ->
            ( model
            , API.generateNewSheetId model.id withSheet
            )

        NewSheetId sheet result ->
            case result of
                Ok id ->
                    ( model
                    , Task.perform
                        identity
                        (Task.succeed (AddSheet id sheet))
                    )

                Err _ ->
                    ( model, Cmd.none )

        AddSheet id sheet ->
            ( { model
                  | sheets = Dict.insert id sheet model.sheets
                  , sheetsOrdering = Array.push id model.sheetsOrdering
              }
            , Task.perform
                identity
                (Task.succeed (OpenFullSheet id True))
            )

        RemoveSheet id ->
            (model, API.deleteSheetId model.id id)

        SheetRemoved result ->
            case result of
                Ok id ->
                    ( { model
                          | sheets = Dict.remove id model.sheets
                          , sheetsOrdering =
                              Array.filter
                              (\id_ -> id_ /= id)
                              model.sheetsOrdering
                      }
                    , Task.perform
                        identity
                        (Task.succeed CloseFullSheet)
                    )

                Err _ ->
                    (model, Cmd.none)

        OnScroll position ->
            ({ model | sheetsViewportX = toFloat position }
            , Cmd.none
            )

        OpenFullSheet id editing ->
            ({ model | fullSheet = Just (FullSheet id editing) }
            , Cmd.none
            )

        CloseFullSheet ->
            ({ model | fullSheet = Nothing }
            , restoreScrollX
                 "sheets-as-columns-container"
                 model.sheetsViewportX
            )

        ToggleFullSheetEdit ->
            ({ model
                 | fullSheet
                   = Maybe.map
                     (\(FullSheet id editing) ->
                          FullSheet id (not editing))
                     model.fullSheet
             }, Cmd.none
            )

        RestoreScrollX _ ->
            (model, Cmd.none)

        DragStart dragId event ->
            ({ model
                 | movingSheet = Just dragId
             }
            , Ports.dragstart event
            )

        DragEnd ->
            ({ model
                 | movingSheet = Nothing
             }
            , Cmd.none
            )

        DragEnter dropId ->
            (model, Cmd.none)

        DragOver dropId ->
            (model, Cmd.none)

        Drop dropId ->
            let
                ordering =
                    case ( model.movingSheet
                               |> Maybe.andThen
                                  (\dragId ->
                                       findArrayIndexOf
                                       dragId
                                       model.sheetsOrdering
                                  )
                         , findArrayIndexOf dropId model.sheetsOrdering
                         ) of
                        (Just a, Just b) ->
                            swapArray a b model.sheetsOrdering
                        _ ->
                            model.sheetsOrdering
            in
                ({ model
                     | movingSheet = Nothing
                }
                , Task.perform
                    identity
                    (Task.succeed
                         (UpdateSheetsOrdering ordering))
                )

        UpdateSheetsOrdering ordering ->
            ({ model
                 | sheetsOrdering = ordering
             }
            , Cmd.none
            )
        

restoreScrollX : String -> Float -> Cmd Msg
restoreScrollX id xpos =
  Dom.getViewportOf id
    |> Task.andThen
       (\info -> Dom.setViewportOf id xpos 0)
    |> Task.attempt RestoreScrollX
    

view : (Int, Int) -> Model r -> Html Msg
view viewportSize model =
    case model.fullSheet of
        Nothing ->
            lazy2 sheetsView viewportSize model
        Just (FullSheet id editing) ->
            fullSheetView
            (FullSheet id editing)
            (Dict.get id model.sheets)

--------------------------------------------------
-- Game Sheets
--------------------------------------------------

sheetsView : (Int, Int)
           -> {r |
               sheets : Dict SheetId SheetModel
              , sheetsOrdering : Array SheetId
              , sheetsViewportX : Float
              , gameType : GameType
              }
           -> Html Msg
sheetsView (viewportWidth, _) { sheets
                              , sheetsOrdering
                              , sheetsViewportX
                              , gameType
                              } =
    let
        sheetWidth = 24 * 15
        threshold = 20
        minBound =
            if Array.length sheetsOrdering > threshold
            then
                Basics.max 0
                    (floor (sheetsViewportX / sheetWidth) - 1)
            else
                0

        maxBound =
            if Array.length sheetsOrdering > threshold
            then
                ceiling
                (toFloat minBound +
                     (toFloat viewportWidth / sheetWidth) + 1)
            else
                Basics.round (1/0)
            
        _ = Debug.log "Min/MaxBound" (minBound, maxBound)

        -- shift = sheetsVisualShift dragDrop sheetsOrdering
    in
    lazy2 div
        [ css
            [ displayFlex
            , alignItems Css.start
            , padding3 (px 0) (Css.rem 0.8) (Css.rem 0.8)
            , overflowX auto
            , Css.property "height" "calc(100vh - 3.6rem)"
            , Css.property "display" "grid"
            , Css.property "grid-auto-columns" "23rem"
            , Css.property "grid-auto-flow" "column"
            , Css.property "grid-column-gap" "1rem"
            , backgroundColor (hex "0079bf")
            ]
        , on "scroll" (scrollDecoder OnScroll)
        , id "sheets-as-columns-container"
        ]
        (sheetsOrdering
             |> Array.foldl
                (\sheetId (index, acc) ->
                     case Dict.get sheetId sheets of
                         Just sheet ->
                             ( index + 1
                             , Array.push
                                 (sheetWrapper
                                      { bounds = (minBound, maxBound)
                                      , index = index
                                      , sheetId = sheetId
                                      , sheet = sheet
                                      -- , shift = shift
                                      }
                                 )
                                 acc
                             )
                         Nothing ->
                             (index, acc)
                )
                (0, Array.empty)
             |> Tuple.second
             |> Array.push (addNewSheetButtons gameType)
             |> Array.toList
         )

sheetsVisualShift : (Maybe SheetId, Maybe SheetId)
                  -> Array SheetId
                  -> VisualShift
sheetsVisualShift (mdragId, mdropId) sheetsOrdering =
    let
        getSlice dragId xs =
            case xs of
                (indexA, a) :: (indexB, b) :: _ ->
                    if
                        
                        -- The dragged item comes first in the array,
                        -- meaning that the drop target must be on the
                        -- right, and therefore the dragged item has
                        -- been moved to the right.
                        
                        a == dragId
                    then
                        ShiftLeft
                        (Array.slice
                             (indexA + 1)
                             (indexB + 1)
                             sheetsOrdering)
                    else
                        
                        -- The dragged item comes second in the array,
                        -- meaning that the drop target must be on the
                        -- left, and therefore the dragged item has
                        -- been moved to the left.
                        
                        ShiftRight
                        (Array.slice indexB indexA sheetsOrdering)
                _ ->
                    NoShift
    in
    case (mdragId, mdropId) of
        (Just dragId, Just dropId) ->
            sheetsOrdering
                |> Array.toIndexedList
                |> List.filter
                   (\(index, id) -> id == dragId || id == dropId)
                |> getSlice dragId
        _ ->
            NoShift


addNewSheetButtons : GameType -> Html Msg
addNewSheetButtons gameType =
    let
        addNewSheet (title, sheetModel) =
            inlineToolbarButton
            [ onClick (GenerateNewSheetId sheetModel) ]
            [ text ("Add a new " ++ title) ]
    in
        div [ css
              [ displayFlex
              , alignItems stretch
              , flexDirection column
              ]
            ]
        (List.map addNewSheet (Sheet.blank gameType))

inlineToolbarButton =
    styled button
        [ whiteSpace noWrap
        , padding2 (Css.em 0.6) (px 0)
        , marginBottom (Css.em 1)
        , marginRight (Css.em 1)
        , backgroundColor (rgba 255 255 255 0.2)
        , color (hex "fff")
        , borderRadius (px 4)
        , cursor pointer
        , border (px 0)
        , hover
            [ backgroundColor (rgba 255 255 255 0.3)
            ]
        ]

sheetColumn =
    styled div
        [ displayFlex
        , Css.property "flex-direction" "column"
        , Css.property "max-height" "calc(100vh - 3.6rem)"
        , Css.property "display" "grid"
        , Css.property "grid-template-rows" "minmax(auto, 1fr)"
        , Css.property "flex" "0 0 23rem"
        , overflowY auto
        ]


sheetList =
    styled div
        [ displayFlex
        , flex (int 1)
        , Css.property "flex-direction" "column"
        , Css.property "align-content" "start"
        , Css.property "display" "grid"
        , Css.property "grid-row-gap" "0.6rem"
        ]


sheetCard : SheetId -> SheetModel -> Html Msg
sheetCard id sheet =
    div [ css
          [ borderRadius (Css.em 0.2)
          , backgroundColor (hex "fff")
          , Css.maxWidth (Css.em 23)
          ]
        ]
    [ div [ css
            [ displayFlex
            , justifyContent flexStart
            , padding3 (Css.em 0.6) (Css.em 0.6) (px 0)
            , justifyContent spaceBetween
            ]
          ]
          [ viewDetailsButton id
          , moveButton id
          ]
    , Html.Styled.map
        (SheetMsg id)
        (Sheet.compactView sheet)
    ]


viewDetailsButton : SheetId -> Html Msg
viewDetailsButton id =
    defaultButton
        [ onClick (OpenFullSheet id False)
        , css
              [ display block ]
        ]
        [ text "View Details" ]

moveButton : SheetId -> Html Msg
moveButton id =
    div [ DragDrop.draggable
        , DragDrop.onDragStart (DragStart id)
        , DragDrop.onDragEnd DragEnd
        ]
        [ text "Move" ]

spacer : Html msg
spacer =
    div [] []


sheetWrapper : { bounds : (Int, Int)
               , index : Int
               , sheetId : SheetId
               , sheet : SheetModel
               -- , shift : VisualShift
               }
             -> Html Msg
sheetWrapper { bounds, index, sheetId, sheet } =
    let
        (minBound, maxBound) = bounds
        -- transform =
        --     case shift of
        --         NoShift ->
        --             Css.batch []
        --         ShiftLeft array ->
        --             if Array.length (Array.filter ((==) sheetId) array) == 1
        --             then
        --                 Css.property "transform" ("translateX(-24em)")
        --             else
        --                 Css.batch []
                        
        --         ShiftRight array ->
        --             if Array.length (Array.filter ((==) sheetId) array) == 1
        --             then
        --                 Css.property "transform" ("translateX(24em)")
        --             else
        --                 Css.batch []
    in
    if index >= minBound && index <= maxBound
    then
        lazy2 sheetColumn []
            [ sheetList
                  [ DragDrop.onDragEnter (DragEnter sheetId)
                  , DragDrop.onDrop (Drop sheetId)
                  , DragDrop.onDragOver (DragOver sheetId)
                  ]
                  [ sheetCard sheetId sheet
                  , spacer
                  , spacer
                  ]
            ]
    else
        div []
            [ text "᠎" ] -- unicode mongolian vowel separator



fullSheetWrapper =
    styled div
        [ displayFlex
        , Css.property "flex-direction" "column"
        , Css.property "max-height" "calc(100vh - 3.6rem)"
        , Css.property "display" "grid"
        , Css.property "grid-template-rows" "minmax(auto, 1fr)"
        , overflowY auto
        ]

fullSheetView : FullSheet -> Maybe SheetModel -> Html Msg
fullSheetView fullsheet mmodel =
    fullSheetWrapper []
        [ sheetList []
              [ fullSheetCard fullsheet mmodel
              , spacer
              , spacer
              ]
        ]

fullSheetCard : FullSheet -> Maybe SheetModel -> Html Msg
fullSheetCard (FullSheet index editing) mmodel =
    case mmodel of
        Nothing ->
            div [] [ text "Not Found" ]
        Just sheet ->
            div [ css
                  [ Css.property "grid-template-columns" "12em 32em"
                  , Css.property "display" "grid"
                  , Css.property "grid-template-rows" "minmax(auto, 1fr)"
                  , Css.property "grid-gap" "1em"
                  , padding2 (px 0) (Css.em 1)
                  , margin2 (px 0) auto
                  ]
                ]
                [ editSheetToolbarView (FullSheet index editing)
                , div [ css
                        [ backgroundColor (hex "fff")
                        , padding3 (Css.em 0.6) (Css.em 1) (Css.em 0.6)
                        , Css.width (Css.em 32)
                        , borderRadius (Css.em 0.2)
                        ]
                      ]
                      [ div [ css
                              [ displayFlex
                              , justifyContent spaceBetween
                              , alignItems center
                              ]
                            ]
                            [ defaultButton
                                  [ onClick CloseFullSheet ]
                                  [ text "← Go back to all sheets" ]
                            , toggleSwitch
                                  { offTitle = "Locked"
                                  , onTitle = "Editing"
                                  , isActive = editing
                                  , toMsg = always ToggleFullSheetEdit
                                  }
                            ]
                      , Html.Styled.map
                            (SheetMsg index)
                            (if editing
                             then
                                 Sheet.editView sheet
                             else
                                 Sheet.view sheet
                            )
                      ]
                ]


editSheetToolbarView : FullSheet -> Html Msg
editSheetToolbarView (FullSheet index isActive) =
    let
        deleteButton =
            defaultButton
                [ onClick (RemoveSheet index)
                , css
                      [ backgroundColor (hex "ff0000")
                      , color (hex "fff")
                      , hover
                            [ backgroundColor (hex "ee0000") ]
                      ]
                ]
                [ text "Delete" ]

        sectionLabel title =
            div [ css
                  [ Css.color (hex "fff")
                  , margin3 (Css.em 1) (px 0) (Css.em 0.65)
                  ]
                ]
                [ text title ]
    in
        div [ css
              [ opacity (num 0.6)
              , hover [ opacity (num 1) ]
              , displayFlex
              , alignItems flexStart
              , flexDirection column
              ]
            ]
        [ sectionLabel "Assigned to"
        , div [ css
                [ borderTop3 (px 1) solid (hex "fff")
                , paddingTop (Css.em 1)
                , margin3 (Css.em 1) (px 0) (Css.em 0.65)
                , Css.color (hex "fff")
                ]
              ] [ text "If you delete this sheet, there is no way to recover it later." ]
        , deleteButton
        ]

toggleSwitch : { offTitle : String
               , onTitle : String
               , isActive : Bool
               , toMsg : (Bool -> msg)
               }
             -> Html msg
toggleSwitch { offTitle, onTitle, isActive, toMsg } =
    span
        [ css
            [ displayFlex
            , alignItems center
            ]
        ]
        [ label
            [ css
                [ position relative
                , display inlineBlock
                , Css.width (px 60)
                , Css.height (px 34)
                , Css.property "transform" "scale(0.65)"
                ]
            ]
            [ input
                [ type_ "checkbox"
                , HA.checked isActive
                , onCheck toMsg
                , css [ display none ]
                ]
                []
            , span
                [ css
                    [ position absolute
                    , cursor pointer
                    , top (px 0)
                    , left (px 0)
                    , right (px 0)
                    , bottom (px 0)
                    , if isActive then
                        batch
                            [ backgroundColor (hex "2196F3")
                            , before
                                [ Css.property
                                    "-webkit-transform"
                                    "translateX(26px)"
                                , Css.property
                                    "-ms-transform"
                                    "translateX(26px)"
                                , Css.property
                                    "transform"
                                    "translateX(26px)"
                                ]
                            ]

                      else
                        backgroundColor (hex "ccc")
                    , Css.property "-webkit-transition" "0.2s"
                    , Css.property "transition" "0.2s ease"
                    , borderRadius (px 34)
                    , before
                        [ position absolute
                        , Css.property "content" "\"\""
                        , Css.height (px 26)
                        , Css.width (px 26)
                        , left (px 4)
                        , bottom (px 4)
                        , backgroundColor (hex "fff")
                        , Css.property "-webkit-transition" "0.2s"
                        , Css.property "transition" "0.2s ease"
                        , borderRadius (pct 50)
                        ]
                    ]
                ]
                []
            ]
        , span
            [ css
                [ fontSize (pct 80)
                , color (hex "888")
                ]
            ]
            [ text (if isActive then onTitle else offTitle) ]
        ]
