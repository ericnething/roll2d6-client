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

module Game.Sheets exposing
    ( view
    , update
    , sheetPermissionsView
    )


import Array exposing (Array)
import List.Extra as List
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
import Game.Player exposing (..)
import Game.Encode exposing (encodeSheet)
import Task
import Util exposing (removeIndexFromArray, findArrayIndexOf, swapArray)
import Game.Decode exposing (scrollDecoder)
import Browser.Dom as Dom
import Time
import API exposing (generateNewSheetId)
import Ports
import DragDrop
import RemoteData as RemoteData exposing (WebData)
import Ports

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
                  | sheets =
                      Dict.insert id sheet model.sheets

                  , sheetsOrdering =
                      Array.push id model.sheetsOrdering

                  , sheetPermissions =
                      Dict.insert id
                      (SomePlayers [])
                      model.sheetPermissions
              }
            , Cmd.batch
                [ Ports.put
                      ( model.ref
                      , id
                      , encodeSheet sheet
                      )
                , Task.perform
                    identity
                    (Task.succeed (OpenFullSheet id True))
                , Task.perform
                    identity
                    (Task.succeed SheetsOrderingUpdated)
                , Task.perform
                    identity
                    (Task.succeed SheetPermissionsUpdated)
                ]
            )

        RemoveSheet id ->
            ( { model
                  | sheets = Dict.remove id model.sheets
                  , sheetsOrdering =
                      Array.filter
                      (\id_ -> id_ /= id)
                      model.sheetsOrdering
                  , sheetPermissions =
                      Dict.remove id model.sheetPermissions
              }
            , Cmd.batch
                [ Ports.remove (model.ref, id)
                , Task.perform
                    identity
                    (Task.succeed CloseFullSheet)
                , Task.perform
                    identity
                    (Task.succeed SheetsOrderingUpdated)
                , Task.perform
                    identity
                    (Task.succeed SheetPermissionsUpdated)
                ]
            )
            
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
                 | movingSheet = MovingSheet dragId Nothing
             }
            , Ports.dragstart event
            )

        DragEnd ->
            ({ model
                 | movingSheet = NotMoving
             }
            , Cmd.none
            )

        DragEnter dropId ->
            let
                movingSheet =
                    case model.movingSheet of
                        NotMoving ->
                            NotMoving
                        MovingSheet dragId _ ->
                            MovingSheet dragId (Just dropId)
            in
                ({ model
                     | movingSheet = movingSheet
                 }
                , Cmd.none
                )

        DragOver dropId ->
            (model, Cmd.none)

        Drop dropId ->
            case model.movingSheet of
                NotMoving ->
                    (model, Cmd.none)
                MovingSheet dragId _ ->
                    let
                        ordering =
                            moveSheet
                                { ordering = model.sheetsOrdering
                                , from = dragId
                                , to = dropId
                                }
                    in
                        ({ model
                             | movingSheet = NotMoving
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
            , Task.perform
                identity
                (Task.succeed SheetsOrderingUpdated)
            )

        SheetsOrderingUpdated ->
            -- This message is only to let Main.elm know to write the
            -- changes to pouchDB
            (model, Cmd.none)

        OpenSheetPermissions _ ->
            -- This is handled in Game.elm to open the overlay
            (model, Cmd.none)

        UpdateSheetPermissions sheetId sheetPermission ->
            ({ model
                 | sheetPermissions =
                     Dict.insert
                         sheetId
                         sheetPermission
                         model.sheetPermissions
             }
            , Task.perform
                identity
                (Task.succeed SheetPermissionsUpdated)
            )

        SheetPermissionsUpdated ->
            -- This message is only to let Main.elm know to write the
            -- changes to pouchDB
            (model, Cmd.none)


moveSheet : { ordering : Array SheetId
            , from : SheetId
            , to : SheetId
            } -> Array SheetId
moveSheet { from, to, ordering } =
    let
        move xs =
            case xs of
                a :: b :: _ ->
                    if
                        a == from
                    then
                        moveAfter from to ordering
                    else
                        moveBefore from to ordering
                _ ->
                    ordering
    in
        ordering
            |> Array.toList
            |> List.filter (\id -> id == from || id == to)
            |> move

moveBefore : a -> a -> Array a -> Array a
moveBefore a b array =
    array
        |> Array.toList
        |> List.filter ((/=) a)
        |> List.foldl
           (\x acc ->
                if x == b
                then
                    b :: a :: acc
                else
                    x :: acc
           )
           []
        |> List.reverse
        |> Array.fromList


moveAfter : a -> a -> Array a -> Array a
moveAfter a b array =
    array
        |> Array.toList
        |> List.filter ((/=) a)
        |> List.foldl
           (\x acc ->
                if x == b
                then
                    a :: b :: acc
                else
                    x :: acc
           )
           []
        |> List.reverse
        |> Array.fromList


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
                { myPlayer = model.myPlayer
                , fullSheet = FullSheet id editing
                , sheetModel = Dict.get id model.sheets
                , permissions =
                    model.sheetPermissions
                        |> Dict.get id
                        |> Maybe.map
                           (\perm -> (perm, model.players))
                }


--------------------------------------------------
-- Game Sheets
--------------------------------------------------

sheetsView : (Int, Int)
           -> {r |
               sheets : Dict SheetId SheetModel
              , sheetsOrdering : Array SheetId
              , sheetsViewportX : Float
              , gameType : GameType
              , movingSheet : MovingSheet
              , myPlayer : Player
              }
           -> Html Msg
sheetsView (viewportWidth, _) { sheets
                              , sheetsOrdering
                              , sheetsViewportX
                              , gameType
                              , movingSheet
                              , myPlayer
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
            
        -- _ = Debug.log "Min/MaxBound" (minBound, maxBound)
    in
    lazy2 div
        [ css
            [ displayFlex
            , alignItems Css.start
            , padding3 (px 0) (Css.rem 0.8) (Css.rem 0.8)
            , overflowX auto
            , Css.property "height" "calc(100vh - 3rem)"
            , Css.property "display" "grid"
            , Css.property "grid-auto-columns" "23rem"
            , Css.property "grid-auto-flow" "column"
            , Css.property "grid-column-gap" "0.65rem"
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
                                      , shift =
                                          case movingSheet of
                                              MovingSheet _ (Just dropId) ->
                                                  if
                                                      dropId == sheetId
                                                  then
                                                      True
                                                  else
                                                      False
                                              _ ->
                                                  False
                                                      
                                      , myPlayer = myPlayer
                                      }
                                 )
                                 acc
                             )
                         Nothing ->
                             (index, acc)
                )
                (0, Array.empty)
             |> Tuple.second
             |> (\array ->
                     case myPlayer.role of
                         PlayerRole ->
                             array
                         _ ->
                             Array.push
                             (addNewSheetButtons gameType)
                             array
                )
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
        , Css.property "max-height" "calc(100vh - 3rem)"
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


sheetCard : Player -> SheetId -> SheetModel -> Html Msg
sheetCard myPlayer id sheet =
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
          , case myPlayer.role of
                PlayerRole ->
                    text ""
                _ ->
                    moveButton id
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
        , css
              [ hover
                    [ Css.property "cursor" "grab" ]
              , active
                    [ Css.property "cursor" "grabbing" ]
              ]
        ]
        [ text "Move" ]

spacer : Html msg
spacer =
    div [] []


sheetWrapper : { bounds : (Int, Int)
               , index : Int
               , sheetId : SheetId
               , sheet : SheetModel
               , shift : Bool
               , myPlayer : Player
               }
             -> Html Msg
sheetWrapper { bounds, index, sheetId, sheet, shift, myPlayer } =
    let
        (minBound, maxBound) = bounds
    in
    if index >= minBound && index <= maxBound
    then
        lazy2 sheetColumn [ DragDrop.onDragEnter (DragEnter sheetId)
                          , DragDrop.onDrop (Drop sheetId)
                          , DragDrop.onDragOver (DragOver sheetId)
                          ]
            [ sheetList
                  [ css
                    [ if shift
                      then
                          Css.property "transform" "translateY(1em)"
                      else
                          Css.batch []
                    ]
                  ]
                  [ sheetCard myPlayer sheetId sheet
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
        , Css.property "max-height" "calc(100vh - 3rem)"
        , Css.property "display" "grid"
        , Css.property "grid-template-rows" "minmax(auto, 1fr)"
        , overflowY auto
        ]

fullSheetView : { myPlayer : Player
                , fullSheet : FullSheet
                , sheetModel : Maybe SheetModel
                , permissions : Maybe (SheetPermission, List Player)
                }
              -> Html Msg
fullSheetView { myPlayer, fullSheet, sheetModel, permissions } =
    fullSheetWrapper []
        [ sheetList []
              [ fullSheetCard
                    { myPlayer = myPlayer
                    , fullSheet = fullSheet
                    , sheetModel = sheetModel
                    , permissions = permissions
                    }
              , spacer
              , spacer
              ]
        ]

fullSheetCard : { myPlayer : Player
                , fullSheet : FullSheet
                , sheetModel : Maybe SheetModel
                , permissions : Maybe (SheetPermission, List Player)
                }
              -> Html Msg
fullSheetCard { myPlayer
              , fullSheet
              , sheetModel
              , permissions
              } =
    let
        (FullSheet sheetId editing) = fullSheet

        editToggle =
            toggleSwitch
            { offTitle = "Locked"
            , onTitle = "Editing"
            , isActive = editing
            , toMsg = always ToggleFullSheetEdit
            }
    in
    case sheetModel of
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
                [ editSheetToolbarView
                      { myPlayer = myPlayer
                      , fullSheet = fullSheet
                      , permissions = permissions
                      }
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
                            , case myPlayer.role of
                                  PlayerRole ->
                                      if hasPermission
                                          (Maybe.map Tuple.first permissions)
                                          myPlayer.id
                                      then
                                          editToggle
                                      else
                                          text ""
                                  _ ->
                                      editToggle
                            ]
                      , Html.Styled.map
                            (SheetMsg sheetId)
                            (if editing
                             then
                                 Sheet.editView sheet
                             else
                                 Sheet.view sheet
                            )
                      ]
                ]


editSheetToolbarView :
    { myPlayer : Player
    , fullSheet : FullSheet
    , permissions : Maybe (SheetPermission, List Player)
    }
    -> Html Msg
editSheetToolbarView { myPlayer, fullSheet, permissions } =
    let
        (FullSheet sheetId isActive) = fullSheet

        deleteButton =
            defaultButton
                [ onClick (RemoveSheet sheetId)
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
              , Css.color (hex "fff")
              ]
            ]
        [ sectionLabel "Assigned to"
        , assignedToSheetView permissions
        , case myPlayer.role of
              PlayerRole ->
                  text ""
              _ ->
                  defaultButton
                  [ onClick (OpenSheetPermissions sheetId) ]
                  [ text "Manage Permissions" ]

        , case myPlayer.role of
              PlayerRole ->
                  text ""
              _ ->
                  div []
                  [ div [ css
                          [ borderTop3 (px 1) solid (hex "fff")
                          , paddingTop (Css.em 1)
                          , margin3 (Css.em 1) (px 0) (Css.em 0.65)
                          , Css.color (hex "fff")
                          ]
                        ]
                        [ text "If you delete this sheet, there is no way to recover it later." ]
                  , deleteButton
                  ]
        ]

assignedToSheetView : Maybe (SheetPermission, List Player) -> Html Msg
assignedToSheetView mpermissions =
    case mpermissions of
        Just (permissions, players) ->
            case permissions of
                SomePlayers ids ->
                    ul []
                        (List.map
                             (\player ->
                                  if List.member player.id ids
                                  then
                                      li [] [ text player.displayName ]
                                  else
                                      text ""
                             )
                             players
                        )
                AllPlayers ->
                    ul [] [ li [] [ text "Everyone" ] ]
        _ ->
            text ""

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


--------------------------------------------------
-- Sheet Permissions
--------------------------------------------------

sheetPermissionsView
    : SheetId
    -> { r |
         myPlayer : Player
       , players : List Player
       , sheetPermissions : Dict SheetId SheetPermission
       }
    -> Html Msg
sheetPermissionsView sheetId { myPlayer
                             , players
                             , sheetPermissions
                             } =
    let
        allPlayersToggle =
            div [ css
                  [ displayFlex
                  , alignItems center
                  , justifyContent spaceBetween
                  ]
                ]
                [ div [] [ text "Everyone" ]
                , toggleSwitch
                      { offTitle = ""
                      , onTitle = ""
                      , isActive =
                          case Dict.get sheetId sheetPermissions of
                              Just AllPlayers ->
                                  True
                              _ ->
                                  False
                      , toMsg =
                          \isActive ->
                              if
                                  isActive
                              then
                                  UpdateSheetPermissions
                                  sheetId
                                  AllPlayers
                              else
                                  UpdateSheetPermissions
                                  sheetId
                                  (SomePlayers [])
                      }
                ]
        toSheetPermission playerId isActive =
            case Dict.get sheetId sheetPermissions of
                Just (SomePlayers ids) ->
                    if
                        isActive
                    then
                        SomePlayers (playerId :: ids)
                    else
                        SomePlayers (List.filter ((/=) playerId) ids)

                Just AllPlayers ->
                    SomePlayers [ playerId ]

                Nothing ->
                    if
                        isActive
                    then
                        SomePlayers [playerId]
                    else
                        SomePlayers []

        playerView player =
            div [ css
                  [ displayFlex
                  , alignItems center
                  , justifyContent spaceBetween
                  ]
                ]
                [ div [] [ text player.displayName ]
                , toggleSwitch
                      { offTitle = ""
                      , onTitle = ""
                      , isActive =
                          case Dict.get sheetId sheetPermissions of
                              Just (SomePlayers ids) ->
                                  if List.member player.id ids
                                  then
                                      True
                                  else
                                      False
                              _ ->
                                  False
                      , toMsg =
                          UpdateSheetPermissions sheetId
                          << toSheetPermission player.id
                      }
                ]
    in
        div []
            (allPlayersToggle :: List.map playerView players)


hasPermission : Maybe SheetPermission
              -> PlayerId
              -> Bool
hasPermission mpermissions playerId =
    case mpermissions of
        Just AllPlayers ->
            True

        Just (SomePlayers ids) ->
            List.member playerId ids

        Nothing ->
            False
