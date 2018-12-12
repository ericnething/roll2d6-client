module Game.Sheets exposing
    ( view
    , update
    )


import Array exposing (Array)

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
import Util exposing (removeIndexFromArray)
import Game.Decode exposing (scrollDecoder)
import Browser.Dom as Dom

update : Msg -> Model r -> (Model r, Cmd Msg)
update msg model =
    case msg of
        SheetMsg index submsg ->
            case Array.get index model.sheets of
                Nothing ->
                    ( model, Cmd.none )

                Just sheet ->
                    let
                        ( newSheet, cmd ) =
                            Sheet.updateSheet submsg sheet
                    in
                    ( { model
                        | sheets =
                            Array.set
                                index
                                newSheet
                                model.sheets
                      }
                    , Cmd.map (SheetMsg index) cmd
                    )

        AddSheet sheet ->
            ( { model
                | sheets =
                    Array.push
                        sheet
                        model.sheets
              }
            , Task.perform
                OpenFullSheet
                (Task.succeed (Array.length model.sheets))
            )

        RemoveSheet index ->
            ( { model
                | sheets =
                    removeIndexFromArray index model.sheets
              }
            , Task.perform
                identity
                (Task.succeed CloseFullSheet)
            )

        OnScroll position ->
            ({ model | sheetsViewportX = toFloat position }
            , Cmd.none
            )

        OpenFullSheet index ->
            ({ model | fullSheet = Just (FullSheet index False) }
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
                     (\(FullSheet index editing) ->
                          FullSheet index (not editing))
                     model.fullSheet
             }, Cmd.none
            )

        RestoreScrollX _ ->
            (model, Cmd.none)
        

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
        Just (FullSheet index editing) ->
            fullSheetView
            (FullSheet index editing)
            (Array.get index model.sheets)

--------------------------------------------------
-- Game Sheets
--------------------------------------------------

sheetsView : (Int, Int)
           -> {r |
               sheets : Array SheetModel
              , sheetsViewportX : Float
              , gameType : GameType
              }
           -> Html Msg
sheetsView (viewportWidth, _) { sheets, sheetsViewportX, gameType } =
    let
        sheetWidth = 24 * 15
        minBound =
            Basics.max 0
                (floor (sheetsViewportX / sheetWidth) - 1)
        maxBound =
            ceiling
            (toFloat minBound +
                 (toFloat viewportWidth / sheetWidth) + 1)

        threshold = 20
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
        ((Array.toList <|
             if Array.length sheets > threshold
             then
                 (Array.indexedMap
                      (sheetWrapper
                           (Debug.log
                                "Min/MaxBound"
                                (minBound, maxBound)))
                      sheets)
            else
                Array.indexedMap
                    (sheetWrapper (0, Basics.round (1/0)))
                    sheets
        ) ++
             [ addNewSheetButtons gameType ])
                


addNewSheetButtons : GameType -> Html Msg
addNewSheetButtons gameType =
    let
        addNewSheet (title, blank) =
            inlineToolbarButton
            [ onClick (AddSheet blank) ]
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


-- editSheetView :
--     Int
--     -> Maybe SheetModel
--     -> Html Msg
-- editSheetView index mmodel =
--     case mmodel of
--         Nothing ->
--             div [] [ text "Not Found" ]

--         Just sheet ->
--             div
--                 [ css
--                     [ margin2 (Css.em 4) auto
--                     , backgroundColor (hex "fff")
--                     , padding (Css.em 2)
--                     -- , Css.width (Css.em 32)
--                     , borderRadius (Css.em 0.2)
--                     ]
--                 ]
--                 [ editSheetToolbarView index
--                 , Html.Styled.map
--                     (SheetMsg index)
--                     (Sheet.editView sheet)
--                 ]


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


sheetCard : Int -> SheetModel -> Html Msg
sheetCard index sheet =
    div
        [ css
            [ borderRadius (Css.em 0.2)
            , backgroundColor (hex "fff")
            , Css.maxWidth (Css.em 23)
            ]
        ]
        [ div
            [ css
                [ displayFlex
                , justifyContent flexStart
                , padding3 (Css.em 0.6) (Css.em 0.6) (px 0)
                ]
            ]
            [ defaultButton
                [ onClick (OpenFullSheet index)
                , css
                    [ display block ]
                ]
                [ text "View Details" ]
            ]
        , Html.Styled.map
            (SheetMsg index)
            (Sheet.compactView sheet)
        ]


spacer : Html msg
spacer =
    div [] []


sheetWrapper : (Int, Int)
             -> Int
             -> SheetModel
             -> Html Msg
sheetWrapper (minBound, maxBound) index sheet =
    if index >= minBound && index <= maxBound
    then
        lazy2 sheetColumn []
            [ sheetList []
                  [ sheetCard index sheet
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
                  [ Css.property "grid-template-columns" "16em 32em"
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
                        , padding (Css.em 1)
                        , Css.width (Css.em 32)
                        , borderRadius (Css.em 0.2)
                        -- , margin2 (Css.em 0) auto
                        ]
                      ]
                      [ toggleSwitch
                            { offTitle = "Locked"
                            , onTitle = "Editing"
                            , isActive = editing
                            , toMsg = always ToggleFullSheetEdit
                            }
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
        doneButton =
            defaultButton
                [ onClick CloseFullSheet ]
                [ text "← Go back to all sheets" ]

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
        [ doneButton
        , sectionLabel "Assigned to"
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
