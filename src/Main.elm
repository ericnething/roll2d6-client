module Main exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Array exposing (Array)
import Task
import Util exposing (removeIndexFromArray)
import CharacterSheet.Model as CharacterSheet
import CharacterSheet.Update as CharacterSheet
import CharacterSheet.View as CharacterSheet
import CharacterSheet.Template exposing
    ( initialCharacterSheet
    , fateCore
    , dresdenFilesAccelerated
    , tachyonSquadronShip
    , sarissa_dfa
    , harryDresden_dfa
    )

main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }

-- Model

type ViewMode
    = EditView Int
    | ReadOnlyView

type alias Model =
    { characterSheets : Array CharacterSheet.Model
    , viewMode : ViewMode
    }

initialModel : Model
initialModel =
    { characterSheets =
          Array.fromList
              [ CharacterSheet.initialModel
                    initialCharacterSheet
              , CharacterSheet.initialModel
                    harryDresden_dfa
              , CharacterSheet.initialModel
                    sarissa_dfa
              , CharacterSheet.initialModel
                    tachyonSquadronShip
              , CharacterSheet.initialModel
                    initialCharacterSheet
              , CharacterSheet.initialModel
                    initialCharacterSheet
              ]
    , viewMode = ReadOnlyView
    }

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

-- Update

type Msg
    = CharacterSheetMsg Int CharacterSheet.Msg
    | ChangeViewMode ViewMode
    | AddNewCharacterSheet
    | RemoveCharacterSheet Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CharacterSheetMsg index submsg ->
            case Array.get index model.characterSheets of
                Nothing ->
                    (model, Cmd.none)
                Just characterSheet ->
                    let
                        (updatedCharacterSheet, cmd) =
                            CharacterSheet.update
                            submsg
                            characterSheet
                    in
                        ({ model
                             | characterSheets
                               = Array.set
                                 index
                                 updatedCharacterSheet
                                 model.characterSheets
                         }, Cmd.map (CharacterSheetMsg index) cmd)

        ChangeViewMode viewMode ->
            ({ model
                 | viewMode = viewMode
             }
            , Cmd.none)

        AddNewCharacterSheet ->
            ({ model
                 | characterSheets
                   = Array.push
                     (CharacterSheet.initialModel
                          CharacterSheet.Template.blank)
                     model.characterSheets
             }
            , Task.perform
                ChangeViewMode
                (Task.succeed
                     (EditView
                          (Array.length
                               model.characterSheets))))

        RemoveCharacterSheet index ->
            ({ model
                 | characterSheets
                   = removeIndexFromArray index model.characterSheets
             }
            , Task.perform
                ChangeViewMode
                (Task.succeed ReadOnlyView))


-- View

view : Model -> Html Msg
view model =
    div
    [ css
      [ Css.property "display" "grid"
      , Css.property "grid-template-rows" "2.2rem auto"
      , Css.property "grid-row-gap" "0.8rem"
       
      ]
    ]
    [ topNavigation
    , case model.viewMode of
          ReadOnlyView ->
              characterSheetsView
              model.characterSheets
          EditView index ->
              editCharacterSheetView
              index
              (Array.get index model.characterSheets)
    ]

topNavigation : Html Msg
topNavigation =
    header
    [ css
      [ displayFlex
      , alignItems center
      , backgroundColor (hex "0079bf")
      , Css.height (Css.rem 3)
      , color (hex "fff")
      , position sticky
      , top (px 0)
      , left (px 0)
      ]
    ]
    [ h1 [] [ text "Fate RPG" ]
    , button [ onClick AddNewCharacterSheet ]
        [ text "Add New Character Sheet" ]
    ]

characterSheetsView : Array CharacterSheet.Model -> Html Msg
characterSheetsView characterSheets =
    div [ css
          [ displayFlex
          , alignItems Css.start
          , padding3 (px 0) (Css.rem 0.8) (Css.rem 0.8)
          , overflowX auto
          , Css.property "height" "calc(100vh - 3rem)"
          , Css.property "display" "grid"
          , Css.property "grid-auto-columns" "23rem"
          , Css.property "grid-auto-flow" "column"
          , Css.property "grid-column-gap" "1rem"
          , backgroundColor (hex "0079bf")
          ]
        ]
        (Array.toList
            <| Array.indexedMap
                characterSheetWrapper
                characterSheets)


editCharacterSheetView : Int
                       -> Maybe CharacterSheet.Model
                       -> Html Msg
editCharacterSheetView index mmodel =
    case mmodel of
        Nothing ->
            div [] [ text "Not Found" ]
        Just characterSheet ->
            div [ css
                  [ margin2 (px 0) auto
                  ]
                ]
                [ editCharacterSheetToolbarView index
                , Html.Styled.map
                    (CharacterSheetMsg index)
                    (CharacterSheet.editView characterSheet)
                ]

editCharacterSheetToolbarView : Int -> Html Msg
editCharacterSheetToolbarView index =
    div [ css
          [ displayFlex
          , alignItems center
          ]
        ]
        [ CharacterSheet.defaultButton
              [ onClick (ChangeViewMode ReadOnlyView) ]
              [ text "Done" ]
        , CharacterSheet.defaultButton
              [ onClick (RemoveCharacterSheet index)
              , css
                    [ backgroundColor (hex "ff0000")
                    , color (hex "fff")
                    , hover
                        [ backgroundColor (hex "ee0000") ]
                    ]
              ]
              [ text "Delete" ]
        ]

characterSheetColumn =
    styled div
        [ displayFlex
        , Css.property "flex-direction" "column"
        , Css.property "max-height" "calc(100vh - 3.8rem)"
        -- , borderRadius (Css.rem 0.3)
        , Css.property "display" "grid"
        , Css.property "grid-template-rows" "minmax(auto, 1fr)"
        , Css.property "flex" "0 0 23rem"
        , overflowY auto
        ]

characterSheetList =
    styled div
        [ displayFlex
        , flex (int 1)
        , Css.property "flex-direction" "column"
        , Css.property "align-content" "start"
        -- , padding3 (px 0) (Css.rem 0.6) (Css.rem 0.5)
        -- , overflowY auto
        , Css.property "display" "grid"
        , Css.property "grid-row-gap" "0.6rem"
        ]

characterSheetCard : Int -> CharacterSheet.Model -> Html Msg
characterSheetCard index characterSheet =
    div [ css
          [ borderRadius (Css.em 0.2)
          , backgroundColor (hex "fff")
          ]
        ]
    [ div
      []
      [ CharacterSheet.defaultButton
            [ onClick (ChangeViewMode (EditView index))
            , css
                  [ display block ]
            ]
            [ text "Edit" ]
      ]
    , Html.Styled.map
        (CharacterSheetMsg index)
        (CharacterSheet.readOnlyView characterSheet)
    ]


characterSheetWrapper : Int
                      -> CharacterSheet.Model
                      -> Html Msg
characterSheetWrapper index characterSheet =
    characterSheetColumn []
        [ characterSheetList []
          [ characterSheetCard index characterSheet 
          ]
        ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

