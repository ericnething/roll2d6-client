module Main exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Array exposing (Array)
import CharacterSheet.Model as CharacterSheet
import CharacterSheet.Update as CharacterSheet
import CharacterSheet.View as CharacterSheet
import CharacterSheet.Template exposing
    ( initialCharacterSheet
    , fateCore
    , dresdenFilesAccelerated
    , tachyonSquadronShip
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
                    tachyonSquadronShip
              ]
    , viewMode = ReadOnlyView
    }

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

-- Update

type Msg
    = CharacterSheetMsg Int CharacterSheet.Msg
    | ChangeViewMode ViewMode

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

-- View

view : Model -> Html Msg
view model =
    div
    []
    [ h1 [] [ text "Fate RPG" ]
    , case model.viewMode of
          ReadOnlyView ->
              characterSheetsView
              model.characterSheets
          EditView index ->
              editCharacterSheetView
              index
              (Array.get index model.characterSheets)
    ]

characterSheetsView : Array CharacterSheet.Model -> Html Msg
characterSheetsView characterSheets =
    div [ css
          [ displayFlex
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
            div []
                [ CharacterSheet.defaultButton
                      [ onClick (ChangeViewMode ReadOnlyView)
                      , css
                            [ display block ]
                      ]
                      [ text "Done" ]
                , Html.Styled.map
                    (CharacterSheetMsg index)
                    (CharacterSheet.editView characterSheet)
                ]

characterSheetWrapper : Int
                      -> CharacterSheet.Model
                      -> Html Msg
characterSheetWrapper index characterSheet =
    div []
        [ CharacterSheet.defaultButton
              [ onClick (ChangeViewMode (EditView index))
              , css
                    [ display block ]
              ]
              [ text "Edit" ]
        , Html.Styled.map
              (CharacterSheetMsg index)
              (CharacterSheet.readOnlyView characterSheet)
        ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

