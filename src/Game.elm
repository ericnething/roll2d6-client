module Game exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Array exposing (Array)
import Task
import Util exposing (removeIndexFromArray)
import CharacterSheet
import CharacterSheet.Template exposing
    ( initialCharacterSheet
    , fateCore
    , dresdenFilesAccelerated
    , tachyonSquadronShip
    , sarissa_dfa
    , harryDresden_dfa
    )

-- Model

type Overlay
    = EditCharacterSheet Int
    | EditGameSettings
    | OverlayNone

type alias Model =
    { id : Int
    , title : String
    , characterSheets : Array CharacterSheet.Model
    , overlay : Overlay
    }

initialModel : Model
initialModel =
    { id = 0
    , title = "My First Game"
    , characterSheets =
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
    , overlay = OverlayNone
    }

emptyModel : Int -> Model
emptyModel id =
    { id = id
    , title = "New Game #" ++ toString id
    , characterSheets = Array.fromList []
    , overlay = OverlayNone
    }

-- Update

type ConsumerMsg
    = ExitToLobby
    | LocalMsg Msg

type Msg
    = CharacterSheetMsg Int CharacterSheet.Msg
    | AddCharacterSheet
    | RemoveCharacterSheet Int
    | UpdateGameTitle String
    | OpenOverlay Overlay
    | CloseOverlay

update : Msg -> Model -> (Model, Cmd ConsumerMsg)
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
                         }, Cmd.map (LocalMsg << CharacterSheetMsg index) cmd)

        AddCharacterSheet ->
            ({ model
                 | characterSheets
                   = Array.push
                     (CharacterSheet.initialModel
                          CharacterSheet.Template.blank)
                     model.characterSheets
             }
            , Task.perform
                (LocalMsg << OpenOverlay)
                (Task.succeed
                     (EditCharacterSheet
                          (Array.length
                               model.characterSheets))))

        RemoveCharacterSheet index ->
            ({ model
                 | characterSheets
                   = removeIndexFromArray index model.characterSheets
             }
            , Task.perform
                LocalMsg
                (Task.succeed CloseOverlay))

        UpdateGameTitle title ->
            ({ model | title = title }
            , Cmd.none)

        OpenOverlay overlay ->
            ({ model | overlay = overlay }, Cmd.none)

        CloseOverlay ->
            ({ model | overlay = OverlayNone }, Cmd.none)


-- View

view : Model -> Html ConsumerMsg
view model =
    div
    [ css
      [ Css.property "display" "grid"
      , Css.property "grid-template-rows" "2.4rem 2.4rem auto"
      , Css.property "grid-row-gap" "0.6rem"
      , backgroundColor (hex "0079bf")
      ]
    ]
    [ topNavigation
    , topToolbar model
          |> Html.Styled.map LocalMsg 
    , characterSheetsView model.characterSheets
          |> Html.Styled.map LocalMsg
    , case model.overlay of
          OverlayNone ->
              text ""

          EditCharacterSheet index ->
              overlay
              []
              [ Html.Styled.map LocalMsg <|
                    editCharacterSheetView
                    index
                    (Array.get index model.characterSheets)
              ]

          EditGameSettings ->
              overlay
              []
              [ gameSettingsView model
                    |> Html.Styled.map LocalMsg
              ]
    ]

topNavigation : Html ConsumerMsg
topNavigation =
    header
    [ css
      [ displayFlex
      , alignItems center
      , justifyContent spaceBetween
      , backgroundColor (rgba 0 0 0 0.15)
      , color (hex "fff")
      , padding2 (px 0) (Css.em 0.2)
      ]
    ]
    [ navigationButton [ onClick ExitToLobby ]
        [ text "My Games" ]
    , appName
    , navigationButton [] [ text "My Account" ]
    ]

topToolbar : Model -> Html Msg
topToolbar model =
    div
    [ css
      [ displayFlex
      , alignItems center
      , backgroundColor transparent
      , color (hex "fff")
      , padding2 (px 0) (Css.em 1)
      ]
    ]
    [ gameTitle model.title
    , onlinePlayers
    , addNewCharacterSheetButton
    , gameSettingsButton
    , invitePlayerButton
    ]

appName : Html msg
appName =
    div [ css
           [ marginLeft (Css.em 1)
           , opacity (num 0.8)
           , Css.property "font-variant" "all-small-caps"
           , fontWeight (int 500)
           , fontSize (Css.em 1.2)
           ]
         ] 
        [ text "Fate RPG" ]

gameTitle : String -> Html Msg
gameTitle title =
    div
    [ css
      [ marginRight (Css.em 1)
      ]
    ]
    [ text title ]

addNewCharacterSheetButton : Html Msg
addNewCharacterSheetButton =
    toolbarButton
    [ onClick AddCharacterSheet
    , css [ marginRight (Css.em 1) ]
    ]
    [ text "Add Character Sheet" ]


gameSettingsButton : Html Msg
gameSettingsButton =
    toolbarButton
    [ onClick (OpenOverlay EditGameSettings)
    , css [ marginRight (Css.em 1) ]
    ]
    [ text "Game Settings" ]

gameSettingsView : Model -> Html Msg
gameSettingsView model =
    div
    [ css
      [ margin2 (Css.em 4) auto
      , backgroundColor (hex "fff")
      , padding (Css.em 2)
      , Css.width (Css.em 32)
      , borderRadius (Css.em 0.2)
      ]
    ]
    [ h1 [] [ text "Game Settings" ]
    , div []
        [ label [] [ text "Game Title" ]
        , input
              [ type_ "text"
              , onInput UpdateGameTitle
              , value model.title
              ] []
        ]
    , button
          [ onClick CloseOverlay ]
          [ text "Done" ]
    ]

invitePlayerButton : Html Msg
invitePlayerButton =
    toolbarButton
    [ css [ marginRight (Css.em 1) ]
    ]
    [ text "Invite Player" ]

invitePlayersCircleButton : Html Msg
invitePlayersCircleButton =
    button
    [ css
      [ borderRadius (px 999)
      , Css.width (Css.em 1.9)
      , Css.height (Css.em 1.9)
      , backgroundColor (rgba 255 255 255 0.2)
      , color (hex "eee")
      , textAlign center
      , marginLeft (Css.em 0.35)
      , border3 (px 2) solid (hex "eee")
      ]
    ] [ text "+" ]

onlinePlayers : Html Msg
onlinePlayers =
    let
        avatar name bg =
            div
            [ css
              [ borderRadius (px 999)
              , Css.width (Css.em 1.9)
              , Css.height (Css.em 1.9)
              , backgroundColor (hex bg)
              , color (hex "eee")
              , textAlign center
              , marginLeft (Css.em -0.35)
              , border3 (px 2) solid (hex "eee")
              ]
            ]
            [ text name ]
    in
        span [ css
               [ displayFlex
               , alignItems center
               , marginRight (Css.em 1)
               ]
             ]
        [ avatar "W" "001f3f"
        , avatar "O" "FF851B"
        , avatar "R" "85144b"
        , avatar "G" "2ECC40"
        , avatar "B" "0074D9"
        ]

characterSheetsView : Array CharacterSheet.Model -> Html Msg
characterSheetsView characterSheets =
    div [ css
          [ displayFlex
          , alignItems Css.start
          , padding3 (px 0) (Css.rem 0.8) (Css.rem 0.8)
          , overflowX auto
          , Css.property "height" "calc(100vh - 6rem)"
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
                  [ margin2 (Css.em 4) auto
                  , backgroundColor (hex "fff")
                  , padding (Css.em 2)
                  , Css.width (Css.em 32)
                  , borderRadius (Css.em 0.2)
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
              [ onClick CloseOverlay ]
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
        , Css.property "max-height" "calc(100vh - 6rem)"
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
      [ css
        [ displayFlex
        , justifyContent flexStart
        , padding3 (Css.em 0.6) (Css.em 0.6) (px 0)
        ]
      ]
      [ CharacterSheet.defaultButton
            [ onClick (OpenOverlay (EditCharacterSheet index))
            , css
                  [ display block ]
            ]
            [ text "Edit" ]
      ]
    , Html.Styled.map
        (CharacterSheetMsg index)
        (CharacterSheet.readOnlyView characterSheet)
    ]


spacer : Html msg
spacer =
    div [] []

characterSheetWrapper : Int
                      -> CharacterSheet.Model
                      -> Html Msg
characterSheetWrapper index characterSheet =
    characterSheetColumn []
        [ characterSheetList []
          [ characterSheetCard index characterSheet
          , spacer
          , spacer
          ]
        ]



toolbarButton =
    styled button
        [ whiteSpace noWrap
        , padding2 (Css.em 0.35) (Css.em 0.5)
        , backgroundColor (rgba 255 255 255 0.2)
        , color (hex "fff")
        , borderRadius (px 4)
        , cursor pointer
        , border (px 0)
        , hover
              [ backgroundColor (rgba 255 255 255 0.3)
              ]
        ]

navigationButton =
    styled button
        [ whiteSpace noWrap
        , lineHeight (num 1)
        , padding2 (Css.em 0.3) (Css.em 0.5)
        , backgroundColor (rgba 255 255 255 0.3)
        , color (hex "fff")
        , borderRadius (px 4)
        , cursor pointer
        , border (px 0)
        , hover
              [ backgroundColor (rgba 255 255 255 0.2)
              ]
        ]


overlay =
    styled div
        [ position fixed
        , top (px 0)
        , left (px 0)
        , Css.height (vh 100)
        , Css.width (vw 100)
        , Css.property "pointer-events" "all"
        , backgroundColor (rgba 0 0 0 0.5)
        , overflowY scroll
        ]
