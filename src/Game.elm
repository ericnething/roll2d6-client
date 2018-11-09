module Game exposing (subscriptions, update, view)

import Array exposing (Array)
import CharacterSheet
import CharacterSheet.Template
import Css exposing (..)
import Game.Types exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode
import PouchDB exposing (PouchDBRef)
import PouchDB.Decode exposing (decodeGameData)
import Task
import Util exposing (removeIndexFromArray)
import Browser.Navigation as Navigation
import Route


-- init : GameId -> (Model, Cmd ConsumerMsg)
-- init gameId =
    

subscriptions : Model -> Sub ConsumerMsg
subscriptions _ =
    Sub.batch
        [ PouchDB.getResponse (LocalMsg << UpdateCurrentGame)
        , PouchDB.changesReceived
            (always (LocalMsg <| ChangesReceived))
        ]


update : Navigation.Key -> Msg -> Model -> ( Model, Cmd ConsumerMsg )
update navkey msg model =
    case msg of
        CharacterSheetMsg index submsg ->
            case Array.get index model.characterSheets of
                Nothing ->
                    ( model, Cmd.none )

                Just characterSheet ->
                    let
                        ( updatedCharacterSheet, cmd ) =
                            CharacterSheet.update
                                submsg
                                characterSheet
                    in
                    ( { model
                        | characterSheets =
                            Array.set
                                index
                                updatedCharacterSheet
                                model.characterSheets
                      }
                    , Cmd.map (LocalMsg << CharacterSheetMsg index) cmd
                    )

        AddCharacterSheet ->
            ( { model
                | characterSheets =
                    Array.push
                        (CharacterSheet.initialModel
                            CharacterSheet.Template.blank
                        )
                        model.characterSheets
              }
            , Task.perform
                (LocalMsg << OpenOverlay)
                (Task.succeed
                    (EditCharacterSheet
                        (Array.length
                            model.characterSheets
                        )
                    )
                )
            )

        RemoveCharacterSheet index ->
            ( { model
                | characterSheets =
                    removeIndexFromArray index model.characterSheets
              }
            , Task.perform
                LocalMsg
                (Task.succeed CloseOverlay)
            )

        UpdateGameTitle title ->
            ( { model | title = title }
            , Cmd.none
            )

        OpenOverlay overlay_ ->
            ( { model | overlay = overlay_ }, Cmd.none )

        CloseOverlay ->
            ( { model | overlay = OverlayNone }, Cmd.none )

        UpdateCurrentGame value ->
            case decodeGameData value of
                Ok gameData ->
                    ( mergeGameData model gameData
                    , Cmd.none
                    )

                Err err ->
                    ( model, Cmd.none )

        ChangesReceived ->
            ( model, PouchDB.get model.ref )

        ExitToLobby ->
            (model
            , Navigation.pushUrl
                navkey
                (Route.toUrlString Route.Lobby)
            )


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
        [ navigationButton [ onClick (LocalMsg ExitToLobby) ]
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
    div
        [ css
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
                ]
                []
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
        ]
        [ text "+" ]


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
    span
        [ css
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
    div
        [ css
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
        (Array.toList <|
            Array.indexedMap
                characterSheetWrapper
                characterSheets
        )


editCharacterSheetView :
    Int
    -> Maybe CharacterSheet.Model
    -> Html Msg
editCharacterSheetView index mmodel =
    case mmodel of
        Nothing ->
            div [] [ text "Not Found" ]

        Just characterSheet ->
            div
                [ css
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
    div
        [ css
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


characterSheetWrapper :
    Int
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
