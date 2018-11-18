module Game
    exposing
    ( subscriptions
    , update
    , view
    )

import Array exposing (Array)
import CharacterSheet
import CharacterSheet.Template
import CharacterSheet.View exposing (inputStyles)
import Css exposing (..)
import Game.Types exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Time
import Json.Decode
import PouchDB exposing (PouchDBRef)
import PouchDB.Decode
    exposing
    ( decodeGameData
    , decodePlayerList
    , decodePlayerPresence
    )
import Task
import Util exposing (removeIndexFromArray)
import Browser.Navigation as Navigation
import Route
import RemoteData exposing (WebData)
import API
import Icons


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ PouchDB.getResponse UpdateCurrentGame
        , PouchDB.changesReceived (always ChangesReceived)
        , PouchDB.sse_playerListUpdated
            (ServerEventReceived
                 << PlayerListUpdated
                 << decodePlayerList)
        , PouchDB.sse_playerPresenceUpdated
            (ServerEventReceived
                 << PlayerPresenceUpdated
                 << decodePlayerPresence)
        , Time.every 45000 (always Ping)
        ]


update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
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
                    , Cmd.map (CharacterSheetMsg index) cmd
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
                OpenOverlay
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
                identity
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
            ( model
            , Cmd.batch
                [ API.setPresenceOffline model.id
                , PouchDB.closeEventStream model.ref
                , Navigation.pushUrl
                    navkey
                    (Route.toUrlString Route.Lobby)
                ]
            )

        CreateInvite ->
            ( model
            , API.createInvite model.id
            )

        InviteCreated result ->
            ({ model
                 | overlay = InstantInvite result
             }
            , Cmd.none
            )


        PlayerList gameId players ->
            ({ model | players = players }
            , Cmd.none
            )

        ServerEventReceived (PlayerListUpdated ePlayers) ->
            case ePlayers of
                Ok players ->
                    ({ model
                         | players
                             = RemoteData.succeed players
                     }
                    , Cmd.none
                    )
                Err err ->
                    (model, Cmd.none)

        ServerEventReceived (PlayerPresenceUpdated ePresence) ->
            case ePresence of
                Ok presenceList ->
                    ({ model
                         | players
                             = model.players
                                   |> RemoteData.map
                                      (updatePlayerPresenceList
                                           presenceList)
                     }
                    , Cmd.none
                    )
                Err err ->
                    (model, Cmd.none)

        Ping ->
            (model, API.ping model.id)

        Pong ->
            (model, Cmd.none)

        NoOp ->
            (model, Cmd.none)


updatePlayerPresenceList : List PlayerPresence
                         -> List Person
                         -> List Person
updatePlayerPresenceList presenceList players =
    List.map
        (\player ->
             let
                 maybePresence =
                     List.filter
                     (\{ id } -> player.id == id)
                     presenceList
             in
                 case maybePresence of
                     [] ->
                         player
                     { presence } :: _ ->
                         { player | presence = presence }
        )
        players


-- View


view : Model -> Html Msg
view model =
    div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-rows" "3rem auto"
            , Css.property "grid-row-gap" "0.6rem"
            , backgroundColor (hex "0079bf")
            ]
        ]
        [ topToolbar model
        , characterSheetsView model.characterSheets
        , overlayView model
        ]


--------------------------------------------------
-- Top Navigation
--------------------------------------------------

topNavigation : Html Msg
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
        [
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


--------------------------------------------------
-- Top Toolbar
--------------------------------------------------

topToolbar : Model -> Html Msg
topToolbar model =
    header
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent spaceBetween
            , backgroundColor transparent
            , color (hex "fff")
            , padding3 (Css.em 0.6) (Css.em 1) (Css.em 0)
            ]
        ]
        [ div [ css [ displayFlex ] ]
              [ exitGameButton
              , onlinePlayers model.players
              ]
        , gameTitle model.title
        , buttons
        ]


gameTitle : String -> Html Msg
gameTitle title =
    div
        [ css
            [ margin2 (Css.em 0) (Css.em 1)
            , overflow Css.hidden
            , textOverflow ellipsis
            , whiteSpace noWrap
            , maxWidth (Css.vw 32)
            ]
        ]
        [ text title ]

exitGameButton : Html Msg
exitGameButton =
    toolbarButton [ onClick ExitToLobby
                  , css [ marginRight (Css.em 1) ]
                  ]
    [ text "Exit Game" ]

buttons : Html Msg
buttons =
    div [ css
          [ displayFlex
          , alignItems center
          , whiteSpace noWrap
          ]
        ]
    [ addNewCharacterSheetButton
    , invitePlayerButton
    , gameSettingsButton
    , showPlayerListButton
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


addNewCharacterSheetButton : Html Msg
addNewCharacterSheetButton =
    toolbarButton
        [ onClick AddCharacterSheet
        , css [ marginLeft (Css.em 0.5) ]
        ]
        [ Icons.addCharacterSheet ]
        -- [ text "Add Character Sheet" ]


gameSettingsButton : Html Msg
gameSettingsButton =
    toolbarButton
        [ onClick (OpenOverlay EditGameSettings)
        , css [ marginLeft (Css.em 0.5) ]
        ]
        [ Icons.gameSettings ]
        -- [ text "Game Settings" ]


invitePlayerButton : Html Msg
invitePlayerButton =
    toolbarButton
        [ onClick CreateInvite
        , css [ marginLeft (Css.em 0.5) ]
        ]
        [ Icons.instantInvite ]
        -- [ text "Invite Player" ]

showPlayerListButton : Html Msg
showPlayerListButton =
    toolbarButton
        [ onClick (OpenOverlay ShowPlayerList)
        , css [ marginLeft (Css.em 0.5) ]
        ]
        [ Icons.players ]

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


onlinePlayers : WebData (List Person) -> Html Msg
onlinePlayers players_ =
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
                    , marginRight (Css.em 0.25)
                    , border3 (px 2) solid transparent
                    , lineHeight (num 1.5)
                    ]
                ]
                [ text name ]

        colors =
            Array.fromList
                [ "2ECC40"
                , "FF851B"
                , "85144b"
                , "0074D9"
                , "001f3f"
                ]

        chooseColor id =
            let
                index = modBy id (Array.length colors)
            in
                case Array.get index colors of
                    Nothing ->
                        "0074D9"
                    Just color ->
                        color
    in
    span
        [ css
            [ displayFlex
            , alignItems center
            , marginRight (Css.em 1)
            ]
        ]
        (case players_ of
            RemoteData.Success players ->
                players
                    |> List.filter
                       (\{ presence } -> presence == Online )
                    |> List.map
                       (\{ username, id } ->
                            avatar
                            (String.left 1 username)
                            (chooseColor id))
            _ -> [ text "" ]
        )

--------------------------------------------------
-- Character Sheets
--------------------------------------------------

characterSheetsView : Array CharacterSheet.Model -> Html Msg
characterSheetsView characterSheets =
    div
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
        , Css.property "max-height" "calc(100vh - 3.6rem)"
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


--------------------------------------------------
-- Overlay
--------------------------------------------------

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

overlayView : Model -> Html Msg
overlayView model =
    case model.overlay of
        OverlayNone ->
            text ""
                
        EditCharacterSheet index ->
            overlay
            []
            [ editCharacterSheetView
                  index
                  (Array.get index model.characterSheets)
            ]
                    
        EditGameSettings ->
            overlay [] [ gameSettingsView model ]

        InstantInvite mInvite ->
            overlay [] [ instantInviteView mInvite ]

        ShowPlayerList ->
            overlay [] [ playerListView model.players ]


--------------------------------------------------
-- Game Settings
--------------------------------------------------

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
        , defaultButton
            [ onClick CloseOverlay ]
            [ text "Done" ]
        ]


--------------------------------------------------
-- Instant Invite
--------------------------------------------------

instantInviteView : WebData String -> Html Msg
instantInviteView mInvite =
    div
    [ css
      [ margin2 (Css.em 4) auto
      , backgroundColor (hex "fff")
      , padding (Css.em 2)
      , Css.width (Css.em 32)
      , borderRadius (Css.em 0.2)
      ]
    ]
    [ case mInvite of
          RemoteData.NotAsked ->
              text ""
          RemoteData.Loading ->
              text "Creating your invitation now"
          RemoteData.Failure err ->
              text "Something went wrong."
          RemoteData.Success invite ->
              div []
                  [ label [ css
                            [ display block
                            ]
                          ]
                        [ text "Players can join your game by following this link" ]
                  , input
                        [ type_ "text"
                        , readonly True
                        , value
                              (Route.toUrlString
                                   (Route.Invite invite))
                        , css
                              [ inputStyles
                              , backgroundColor (hex "eee")
                              ]
                        ]
                        []
                  , div []
                      [ text "This invite will expire in 24 hours" ]
                  ]
    , button
          [ type_ "button"
          , onClick CloseOverlay
          ]
          [ text "Close" ]
    ]
            
--------------------------------------------------
-- Player List
--------------------------------------------------

playerListView : WebData (List Person) -> Html Msg
playerListView mPlayers =
    div [ css
          [ margin2 (Css.em 4) auto
          , backgroundColor (hex "fff")
          , padding (Css.em 2)
          , Css.width (Css.em 32)
          , borderRadius (Css.em 0.2)
          ]
        ]
    [ case mPlayers of
          RemoteData.NotAsked ->
              text "Not Asked"
          RemoteData.Loading ->
              text "Loading player list"
          RemoteData.Failure err ->
              text ("Something went wrong." ++ Debug.toString err)
          RemoteData.Success players ->
              div []
                  [ div [] (List.map playerListItemView players)
                  , defaultButton
                        [ onClick CloseOverlay ]
                        [ text "Close" ]
                  ]
    ]                  


playerListItemView : Person -> Html Msg
playerListItemView player =
    div []
        [ text player.username
        , case player.presence of
              Online ->
                  presenceIndicator "00ff00" "online"
              Offline ->
                  presenceIndicator "ff0000" "offline"
        ]


presenceIndicator : String -> String -> Html msg
presenceIndicator color status =
    span [ css
           [ borderRadius (pct 50)
           , Css.color (hex color)
           , margin2 (Css.em 0) (Css.em 0.5)
           ]
         ]
        [ text status ]

defaultButton =
    styled button
        [ whiteSpace noWrap
        , padding2 (Css.em 0.1) (Css.em 0.5)
        , backgroundColor (hex "fff")
        , border3 (px 1) solid (hex "ccc")
        , borderRadius (px 4)
        , cursor pointer
        , hover
            [ backgroundColor (hex "eee") ]
        ]
