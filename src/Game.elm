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

module Game
    exposing
    ( update
    , view
    , subscriptions
    , maybeWriteToPouchDB
    )

import Array exposing (Array)
import Dict
import Debouncer.Messages as Debouncer
    exposing
        ( Debouncer
        , provideInput
        , toDebouncer
        )
import Chat
import Chat.Types as Chat
import Game.Sheet as Sheet
import Css exposing (..)
import Game.Types exposing (..)
import Game.GameType exposing (..)
import Game.Player exposing (..)
import Game.Sheets.Types as Sheets exposing (SheetId)
import Game.Sheets as Sheets
import Game.Sheet.Types exposing (SheetMsg, SheetModel)
import Game.Encode
    exposing
    ( encodeGame
    , encodeGameData
    , encodeSheet
    )
import Html
import Html.Styled exposing (..)
import Html.Styled.Lazy exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Common exposing (defaultButton, inputStyles)
import Time
import Json.Decode
import Ports exposing (PouchDBRef)
import Game.Decode
    exposing
    ( decodeGameData
    , decodePlayerList
    , scrollDecoder
    , decodeSheetUpdate
    , decodeChanges
    )
import Task
import Util exposing (toCmd)
import Browser.Navigation as Navigation
import Route
import RemoteData exposing (WebData)
import API
import Icons
import List.Extra as List
import Browser.Dom as Dom
import Http
import Ports


subscriptions : Sub Msg
subscriptions =
    Ports.changesReceived ChangesReceived

update : Navigation.Key
       -> Msg
       -> Model
       -> (Model, Cmd Msg)
update navkey msg model =
    case msg of
        SheetsMsg submsg ->
            let
                (newModel, cmd) = Sheets.update submsg model
            in
                ( newModel
                , Cmd.map SheetsMsg cmd)

        UpdateGameTitle title ->
            ( { model | title = title }, Cmd.none)

        UpdateGameTitleInDB ->
            ( model
            , Cmd.batch
                [ API.updateGameTitle model.id model.title
                , toCmd CloseOverlay
                ]
            )

        GameTitleUpdated ->
            (model, Cmd.none)

        OpenOverlay ->
            ({ model | overlay = ShowOverlay }, Cmd.none)

        CloseOverlay ->
            ({ model | overlay = HideOverlay }, Cmd.none)

        ChangesReceived changes ->
            case decodeChanges model.gameType changes of
                Ok { maybeGame, sheets } ->
                    let
                        newModel =
                            case maybeGame of
                                Nothing ->
                                    model
                                Just game ->
                                    mergeGameData model game
                    in
                        ({ newModel
                             | sheets
                                 = Dict.union sheets model.sheets
                         }
                        , Cmd.none
                        )

                Err err ->
                    let _ = Debug.log "Changes Received Error" err in
                    (model, Cmd.none)

        ExitToLobby ->
            ( model
            , Cmd.batch
                [ toCmd <| ChatMsg (Chat.LeaveRoom model.id)
                , Navigation.replaceUrl
                    navkey
                    (Route.toUrlString Route.Lobby)
                ]
            )

        SwitchToLobby ->
            (model, Cmd.none)

        RemovePlayer playerId ->
            (model, API.removePlayer model.id playerId)

        PlayerRemoved gameId playerId result ->
            case result of
                Ok _ ->
                    ({ model
                         | sheetPermissions =
                             Dict.map
                             (\_ permission ->
                                  case permission of
                                      Sheets.SomePlayers ids ->
                                          Sheets.SomePlayers
                                          (List.filter
                                               ((/=) playerId)
                                               ids)
                                      Sheets.AllPlayers ->
                                          Sheets.AllPlayers
                             )
                             model.sheetPermissions
                     }
                    , toCmd PlayerRemovedSuccess
                    )
                Err _ ->
                    ( model, Cmd.none )

        PlayerRemovedSuccess ->
            -- This message is only to let Main.elm know to write the
            -- changes to pouchDB
            (model, Cmd.none)

        ChatMsg _ ->
            -- This is handled in Main.elm
            (model, Cmd.none)

        DebounceMsg submsg ->
            Debouncer.update (update navkey) updateDebouncer submsg model

        WriteGameToPouchDB ref docId game ->
            ( model
            , Ports.put
                ( ref
                , docId
                , encodeGameData game
                )
            )

        WriteSheetToPouchDB ref sheetId gameType sheet ->
            ( model
            , Ports.put
                ( ref
                , sheetId
                , encodeSheet sheet
                )
            )

        NoOp ->
            (model, Cmd.none)

        SwitchTab ->
            (model, Cmd.none)
                
        SwitchSettingsTab tab ->
            ({ model | settingsTab = tab }, Cmd.none)


updateDebouncer : Debouncer.UpdateConfig Msg Model
updateDebouncer =
    { mapMsg = DebounceMsg
    , getDebouncer = .debouncer
    , setDebouncer =
        \debouncer model ->
            { model | debouncer = debouncer }
    }

view : (Int, Int) -> Maybe Chat.Room -> Model -> Html Msg
view viewportSize mChatRoom model =
    div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-rows" "3rem auto"
            , Css.property "grid-template-columns" "1fr 26em"
            -- , Css.property "grid-row-gap" "0.6rem"
            , backgroundColor (hex "0079bf")
            ]
        ]
        [ lazy topNavigation model
        , Sheets.view viewportSize model
            |> Html.Styled.map SheetsMsg
        , lazy sidebar mChatRoom
        , lazy maybeOverlayView model
        ]


--------------------------------------------------
-- Top Toolbar
--------------------------------------------------

topNavigation : Model -> Html Msg
topNavigation model =
    header
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent spaceBetween
            , backgroundColor transparent
            , color (hex "fff")
            , padding2 (Css.em 0) (Css.em 1)
            , Css.property "grid-column" "1 / 3"
            ]
        ]
        [ topNavigationSection []
          [ button [ css
                     [ displayFlex
                     , marginRight auto
                     , backgroundColor transparent
                     , border (px 0)
                     , color inherit
                     , cursor pointer
                     ]
                 , onClick SwitchToLobby
                 ]
                [ userBadge "Geronimo" "/lib/fox-avatar-2.jpg" ]
          ]
        , topNavigationSection [] [ tabsView model ]
        , topNavigationSection []
              [ span [ css [ marginLeft auto ] ]
                    [ onlinePlayers model.players ]
              ]
        ]

userBadge : String -> String -> Html Msg
userBadge name url =
    div [ css
          [ displayFlex
          , alignItems center
          ]
        ]
    [ img [ css
            [ borderRadius (pct 50)
            , Css.height (Css.rem 2.133) -- 32px when base is 15px
            , Css.width (Css.rem 2.133)
            ]
          , src url
          ] []
    , span [ css
             [ marginLeft (Css.em 0.5)
             ]
           ]
        [ text name ]
    ]


topNavigationSection =
    styled div
        [ displayFlex
        , flex (int 1)
        , justifyContent center
        ]

tabView : String -> Msg -> Bool -> Html Msg
tabView name handleClick isActive =
    div [ css
          [ if isActive then
                Css.batch
                    [ backgroundColor (rgba 255 255 255 0.2)
                    , color (hex "fff")
                    ]
            else
                Css.batch
                    [ color (rgba 255 255 255 0.70)
                    , hover
                        [ color (hex "fff") ]
                    ]
          , borderRadius (px 4)
          , padding2 (Css.em 0.4) (Css.em 0.7)
          , cursor pointer
          ]
        , onClick handleClick
        ]
        [ text name
        ]

tabsView : Model -> Html Msg
tabsView model =
    div [ css
          [ displayFlex
          , alignItems center
          ]
        ]
        [ tabView "Public" SwitchTab True
        , tabView "Private" SwitchTab False
        , tabView "Canvas" SwitchTab False
        ]

onlinePlayers : List Player -> Html Msg
onlinePlayers players =
    let
        avatar url =
            img [ css
                  [ borderRadius (pct 50)
                  , Css.height (Css.rem 2.133) -- 32px when base is 15px
                  , Css.width auto
                  , marginLeft (Css.em 0.5)
                  ]
                , src url
                ] []
    in
    div [ css
          [ displayFlex
          , alignItems center
          ]
        ]
    ( iconButton [] [ Icons.plusCircle ]
    :: List.map avatar
         [ "/lib/bird-avatar.jpg"
         , "/lib/cat-avatar.jpg"
         , "/lib/frog-avatar.jpg"
         , "/lib/fox-avatar-2.jpg"
         ]
    )

iconButton =
    styled button
        [ whiteSpace noWrap
        , padding2 (Css.em 0.45) (Css.em 0.4)
        , marginTop (Css.em 0.35)
        , backgroundColor transparent
        , border (px 0)
        , borderRadius (px 4)
        , color (hex "eee")
        , cursor pointer
        , hover
            [ backgroundColor (rgba 255 255 255 0.20)
            ]
        ]


------------------------------------------------------------
-- Game Settings Overlay
------------------------------------------------------------

overlay =
    styled div
        [ position fixed
        , top (px 0)
        , left (px 0)
        , Css.height (vh 100)
        , Css.width (vw 100)
        , Css.property "pointer-events" "all"
        , backgroundColor (hex "302633")
        , color (hex "eee")
        ]

overlayIconButton =
    styled button
        [ whiteSpace noWrap
        , padding2 (Css.em 0.45) (Css.em 0.4)
        , backgroundColor transparent
        , border (px 0)
        , borderRadius (px 4)
        , color (hex "eee")
        , cursor pointer
        , Css.float right
        , Css.property "transform" "translateY(-0.45rem)"
        , hover
            [ backgroundColor (rgba 255 255 255 0.20)
            ]
        ]

maybeOverlayView : Model -> Html Msg
maybeOverlayView model =
    case model.overlay of
        HideOverlay ->
            text ""
        ShowOverlay ->
            overlayView model

overlayView : Model -> Html Msg
overlayView model =
    overlay []
        [ div [ css
                [ Css.maxWidth (Css.rem 66)
                , margin3 (Css.rem 4.27) auto (px 0)
                ]
              ]
              [ overlayIconButton
                    [ onClick CloseOverlay ]
                    [ Icons.xCircleBig ]
              , div [ css
                      [ Css.property "display" "grid"
                      , Css.property "grid-template-columns" "13.33rem 1fr auto"
                      , Css.property "grid-column-gap" "4.27rem"
                      ]
                    ]
                    [ gameSettingsTabListView model
                    , div [ css [ overflowY scroll ] ]
                        [ case model.settingsTab of
                              ManagePlayersTab submodel ->
                                managePlayersView model submodel
                              
                              ChangeGameTitleTab ->
                                  text "Change game title"

                              LeaveGameTab ->
                                  text " Leave game"
                        ]
                    ]
              ]
        ]

------------------------------------------------------------
-- Game Settings Tabs
------------------------------------------------------------

gameSettingsTabView : String -> Msg -> Bool -> Html Msg
gameSettingsTabView name handleClick isActive =
    div [ css
          [ if isActive then
                Css.batch
                    [ backgroundColor (rgba 255 255 255 0.2)
                    , color (hex "fff")
                    ]
            else
                Css.batch
                    [ color (rgba 255 255 255 0.70)
                    , hover
                        [ color (hex "fff") ]
                    ]
          , borderRadius (px 4)
          , padding2 (Css.em 0.4) (Css.em 0.7)
          , cursor pointer
          ]
        , onClick handleClick
        ]
        [ text name
        ]

gameSettingsTabListView : { r | settingsTab : SettingsTab } -> Html Msg
gameSettingsTabListView { settingsTab } =
    let
        tab title ctor =
            gameSettingsTabView
            title
            (SwitchSettingsTab ctor)
            (settingsTab == ctor)
    in
    div [ css
          [ displayFlex
          , flexDirection column
          ]
        ]
        [ gameSettingsTabView "Manage Players"
              (SwitchSettingsTab (ManagePlayersTab defaultAddPlayerModel))
              (case settingsTab of
                   ManagePlayersTab _ ->
                       True

                   _ ->
                       False
              )
        , tab "Change Game Title" ChangeGameTitleTab
        , gameSettingsTabView "Quit To Lobby" ExitToLobby False
        , tab "Leave Game" LeaveGameTab
        ]

------------------------------------------------------------
-- Change Game Title
------------------------------------------------------------

changeGameTitleView : { r | title : String } -> Html Msg
changeGameTitleView { title } =
    div [ css
          [ Css.maxWidth (Css.rem 30) ]
        ]
        [ label [] [ text "Game Title" ]
        , input
              [ type_ "text"
              , onInput UpdateGameTitle
              , value title
              ]
              []
        , defaultButton
              [ onClick UpdateGameTitleInDB ]
              [ text "Done" ]
        ]

------------------------------------------------------------
-- Manage Players
------------------------------------------------------------

managePlayersView : { r | players : List Player }
                  -> ManagePlayersModel
                  -> Html Msg
managePlayersView { players } screen =
    case screen of
        AddPlayerScreen model ->
            div []
                [ playerListView players
                , addPlayerView
                ]
        RemovePlayerScreen model ->
            div []
                [ removePlayerView
                ]            

playerListView : List Player -> Html Msg
playerListView players =
    let
        rows = tbody []
            (players
            |> List.reverse
            |> List.map playerRow
            )
        styledTh =
            styled th
                [ borderBottom3 (px 1) solid (rgba 255 255 255 0.70) ]
        headers =
            tr [ css
                 [ fontSize (Css.em 0.9)
                 , textAlign left
                 , color (rgba 255 255 255 0.70)
                 ]
               ]
                [ styledTh [] [ text "Player" ]
                , styledTh [] [ text "Username" ]
                , styledTh [] [ text "Role"]
                , styledTh [] [ text ""]
                ]
    in
    div [ css
          [ Css.width (pct 100) ]
        ]
        [ Html.Styled.table
              [ css
                [ Css.width (pct 100)
                , borderSpacing2 (px 0) (Css.em 1)
                , Css.property "transform" "translateY(-1rem)"
                ]
              ]
              [ colgroup []
                    [ col [ css [ Css.width (pct 35)] ] []
                    , col [ css [ Css.width (pct 30)] ] []
                    , col [ css [ Css.width (pct 25)] ] []
                    , col [ css [ Css.width (pct 10)] ] []
                    ]
              , headers
              , rows
              ]
        ]

playerRow : Player -> Html Msg
playerRow { id, displayName, role } =
    tr [ css [ margin2 (Css.em 1) (Css.em 0) ] ]
        [ td
              [ css
                [ marginRight (Css.em 1)
                , paddingRight (Css.em 1)
                ]
              ]
              [ userBadge "Geronimo" "/lib/fox-avatar-2.jpg"]
        , td [ css [ paddingRight (Css.em 1) ] ]
            [ text displayName ]
        , td [ css [ paddingRight (Css.em 1) ] ]
            [ text (showRole role) ]
        , td [ css [ textAlign right ] ]
            [ darkButton
                  [ onClick (switchToRemovePlayer id) ]
                  [ text "Remove" ]
            ]
        ]

addPlayerView : Html Msg
addPlayerView =
    div [ css [ Css.maxWidth (Css.rem 20) ] ]
        [ label [ css
                  [ display block
                  , marginBottom (Css.rem 0.25)
                  ]
                ]
              [ text "Add a player by username" ]
        , input
              [ type_ "text"
              , css [ inputStyles ]
              -- , onInput UpdateNewPlayerFormInput
              -- , value username
              ]
              []
        , div [ css
                [ marginTop (Css.rem 1) ]
              ]
            [ addPlayerButton ]
        ]

removePlayerView : Html Msg
removePlayerView =
    div [ css [ Css.maxWidth (Css.rem 20) ] ]
        [ div [ css
                [ marginBottom (Css.rem 0.9) ]
              ]
              [ text "Remove this player from the game?" ]
        , userBadge "Geronimo" "/lib/fox-avatar-2.jpg"
        , label [ css
                  [ display block
                  , margin3 (Css.rem 1) (px 0) (Css.rem 0.5)
                  ]
                ]
              [ text "Type the player's name to confirm." ]
        , input
              [ type_ "text"
              , css [ inputStyles ]
              -- , onInput UpdateRemovePlayerFormInput
              -- , value username
              ]
              []
        , div [ css
                [ marginTop (Css.rem 1) ]
              ]
            [ div [ css
                    [ displayFlex
                    , alignItems center
                    ]
                  ]
                  [ darkButton
                        [ onClick switchToAddPlayer ]
                        [ text "Cancel" ]
                  , removePlayerButton
                  ]
            ]
        ]

addPlayerButton : Html Msg
addPlayerButton =
    button [ css
             [ whiteSpace noWrap
             , padding2 (Css.em 0.25) (Css.em 0.5)
             , backgroundColor (hex "0D8624")
             , border (px 0)
             , color (hex "eee")
             , borderRadius (px 4)
             , cursor pointer
             ]
           -- , onClick AddPlayer
           ]
    [ text "Add Player" ]

removePlayerButton : Html Msg
removePlayerButton =
    button [ css
             [ whiteSpace noWrap
             , padding2 (Css.em 0.25) (Css.em 0.5)
             , backgroundColor (rgba 237 66 66 0.75)
             , border (px 0)
             , color (hex "eee")
             , borderRadius (px 4)
             , cursor pointer
             , marginLeft (Css.rem 1)
             ]
           -- , onClick RemovePlayer
           ]
    [ text "Remove Player" ]


darkButton =
    styled button
        [ whiteSpace noWrap
        , padding2 (Css.em 0.1) (Css.em 0.5)
        , backgroundColor transparent
        , border3 (px 1) solid (hex "eee")
        , color (hex "eee")
        , borderRadius (px 4)
        , cursor pointer
        , hover
            [ backgroundColor (hex "eee")
            , color (hex "302633")
            ]
        ]


------------------------------------------------------------
-- Side Menu
------------------------------------------------------------

sidebar : Maybe Chat.Room -> Html Msg
sidebar mChatRoom =
    div [ css
          [ Css.property "grid-row" "2"
          , Css.property "grid-column" "2 / span 1"
          ]

        ]
    [ Icons.diceDefs
    , case mChatRoom of
          Just chatRoom ->
              Html.Styled.map ChatMsg (Chat.compactRoomView chatRoom)
          Nothing ->
              text ""
    ]

jumpToBottom : String -> Cmd Msg
jumpToBottom id =
  Dom.getViewportOf id
    |> Task.andThen
       (\info ->
            -- if Debug.log "viewport difference" (info.scene.height
            --     - (info.viewport.y
            --        + info.viewport.height)) < 300
            -- then
                Dom.setViewportOf id 0 info.scene.height
            -- else
            --     Task.succeed ()
       )
    |> Task.attempt (\_ -> NoOp)


------------------------------------------------------------
-- PouchDB
------------------------------------------------------------

maybeWriteToPouchDB : Msg -> Model -> Cmd Msg
maybeWriteToPouchDB msg newGame =
    case msg of
        SheetsMsg (Sheets.SheetMsg sheetId _) ->
            debouncedWriteSheetToPouchDB
                sheetId
                { ref = newGame.ref
                , msheet = Dict.get sheetId newGame.sheets
                , gameType = newGame.gameType
                }

        -- Writing to pouchDB for adding and removing sheets is
        -- handled in Sheets.elm directly using ports

        SheetsMsg Sheets.SheetsOrderingUpdated ->
            writeGameToPouchDB newGame

        SheetsMsg Sheets.SheetPermissionsUpdated ->
            writeGameToPouchDB newGame

        PlayerRemovedSuccess ->
            writeGameToPouchDB newGame

        UpdateGameTitle _ ->
            debouncedWriteGameToPouchDB newGame

        _ ->
            Cmd.none

debouncedWriteSheetToPouchDB : SheetId
                             -> { ref : PouchDBRef
                                , msheet : Maybe SheetModel
                                , gameType : GameType
                                }
                             -> Cmd Msg
debouncedWriteSheetToPouchDB sheetId { ref, msheet, gameType } =
    case msheet of
        Nothing ->
            Cmd.none

        Just sheet ->
            toCmd (WriteSheetToPouchDB
                       ref
                       sheetId
                       gameType
                       sheet
                  |> provideInput
                  |> DebounceMsg
                  )


debouncedWriteGameToPouchDB : Model -> Cmd Msg
debouncedWriteGameToPouchDB { ref
                            , title
                            , gameType
                            , sheetsOrdering
                            , sheetPermissions
                            } =
    toCmd
    (WriteGameToPouchDB ref "game"
         { title = title
         , gameType = gameType
         , sheetsOrdering = sheetsOrdering
         , sheetPermissions = sheetPermissions
         }
    |> provideInput
    |> DebounceMsg
    )

writeGameToPouchDB : Model -> Cmd Msg
writeGameToPouchDB { ref
                   , title
                   , gameType
                   , sheetsOrdering
                   , sheetPermissions
                   } =
    toCmd
    (WriteGameToPouchDB ref "game"
         { title = title
         , gameType = gameType
         , sheetsOrdering = sheetsOrdering
         , sheetPermissions = sheetPermissions
         }
    )
