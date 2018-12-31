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

module Game
    exposing
    ( subscriptions
    , update
    , view
    )

import Array exposing (Array)
import Dict
import Game.Sheet as Sheet
import Css exposing (..)
import Game.Types exposing (..)
import Game.GameType exposing (..)
import Game.Person exposing (..)
import Game.Sheets.Types as Sheets
import Game.Sheets as Sheets
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
    , decodePlayerPresence
    , decodeChatMessageList
    , scrollDecoder
    , decodeSheetUpdate
    , decodeChanges
    )
import Task
import Util exposing (removeIndexFromArray)
import Browser.Navigation as Navigation
import Route
import RemoteData exposing (WebData)
import API
import Icons
import List.Extra as List
import Chat.Parser as Chat
import Chat.DiceRoller as DiceRoller
import Browser.Dom as Dom
import Http


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.changesReceived ChangesReceived
        , Ports.sse_playerListUpdated
            (ServerEventReceived
                 << PlayerListUpdated
                 << decodePlayerList)
        , Ports.sse_playerPresenceUpdated
            (ServerEventReceived
                 << PlayerPresenceUpdated
                 << decodePlayerPresence)
        , Ports.sse_chatMessageReceived
            (ServerEventReceived
                 << ChatMessagesReceived
                 << decodeChatMessageList)
        , Time.every 45000 (always Ping)
        ]


update : Navigation.Key
       -> Msg
       -> Model
       -> ( Model, Cmd Msg )
update navkey msg model =
    case msg of
        SheetsMsg (Sheets.OpenSheetPermissions sheetId) ->
            ( model
            , Task.perform
                identity
                (Task.succeed
                     (OpenOverlay
                          (ManageSheetPermissions sheetId)))
            )

        SheetsMsg submsg ->
            let
                ( newModel, cmd ) =
                    Sheets.update submsg model
            in
                ( newModel
                , Cmd.map SheetsMsg cmd
                )

        UpdateGameTitle title ->
            ( { model | title = title }
            , Cmd.none
            )

        UpdateGameTitleInDB ->
            ( model
            , Cmd.batch
                [ API.updateGameTitle model.id model.title
                , Task.perform identity (Task.succeed CloseOverlay)
                ]
            )

        GameTitleUpdated ->
            (model, Cmd.none)

        OpenOverlay overlay_ ->
            ( { model | overlay = overlay_ }, Cmd.none )

        CloseOverlay ->
            ( { model | overlay = OverlayNone }, Cmd.none )

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
                    -- ( model, Ports.get (model.ref, docId) )

                Err err ->
                    let _ = Debug.log "Changes Received Error" err
                    in
                    ( model, Cmd.none )

        ExitToLobby ->
            ( model
            , Cmd.batch
                [ API.setPresenceOffline model.id
                , Ports.closeEventStream model.ref
                , Navigation.replaceUrl
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

        RemovePlayer playerId ->
            ( model, API.removePlayer model.id playerId )

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
                    , Task.perform
                        identity
                        (Task.succeed PlayerRemovedSuccess)
                    )
                Err _ ->
                    ( model, Cmd.none )

        PlayerRemovedSuccess ->
            -- This message is only to let Main.elm know to write the
            -- changes to pouchDB
            (model, Cmd.none)

        ServerEventReceived (PlayerListUpdated ePlayers) ->
            case ePlayers of
                Ok players ->
                    ({ model | players = players }
                    , Cmd.none
                    )
                Err err ->
                    (model, Cmd.none)

        ServerEventReceived (PlayerPresenceUpdated ePresence) ->
            case ePresence of
                Ok presenceList ->
                    ({ model
                         | players
                             = updatePlayerPresenceList
                               presenceList
                               model.players
                     }
                    , Cmd.none
                    )
                Err err ->
                    (model, Cmd.none)

        ServerEventReceived (ChatMessagesReceived eMessages) ->
            case eMessages of
                Ok messages ->
                    ({ model
                         | chatMessages
                             = messages ++ model.chatMessages
                     }
                    , jumpToBottom "chat-message-list"
                    )
                Err err ->
                    (model, Cmd.none)

        Ping ->
            (model, API.ping model.id)

        Pong ->
            (model, Cmd.none)

        NoOp ->
            (model, Cmd.none)

        UpdateChatInput message ->
            ({ model | chatInput = message }
            , Cmd.none
            )

        ResetChatInput ->
            ({ model | chatInput = "" }
            , Cmd.none
            )

        SendChatMessage chatMessage ->
            ( model
            , Cmd.batch
                [ Task.perform
                      identity
                      (Task.succeed ResetChatInput)
                , API.sendChatMessage model.id chatMessage
                ]
            )

        ChatLogReceived result ->
            case result of
                Ok messages ->
                    ({ model | chatMessages = messages }
                    , jumpToBottom "chat-message-list" )

                Err err ->
                    -- let _ = Debug.log "Chat" err
                    -- in
                        (model, Cmd.none)

        KeyPressChatInput ->
            let
                rawMessage = String.trim model.chatInput
            in
                if String.length rawMessage < 1
                then
                    (model, Cmd.none)
                else
                    case Chat.isDiceRollRequest rawMessage of
                        Just roll_ ->
                            case Chat.parseDiceRollRequest roll_ of
                                Ok rollRequest ->
                                    ( model
                                    , DiceRoller.roll rollRequest
                                    )
                                Err err ->
                                    -- let
                                    --     _ = Debug.log "Error" err
                                    -- in
                                        (model, Cmd.none)
                            
                        Nothing ->
                            (model
                            , Task.perform
                                SendChatMessage
                                (Task.succeed
                                     (NewChatMessage rawMessage))
                            )

        DiceRollResult rollResult ->
            ( model
            , Task.perform
                SendChatMessage
                (Task.succeed
                     (NewDiceRollMessage rollResult))
            )


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


view : (Int, Int) -> Model -> Html Msg
view viewportSize model =
    div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-rows" "3rem auto"
            , Css.property "grid-template-columns" "1fr 23em"
            , Css.property "grid-row-gap" "0.6rem"
            , backgroundColor (hex "0079bf")
            ]
        ]
        [ lazy topToolbar model
        , Sheets.view viewportSize model
            |> Html.Styled.map SheetsMsg
        , lazy sidebar model
        , lazy overlayView model
        ]


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
            , Css.property "grid-column" "1 / 2"
            ]
        ]
        [ buttons model.myPlayerInfo
        , gameTitle model.title
        , div [ css [ displayFlex ] ]
            [ onlinePlayers model.players
            ]
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
                  , css
                        [ lineHeight (num 1.6)
                        ]
                  ]
    [ text "Exit Game" ]

buttons : Person -> Html Msg
buttons player =
    div [ css
          [ displayFlex
          , alignItems center
          , whiteSpace noWrap
          ]
        ]
    (case player.accessLevel of
         Player ->
             [ exitGameButton ]
         _ ->
             [ exitGameButton
             , invitePlayerButton
             , gameSettingsButton
             , showPlayerListButton
             ]
    )

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

gameSettingsButton : Html Msg
gameSettingsButton =
    toolbarButton
        [ onClick (OpenOverlay EditGameSettings)
        , css [ marginLeft (Css.em 0.5) ]
        ]
        [ Icons.gameSettings ]


invitePlayerButton : Html Msg
invitePlayerButton =
    toolbarButton
        [ onClick CreateInvite
        , css [ marginLeft (Css.em 0.5) ]
        ]
        [ Icons.instantInvite ]


showPlayerListButton : Html Msg
showPlayerListButton =
    toolbarButton
        [ onClick (OpenOverlay ManagePlayers)
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


onlinePlayers : List Person -> Html Msg
onlinePlayers players =
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
        (players
             |> List.filter
                (\{ presence } -> presence == Online )
             |> List.map
                (\{ username, id } ->
                     avatar
                     (String.left 1 username)
                     (chooseColor id))
        )

-- popOverView : { id : Int
--               , open : msg
--               , close : msg
--               , popoverState : PopOver
--               , openTrigger : String
--               , closeTrigger : String
--               , menu : Html msg
--               }
--             -> Html msg
-- popOverView { id
--             , open
--             , close
--             , popoverState
--             , openTrigger
--             , closeTrigger
--             , menu
--             } =
--     div [ css
--           [ display inlineFlex
--           , position relative
--           , verticalAlign top
--           ]
--         ]
--         [ div [ ]
--               [ case popoverState of
--                     PopOverNone ->
--                         defaultButton
--                         [ onClick open ]
--                         [ text openTrigger ]
--                     PopOver id_ ->
--                         if id_ == id
--                         then
--                             defaultButton
--                             [ onClick close ]
--                             [ text closeTrigger ]
--                         else
--                             defaultButton
--                             [ onClick open ]
--                             [ text openTrigger ]
--               ]
--         , div [ css
--                 [ left (px 0)
--                 , position absolute
--                 , top (pct 100)
--                 , zIndex (int 20)
--                 , Css.width (Css.em 12)
--                 , backgroundColor (hex "eee")
--                 ]
--               ]
--             [ case popoverState of
--                     PopOverNone ->
--                         text ""
--                     PopOver id_ ->
--                         if id_ == id
--                         then
--                             menu
--                         else
--                             text ""
--             ]
--         ]






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

        EditGameSettings ->
            overlay [] [ gameSettingsView model ]

        InstantInvite mInvite ->
            overlay [] [ instantInviteView mInvite ]

        ManagePlayers ->
            overlay [] [ playerListView model.players ]

        ManageSheetPermissions sheetId ->
            overlay []
                [ div [ css
                        [ margin2 (Css.em 4) auto
                        , backgroundColor (hex "fff")
                        , padding (Css.em 2)
                        , Css.width (Css.em 32)
                        , borderRadius (Css.em 0.2)
                        ]
                      ]
                      [ h1 [] [ text "Players who can edit this sheet" ]
                      , Sheets.sheetPermissionsView sheetId model
                            |> Html.Styled.map SheetsMsg
                      , defaultButton
                            [ onClick CloseOverlay ]
                            [ text "Done" ]
                      ]
                ]


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
            [ onClick UpdateGameTitleInDB ]
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
                              ("https://localhost:4430" ++
                                   Route.toUrlString
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

playerListView : List Person -> Html Msg
playerListView players =
    div [ css
          [ margin2 (Css.em 4) auto
          , backgroundColor (hex "fff")
          , padding (Css.em 2)
          , Css.width (Css.em 32)
          , borderRadius (Css.em 0.2)
          ]
        ]
    [ div []
          [ h2 [ css [ marginTop (px 0) ] ]
                [ text "Manage Players" ]
          , div [] (List.map playerListItemView players)
          , defaultButton
                [ onClick CloseOverlay ]
                [ text "Close" ]
          ]
    ]


playerListItemView : Person -> Html Msg
playerListItemView player =
    div [ css
          [ displayFlex
          , alignItems center
          , justifyContent spaceBetween
          ]
        , class "reveal-buttons-on-hover"
        ]
        [ div [ css
                [ flex2 (int 1) (int 1)
                , case player.presence of
                      Online ->
                          Css.batch []
                      Offline ->
                          opacity (num 0.6)
                ]
              ]
              [ text player.username
              , case player.presence of
                    Online ->
                        presenceIndicator "19b419" "online"
                    Offline ->
                        presenceIndicator "d71b1b" "offline"
              ]
        , defaultButton
              [ onClick (RemovePlayer player.id)
              , css
                [ backgroundColor (hex "ff0000")
                , color (hex "fff")
                , opacity (int 0)
                , hover
                    [ backgroundColor (hex "ee0000") ]
                ]
              ]
              [ text "Remove" ]
        ]


presenceIndicator : String -> String -> Html msg
presenceIndicator color status =
    span [ css
           [ borderRadius (Css.em 0.25)
           , backgroundColor (hex color)
           , Css.color (hex "fff")
           , margin2 (Css.em 0) (Css.em 0.5)
           , padding2 (px 0) (Css.em 0.25)
           ]
         ]
        [ text status ]


--------------------------------------------------
-- Side Menu
--------------------------------------------------

sidebar : Model -> Html Msg
sidebar model =
    div [ css
          [ backgroundColor (hex "ddd")
          , Css.height (vh 100)
          , borderLeft3 (Css.em 0.15) solid (hex "aaa")
          , Css.property "grid-row" "1 / span 2"
          , Css.property "grid-column" "2 / span 1"
          ]

        ]
    [ Icons.diceDefs
    , chatView model
    ]

chatView : Model -> Html Msg
chatView model =
    div [ css
          [ Css.property "display" "grid"
          , Css.property "grid-template-rows" "1fr auto"
          , Css.height (vh 100)
          , padding2 (px 0) (Css.em 0.8)
          , displayFlex
          , flexDirection column
          , justifyContent spaceBetween
          , fontSize (Css.em 0.95)
          ]
        ]
    [ lazy chatMessageListView model.chatMessages
    , lazy chatInputView model.chatInput
    ]

chatInputView : String -> Html Msg
chatInputView message =
    div [ css
            [ padding2 (Css.em 0.8) (px 0)
            ]
          ]
        [ textarea
              [ css
                [ resize none
                , Css.width (pct 100)
                , border (px 0)
                , borderRadius (Css.em 0.35)
                , padding (Css.em 0.35)
                ]
              , rows 4
              , placeholder "Send a message or roll dice"
              , onInput UpdateChatInput
              , onEnter KeyPressChatInput
              , value message
              ]
              []
        ]

chatMessageListView : List ChatMessage -> Html Msg
chatMessageListView messages =
    let
        body =
            case messages of
                [] ->
                    [ chatMessageView <|
                      ChatMessage
                          { timestamp = Time.millisToPosix 0
                          , playerId = 0
                          , playerName = "Help"
                          , body = "You can chat with the other players here and roll your dice."
                          }
                    ]
                _ ->
                    List.map
                        (lazy chatMessageView)
                        (List.reverse messages)
    in
        div [ css
              [ overflowY auto
              , overflowX Css.hidden
              , padding2 (Css.em 0.8) (px 0)
              ]
            , id "chat-message-list"
            ]
        body

styledChatMessage =
    styled div
        [ backgroundColor (hex "eee")
        , padding (Css.em 0.5)
        , borderRadius (Css.em 0.35)
        , marginBottom (Css.em 0.8)
        ]

chatMessageView : ChatMessage -> Html Msg
chatMessageView message =
    case message of
        ChatMessage { playerName, body } ->
            styledChatMessage
            []
            [ div [] [ text playerName ]
            , div [] [ text body ]
            ]
        DiceRollMessage { playerName, result } ->
            let
                (DiceRoll { request }) = result
            in
                styledChatMessage
                []
                [ div [] [ text (playerName ++ " rolled " ++ request) ]
                , showDiceRoll result
                ]


showDiceRoll : DiceRoll -> Html msg
showDiceRoll (DiceRoll roll) =
    div [ css
          [ displayFlex
          , alignItems center
          , lineHeight (num 1)
          , fontSize (Css.em 1.4)
          , flexWrap Css.wrap
          ]
        ]
    [ span [ css
             [ displayFlex
             , margin2 (px 0) (Css.em 0.25)
             ]
           ]
          (case roll.type_ of
               DFate ->
                   roll.results
                       |> List.map showDiceResult
                       |> List.concat
               _ ->
                   roll.results
                       |> List.map showDiceResult
                       |> List.intercalate [ text ", " ]
          )
    , case roll.modifier of
          Nothing ->
              text ""
          Just modifier ->
              let
                  output =
                      if modifier < 0
                      then
                          "(-" ++ String.fromInt (abs modifier) ++ ")"
                      else
                          "(+" ++ String.fromInt modifier ++ ")"
              in
                  span [ css
                         [ margin2 (px 0) (Css.em 0.25)
                         ]
                       ]
                      [ text output ]
    , if List.length roll.results < 2
        && roll.modifier == Nothing
        && roll.type_ /= DFate
      then
          text ""
      else
          span [ css
                 [ margin2 (px 0) (Css.em 0.25)
                 ]
               ]
              [ text ("= " ++ String.fromInt roll.total) ]
    ]

showDiceResult : DiceResult -> List (Html msg)
showDiceResult result =
    case result of
        DFateResult face ->
            [ showDFateFace face ]

        D20Result face ->
            [text (String.fromInt face)]

        D6Result face ->
            [text (String.fromInt face)]

        DOtherResult _ face ->
            [text (String.fromInt face)]
            

showDFateFace : DFateFace -> Html msg
showDFateFace face =
    span [ css
           [ marginRight (Css.em 0.15) ]
         ]
    [ case face of
          DFatePlus ->
              Icons.dFatePlus
          DFateBlank ->
              Icons.dFateBlank
          DFateMinus ->
              Icons.dFateMinus
    ]

onEnter : msg -> Attribute msg
onEnter onEnterAction =
   on "keyup" <|
       Json.Decode.andThen
           (\keyCode ->
               case keyCode of
                   13 ->
                       Json.Decode.succeed onEnterAction

                   _ ->
                       Json.Decode.fail (String.fromInt keyCode)
           )
           keyCode


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

