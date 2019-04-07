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

module Chat exposing
    ( view
    , update
    , closeConnection
    , connectClient
    , joinRoom
    )


import Array exposing (Array)
import List.Extra as List
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Lazy exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Task
import Browser.Dom as Dom
import Time
import Ports exposing (XMPPClientRef)
import Json.Decode

import Chat.Types exposing (..)
import Chat.Decode exposing (decodeStanza)
import Chat.Encode
    exposing
    ( encodeMessage
    , encodeConnectionInfo
    , encodeRoomConn
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClientConnected ref ->
            ({ model | ref = ref, connected = True }
            , Cmd.none
            )

        StanzaReceived (StanzaMessage message) ->
            let _ = Debug.log "Message Received" message in
            ({ model | messages = message :: model.messages }
            , jumpToBottom "chat-message-list"
            )

        StanzaReceived (StanzaPresence { from, presence }) ->
            case presence of
                Online ->
                    ({ model
                         | roster
                           = Dict.insert from.full from model.roster }
                    , Cmd.none
                    )
                Offline ->
                    ({ model
                         | roster
                           = Dict.remove from.full model.roster }
                    , Cmd.none
                    )

        StanzaReceived (StanzaDecodeError e) ->
            let _ = Debug.log "Decoding Error" e in
            (model, Cmd.none)

        UpdateChatInput s ->
            ({ model | input = s }, Cmd.none)

        ResetChatInput ->
            ({ model | input = "" }, Cmd.none)

        SendMessage newMessage ->
            ( model
            , Cmd.batch
                [ Task.perform
                      identity
                      (Task.succeed ResetChatInput)
                , Ports.xmpp_send (model.ref, encodeMessage newMessage)
                ]
            )

        KeyPressChatInput ->            
            let
                rawMessage = String.trim model.input
            in
                if String.length rawMessage < 1
                then
                    (model, Cmd.none)
                else
                    -- case Chat.isDiceRollRequest rawMessage of
                    --     Just roll_ ->
                    --         case Chat.parseDiceRollRequest roll_ of
                    --             Ok rollRequest ->
                    --                 ( model
                    --                 , DiceRoller.roll rollRequest
                    --                 )
                    --             Err err ->
                    --                 -- let
                    --                 --     _ = Debug.log "Error" err
                    --                 -- in
                    --                     (model, Cmd.none)
                            
                    --     Nothing ->
                            (model
                            , Task.succeed (NewMessage model.room rawMessage)
                                |> Task.perform SendMessage
                            )

        NoOp ->
            (model, Cmd.none)

        LeaveCurrentRoom ->
            ({ model
                 | messages = []
                 , input = ""
                 , roster = Dict.empty
             }
            , leaveRoom
                model.ref
                { room = model.room.bare
                , username = model.me.resource
                }
            )

        -- DiceRollResult rollResult ->
        --     ( model
        --     , Task.perform
        --         SendChatMessage
        --         (Task.succeed
        --              (NewDiceRollMessage rollResult))
        --     )

view : Model -> Html Msg
view model =
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
    -- (case mmodel of
    --     Nothing ->
    --         []
    --     Just model ->
            [ lazy messageLogView model.messages
            , lazy inputView model.input
            ]--)

inputView : String -> Html Msg
inputView message =
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

messageLogView : List Message -> Html Msg
messageLogView messages =
    let
        body =
            case messages of
                [] ->
                    [ messageView <|
                          { timestamp = Time.millisToPosix 0
                          , from = { full = "helpbot", bare = "helpbot", resource = "" }
                          , body = "You can chat with the other players here and roll your dice."
                          }
                    ]
                _ ->
                    List.map
                        (lazy messageView)
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

styledMessage =
    styled div
        [ backgroundColor (hex "eee")
        , padding (Css.em 0.5)
        , borderRadius (Css.em 0.35)
        , marginBottom (Css.em 0.8)
        ]

messageView : Message -> Html Msg
messageView { from, body, timestamp } =
    -- case message of
    --     Message { playerName, body } ->
            styledMessage
            []
            [ div [] [ text from.full ]
            , div [] [ text body ]
            ]
        -- DiceRollMessage { playerName, result } ->
        --     let
        --         (DiceRoll { request }) = result
        --     in
        --         styledChatMessage
        --         []
        --         [ div [] [ text (playerName ++ " rolled " ++ request) ]
        --         , showDiceRoll result
        --         ]


-- showDiceRoll : DiceRoll -> Html msg
-- showDiceRoll (DiceRoll roll) =
--     div [ css
--           [ displayFlex
--           , alignItems center
--           , lineHeight (num 1)
--           , fontSize (Css.em 1.4)
--           , flexWrap Css.wrap
--           ]
--         ]
--     [ span [ css
--              [ displayFlex
--              , margin2 (px 0) (Css.em 0.25)
--              ]
--            ]
--           (case roll.type_ of
--                DFate ->
--                    roll.results
--                        |> List.map showDiceResult
--                        |> List.concat
--                _ ->
--                    roll.results
--                        |> List.map showDiceResult
--                        |> List.intercalate [ text ", " ]
--           )
--     , case roll.modifier of
--           Nothing ->
--               text ""
--           Just modifier ->
--               let
--                   output =
--                       if modifier < 0
--                       then
--                           "(-" ++ String.fromInt (abs modifier) ++ ")"
--                       else
--                           "(+" ++ String.fromInt modifier ++ ")"
--               in
--                   span [ css
--                          [ margin2 (px 0) (Css.em 0.25)
--                          ]
--                        ]
--                       [ text output ]
--     , if List.length roll.results < 2
--         && roll.modifier == Nothing
--         && roll.type_ /= DFate
--       then
--           text ""
--       else
--           span [ css
--                  [ margin2 (px 0) (Css.em 0.25)
--                  ]
--                ]
--               [ text ("= " ++ String.fromInt roll.total) ]
--     ]

-- showDiceResult : DiceResult -> List (Html msg)
-- showDiceResult result =
--     case result of
--         DFateResult face ->
--             [ showDFateFace face ]

--         D20Result face ->
--             [text (String.fromInt face)]

--         D6Result face ->
--             [text (String.fromInt face)]

--         DOtherResult _ face ->
--             [text (String.fromInt face)]
            

-- showDFateFace : DFateFace -> Html msg
-- showDFateFace face =
--     span [ css
--            [ marginRight (Css.em 0.15) ]
--          ]
--     [ case face of
--           DFatePlus ->
--               Icons.dFatePlus
--           DFateBlank ->
--               Icons.dFateBlank
--           DFateMinus ->
--               Icons.dFateMinus
--     ]

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


closeConnection : Model -> Cmd msg
closeConnection model =
    -- case mmodel of
    --     Nothing -> Cmd.none
    --     Just model -> 
            Ports.closeChatClient model.ref

connectClient : ConnectionInfo -> Cmd msg
connectClient config =
    Ports.connectChatClient (encodeConnectionInfo config)

joinRoom : XMPPClientRef -> RoomConn -> Cmd msg
joinRoom ref roomConn =
    Ports.joinRoom (ref, encodeRoomConn roomConn)

leaveRoom : XMPPClientRef -> RoomConn -> Cmd msg
leaveRoom ref roomConn =
    Ports.leaveRoom (ref, encodeRoomConn roomConn)
