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
    ( compactRoomView
    , update
    , subscriptions
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
import Json.Decode
import Util exposing (toCmd)
import Ports exposing (XMPPClient)
import Chat.Types exposing (..)
import Chat.Decode exposing (decodeStanza)
import Chat.Encode exposing (encodeMessage)
import Chat.XMPP as XMPP
import Icons

subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Ports.xmpp_received (StanzaReceived << decodeStanza)
        , Ports.xmpp_connected (always ClientConnected)
        ]

update : XMPPClient -> Msg -> Model r -> (Model r, Cmd Msg)
update xmppClient msg model =
    case msg of
        ClientConnected ->
            (model, Cmd.none)

        StanzaReceived (MessageStanza message) ->
            let _ = Debug.log "Message Received" message
                _ = Debug.log "Current Rooms" model.rooms
            in
            ({ model
                 | rooms =
                     Dict.update
                     message.from.bare
                     (updateMessages message)
                     model.rooms
             }
            , jumpToBottom "chat-message-list"
            )

        StanzaReceived (PresenceStanza (from, presence)) ->
            (model
            -- ({ model
            --      | rooms =
            --          Dict.update
            --          from.bare
            --          (updateRoster (updatePresence from presence))
            --          model.rooms
            --  }
            , Cmd.none
            )

        StanzaReceived (StanzaDecodeError e) ->
            let _ = Debug.log "Decoding Error" e in
            (model, Cmd.none)

        UpdateChatInput id s ->
            ({ model
                 | rooms = Dict.update id (updateChatInput s) model.rooms }
            , Cmd.none)

        SendMessage newMessage ->
            ( model
            , Cmd.batch
                [ toCmd (resetChatInput newMessage.to)
                , XMPP.sendMessage
                      xmppClient
                      newMessage
                ]
            )

        EnterKeyPressed to input ->
            let
                message = String.trim input
            in
                if
                    String.length message < 1
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
                            ( model
                            , toCmd <| SendMessage (NewMessage to message)
                            )

        NoOp ->
            (model, Cmd.none)

        JoinRoom id ->
            ( model
            , XMPP.joinRoom
                xmppClient
                { room = id
                , displayName = model.me.displayName
                }
            )

        LeaveRoom id ->
            ( model
            , XMPP.leaveRoom
                xmppClient
                { room = id
                , displayName = model.me.displayName
                }
            )

        -- DiceRollResult rollResult ->
        --     ( model
        --     , Task.perform
        --         SendChatMessage
        --         (Task.succeed
        --              (NewDiceRollMessage rollResult))
        --     )


resetChatInput : BareJID -> Msg
resetChatInput id = UpdateChatInput id ""

-- updateRoster : Dict PersonId Person -> Maybe Person -> Maybe Person
-- updateRoster roster mPerson =
--     case mPerson of
--         Just person -> Just { person | presence = presence }
--         Nothing -> Nothing

updatePresence : Presence -> Maybe Person -> Maybe Person
updatePresence presence mPerson =
    case mPerson of
        Just person -> Just { person | presence = presence }
        Nothing -> Nothing


updateMessages : Message -> Maybe Room -> Maybe Room
updateMessages message mRoom =
    case mRoom of
        Just room ->
            Just { room | messages = message :: room.messages }

        Nothing -> Nothing

updateChatInput : String -> Maybe Room -> Maybe Room
updateChatInput s mRoom =
    case mRoom of
        Just room ->
            Just { room | input = s }

        -- Just (Conversation conversation) ->
        --     Conversation { conversation | input = s }

        Nothing -> Nothing


compactRoomView : Room -> Html Msg
compactRoomView model =
    div [ css
          [ -- Css.property "display" "grid"
          -- , Css.property "grid-template-rows" "1fr auto"
          -- , Css.height (vh 100)
          Css.property "height" "calc(100vh - 3rem)"
          , backgroundColor (hex "302633")
          -- , padding2 (px 0) (Css.em 0.8)
          , displayFlex
          , flexDirection column
          , justifyContent spaceBetween
          , color (hex "eee")
          ]
        ]
    -- (case mmodel of
    --     Nothing ->
    --         []
    --     Just model ->
            [ toolbarView
            , lazy messageLogView model.messages
            , lazy2 inputView model.id model.input
            ]--)

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

toolbarView : Html Msg
toolbarView =
    div [ css
          [ flex (int 0)
          , displayFlex
          , alignItems center
          , justifyContent spaceBetween
          -- , paddingTop (Css.em 0.75)
          , paddingLeft (Css.em 1)
          , paddingRight (Css.em 0.25)
          ]
        ]
        [ roomTitleView "Call from the Deep"
        , div [ css
                [ displayFlex
                , alignItems center
                ]
              ]
            [ iconButton [] [ Icons.headphones ]
            , iconButton [] [ Icons.mic ]
            , iconButton [] [ Icons.phone ]
            , iconButton [] [ Icons.settings ]
            ]
        ]

roomTitleView : String -> Html Msg
roomTitleView s =
    div [] [ text s ]

inputView : RoomId -> String -> Html Msg
inputView to message =
    div [ css
            [ padding (Css.em 0.8)
            ]
          ]
        [ textarea
              [ css
                [ resize none
                , Css.width (pct 100)
                , border (px 0)
                , borderRadius (Css.em 0.35)
                , padding2 (Css.em 0.5) (Css.em 0.8)
                , backgroundColor (hex "E4D6E3")
                ]
              , rows 1
              , placeholder "Send a message or roll dice"
              , onInput (UpdateChatInput to)
              , onEnter (EnterKeyPressed to message)
              , value message
              ]
              []
        ]

messageLogView : List Message -> Html Msg
messageLogView messages =
    let
        testMessages =
            [{ timestamp = Time.millisToPosix 0
             , from = { full = "Geronimo", bare = "Geronimo", resource = "" }
             , body = """Welcome to the game. Make yourself at home. I have created character sheets which have been assigned to each of you already. Just click on the "edit" button to make any changes."""
             }
            ]
        body =
            -- case messages of
            --     [] ->
            --         [ text "You can chat with the other players here and roll your dice."
            --         ]
            --     _ ->
                    List.map
                        (lazy messageView)
                        (List.reverse testMessages)
    in
        div [ css
              [ overflowY auto
              , overflowX Css.hidden
              , padding2 (Css.em 1) (Css.em 1)
              , flex (int 1)
              , fontWeight (int 200)
              , lineHeight (num 1.25)
              ]
            , id "chat-message-list"
            ]
        body

styledMessage =
    styled div
        [ padding2 (Css.em 0.5) (px 0)
        , color (hex "eee")
        , displayFlex
        ]

messageView : Message -> Html Msg
messageView { from, body, timestamp } =
    -- case message of
    --     Message { playerName, body } ->
            styledMessage
            []
            [ img [ css
                    [ borderRadius (pct 50)
                    , Css.height (px 32)
                    , Css.width auto
                    , marginRight (Css.em 0.5)
                    ]
                  , src "/lib/fox-avatar-2.jpg"
                  ] []
            , div []
                  [ div [ css
                          [ color (hex "FF7E1C")
                          , fontWeight (int 300)
                          , marginBottom (Css.em 0.3)
                          ]
                        ]
                        [ text from.full ]
                  , div [] [ text body ]
                  ]
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

