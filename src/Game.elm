module Game
    exposing
    ( subscriptions
    , update
    , view
    )

import Array exposing (Array)
import Game.Sheet as Sheet
import Css exposing (..)
import Game.Types exposing (..)
import Game.Sheet.Types as Sheet
import Game.Sheet as Sheet
import Html
import Html.Styled exposing (..)
import Html.Styled.Lazy exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Time
import Json.Decode
import PouchDB exposing (PouchDBRef)
import Game.Decode
    exposing
    ( decodeGameData
    , decodePlayerList
    , decodePlayerPresence
    , decodeChatMessageList
    , scrollDecoder
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
        , PouchDB.sse_chatMessageReceived
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
        SheetMsg index submsg ->
            case Array.get index model.sheets of
                Nothing ->
                    ( model, Cmd.none )

                Just sheet ->
                    let
                        ( updatedSheet, cmd ) =
                            Sheet.updateSheet submsg sheet
                    in
                    ( { model
                        | sheets =
                            Array.set
                                index
                                updatedSheet
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
                OpenOverlay
                (Task.succeed
                    (EditSheet (Array.length model.sheets))
                )
            )

        RemoveSheet index ->
            ( { model
                | sheets =
                    removeIndexFromArray index model.sheets
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

        OnScroll position ->
            ({ model | sheetsViewportX = toFloat position }
            , Cmd.none
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
        , lazy2 sheetsView viewportSize model
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
            , Css.property "grid-column" "1 / -1"
            ]
        ]
        [ div [ css [ displayFlex ] ]
              [ exitGameButton
              , onlinePlayers model.players
              ]
        , gameTitle model.title
        , buttons model.gameType
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

buttons : GameType -> Html Msg
buttons gameType =
    div [ css
          [ displayFlex
          , alignItems center
          , whiteSpace noWrap
          ]
        ]
    [ addNewSheetButton gameType
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


addNewSheetButton : GameType -> Html Msg
addNewSheetButton gameType =
    toolbarButton
        [ onClick (AddSheet (Sheet.blank gameType))
        , css [ marginLeft (Css.em 0.5) ]
        ]
        [ Icons.addCharacterSheet ]


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
-- Game Sheets
--------------------------------------------------

sheetsView : (Int, Int)
           -> {r |
               sheets : Array Sheet.SheetModel
             , sheetsViewportX : Float
             }
           -> Html Msg
sheetsView (viewportWidth, _) { sheets, sheetsViewportX } =
    let
        sheetWidth = 24 * 15
        minBound =
            Basics.max 0
                (floor (sheetsViewportX / sheetWidth) - 1)
        maxBound =
            ceiling
            (toFloat minBound +
                 (toFloat viewportWidth / sheetWidth) + 1)
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
        ]
        (Array.toList
             (Array.indexedMap
                  (sheetWrapper (Debug.log "Min/MaxBound" (minBound, maxBound)))
                      sheets))


editSheetView :
    Int
    -> Maybe Sheet.SheetModel
    -> Html Msg
editSheetView index mmodel =
    case mmodel of
        Nothing ->
            div [] [ text "Not Found" ]

        Just sheet ->
            div
                [ css
                    [ margin2 (Css.em 4) auto
                    , backgroundColor (hex "fff")
                    , padding (Css.em 2)
                    , Css.width (Css.em 32)
                    , borderRadius (Css.em 0.2)
                    ]
                ]
                [ editSheetToolbarView index
                , Html.Styled.map
                    (SheetMsg index)
                    (Sheet.editView sheet)
                ]


editSheetToolbarView : Int -> Html Msg
editSheetToolbarView index =
    div
        [ css
            [ displayFlex
            , alignItems center
            ]
        ]
        [ defaultButton
            [ onClick CloseOverlay ]
            [ text "Done" ]
        , defaultButton
            [ onClick (RemoveSheet index)
            , css
                [ backgroundColor (hex "ff0000")
                , color (hex "fff")
                , hover
                    [ backgroundColor (hex "ee0000") ]
                ]
            ]
            [ text "Delete" ]
        ]


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


sheetCard : Int -> Sheet.SheetModel -> Html Msg
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
                [ onClick (OpenOverlay (EditSheet index))
                , css
                    [ display block ]
                ]
                [ text "Edit" ]
            ]
        , Html.Styled.map
            (SheetMsg index)
            (Sheet.view sheet)
        ]


spacer : Html msg
spacer =
    div [] []


sheetWrapper : (Int, Int)
             -> Int
             -> Sheet.SheetModel
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
                
        EditSheet index ->
            overlay
            []
            [ editSheetView
                  index
                  (Array.get index model.sheets)
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
              text ("Something went wrong.")-- ++ Debug.toString err)
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

inputStyles =
    batch
        [ Css.width (pct 100)
        , border3 (px 1) solid (hex "888")
        , borderRadius (px 4)
        , padding (Css.em 0.25)
        , flex (int 1)
        ]


--------------------------------------------------
-- Side Menu
--------------------------------------------------

sidebar : Model -> Html Msg
sidebar model =
    div [ css
          [ backgroundColor (hex "ddd")
          , Css.property "height" "calc(100vh - 3.6rem)"
          , borderLeft3 (Css.em 0.15) solid (hex "aaa")
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
          , Css.property "height" "calc(100vh - 3.6rem)"
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

