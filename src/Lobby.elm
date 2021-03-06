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


module Lobby exposing
    ( init
    , update
    , view
    )

import API
import Array exposing (Array)
import Browser.Navigation as Navigation
import Css exposing (..)
import Dict exposing (Dict)
import Game.GameType as Game
import Game.Types as Game
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Http
import Icons
import Json.Encode exposing (Value)
import Lobby.Types exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Task
import Util exposing (toCmd)
import Util.Css exposing (..)


init : Cmd Msg
init =
    toCmd GetGameList


update : Navigation.Key -> Msg -> Model r -> ( Model r, Cmd Msg )
update navkey msg model =
    case msg of
        NewGame ->
            case model.newGameForm of
                NewGameForm { title, gameType } ->
                    if String.length title > 0 then
                        ( model
                        , API.newGame
                            (Game.emptyGameData title gameType)
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model
                    , Cmd.none
                    )

        NewGameResponse result ->
            case result of
                Success gameId ->
                    ( { model | newGameForm = NewGameFormNone }
                    , Navigation.replaceUrl
                        navkey
                        (Route.toUrlString (Route.Game gameId))
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateNewGameTitle title ->
            case model.newGameForm of
                NewGameForm settings ->
                    ( { model
                        | newGameForm =
                            NewGameForm
                                { settings | title = title }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateNewGameType gameType ->
            case model.newGameForm of
                NewGameForm settings ->
                    ( { model
                        | newGameForm =
                            NewGameForm
                                { settings | gameType = gameType }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GetGameList ->
            ( model
            , API.getAllGames
            )

        SetGameList result ->
            case result of
                Failure (Http.BadStatus resp) ->
                    case resp.status.code of
                        401 ->
                            ( model
                            , Navigation.replaceUrl
                                navkey
                                (Route.toUrlString Route.Auth)
                            )

                        _ ->
                            ( { model | games = result }
                            , Cmd.none
                            )

                _ ->
                    ( { model | games = result }
                    , Cmd.none
                    )

        LoadGame id ->
            ( model
            , Navigation.replaceUrl
                navkey
                (Route.toUrlString (Route.Game id))
            )

        Logout ->
            ( model
            , API.logout
            )

        LogoutResponse result ->
            case result of
                Ok _ ->
                    ( model
                    , Navigation.replaceUrl
                        navkey
                        (Route.toUrlString Route.Auth)
                    )

                Err status ->
                    ( model, Cmd.none )

        OpenNewGameForm ->
            ( { model | newGameForm = emptyNewGameForm }, Cmd.none )

        CloseNewGameForm ->
            ( { model | newGameForm = NewGameFormNone }, Cmd.none )

        SwitchTab tab ->
            ( { model | lobbyTab = tab }, Cmd.none )

        ResumeGame ->
            ( model, Cmd.none )

        SwitchSettingsTab tab ->
            ( { model | lobbyTab = SettingsTab tab }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        UpdateAccountDisplayName displayName ->
            updateEditAccountForm model
                (\form -> { form | displayName = displayName })

        UpdateAccountEmail email ->
            updateEditAccountForm model
                (\form -> { form | email = email })

        UpdateAccountCurrentPassword password ->
            updateEditAccountForm model
                (\form -> { form | password = password })

        UpdatePasswordNewPassword newPassword ->
            updateChangePasswordForm model
                (\form -> { form | newPassword = newPassword })

        UpdatePasswordCurrentPassword currentPassword ->
            updateChangePasswordForm model
                (\form -> { form | currentPassword = currentPassword })


updateEditAccountForm model updateForm =
    case model.lobbyTab of
        SettingsTab (AccountTab (EditAccount form)) ->
            ( { model
                | lobbyTab =
                    SettingsTab (AccountTab (EditAccount (updateForm form)))
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


updateChangePasswordForm model updateForm =
    case model.lobbyTab of
        SettingsTab (AccountTab (ChangePassword form)) ->
            ( { model
                | lobbyTab =
                    SettingsTab (AccountTab (ChangePassword (updateForm form)))
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



------------------------------------------------------------
-- View
------------------------------------------------------------


view : Bool -> Model r -> Html Msg
view hasActiveGame model =
    div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-rows" "3rem calc(100vh - 3rem)"
            , Css.property "grid-template-columns" "1fr"
            , backgroundColor (hex "302633")
            , Css.minHeight (vh 100)
            ]
        ]
        [ topNavigation hasActiveGame model
        , div
            [ css
                [ -- displayFlex
                  -- , justifyContent center
                  color (hex "fff")
                , padding3 (Css.em 3) (Css.em 1) (Css.em 0)
                , overflowY scroll
                ]
            ]
            [ case model.lobbyTab of
                GamesTab ->
                    maybeGameListView model

                InvitesTab ->
                    div
                        [ css
                            [ Css.maxWidth (Css.rem 54)
                            , margin3 (Css.rem 1) auto (px 0)
                            ]
                        ]
                        [ text "Invites" ]

                MessagesTab ->
                    div
                        [ css
                            [ Css.maxWidth (Css.rem 54)
                            , margin3 (Css.rem 1) auto (px 0)
                            ]
                        ]
                        [ text "Messages" ]

                SettingsTab submodel ->
                    settingsView submodel
            ]
        ]


defaultButton =
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


primaryButton bgcolor =
    styled button
        [ whiteSpace noWrap
        , padding2 (Css.em 0.1) (Css.em 0.5)
        , backgroundColor bgcolor
        , border (px 0)
        , color (hex "eee")
        , borderRadius (px 4)
        , cursor pointer
        ]


userBadge : Html Msg
userBadge =
    div
        [ css
            [ displayFlex
            , alignItems center
            ]
        ]
        [ img
            [ css
                [ borderRadius (pct 50)
                , Css.height (Css.rem 2.133) -- 32px when base is 15px
                , Css.width (Css.rem 2.133)
                ]
            , src "/lib/fox-avatar-2.jpg"
            ]
            []
        , span
            [ css
                [ marginLeft (Css.em 0.5)
                ]
            ]
            [ text "Geronimo" ]
        ]



------------------------------------------------------------
-- Navigation
------------------------------------------------------------


topNavigationSection =
    styled div
        [ displayFlex
        , flex (int 1)
        , justifyContent center
        ]


topNavigation : Bool -> Model r -> Html Msg
topNavigation hasActiveGame model =
    header
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent spaceBetween
            , backgroundColor transparent
            , color (hex "fff")
            , padding2 (Css.em 0) (Css.em 1)
            , Css.property "grid-column" "1 / 2"
            ]
        ]
        [ topNavigationSection []
            [ span [ css [ displayFlex, marginRight auto ] ]
                [ userBadge
                , if hasActiveGame then
                    defaultButton
                        [ css [ marginLeft (Css.em 1) ]
                        , onClick ResumeGame
                        ]
                        [ text "Resume Game" ]

                  else
                    text ""
                ]
            ]
        , topNavigationSection [] [ tabsView model.lobbyTab ]
        , topNavigationSection []
            [ span [ css [ marginLeft auto ] ] [] ]
        ]


tabView : String -> Msg -> Bool -> Html Msg
tabView name handleClick isActive =
    div
        [ css
            [ if isActive then
                Css.batch
                    [ backgroundColor (rgba 255 255 255 0.2)
                    , color (hex "fff")
                    ]

              else
                Css.batch
                    [ color (rgba 255 255 255 0.7)
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


tabsView : Tab -> Html Msg
tabsView tab =
    div
        [ css
            [ displayFlex
            , alignItems center
            ]
        ]
        [ tabView "Games" (SwitchTab GamesTab) (tab == GamesTab)
        , tabView "Invites" (SwitchTab InvitesTab) (tab == InvitesTab)
        , tabView "Messages" (SwitchTab MessagesTab) (tab == MessagesTab)
        , tabView "Settings"
            (SwitchTab (SettingsTab (AccountTab defaultAccountModel)))
            (case tab of
                SettingsTab _ ->
                    True

                _ ->
                    False
            )
        ]



------------------------------------------------------------
-- Settings
------------------------------------------------------------


settingsView : SettingsTab -> Html Msg
settingsView tab =
    div
        [ css
            [ Css.maxWidth (Css.rem 54)
            , margin3 (Css.rem 1) auto (px 0)
            ]
        ]
        [ div
            [ css
                [ Css.property "display" "grid"
                , Css.property "grid-template-columns" "13.33rem 1fr"
                , Css.property "grid-column-gap" "4.27rem"
                ]
            ]
            [ settingsTabListView tab
            , div []
                [ case tab of
                    AccountTab submodel ->
                        accountSettingsView submodel

                    AppearanceTab ->
                        text "Appearance"
                ]
            ]
        ]


settingsTabView : String -> Msg -> Bool -> Html Msg
settingsTabView name handleClick isActive =
    div
        [ css
            [ if isActive then
                Css.batch
                    [ backgroundColor (rgba 255 255 255 0.2)
                    , color (hex "fff")
                    ]

              else
                Css.batch
                    [ color (rgba 255 255 255 0.7)
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


settingsTabListView : SettingsTab -> Html Msg
settingsTabListView settingsTab =
    let
        tab title ctor =
            settingsTabView
                title
                (SwitchSettingsTab ctor)
                (settingsTab == ctor)
    in
    div
        [ css
            [ displayFlex
            , flexDirection column
            ]
        ]
        [ settingsTabView "My Account"
            (SwitchSettingsTab (AccountTab defaultAccountModel))
            (case settingsTab of
                AccountTab _ ->
                    True

                _ ->
                    False
            )
        , tab "Appearance" AppearanceTab
        , settingsTabView "Sign Out" Logout False
        ]



------------------------------------------------------------
-- Account Settings
------------------------------------------------------------


accountSectionLabel s =
    div
        [ css
            [ fontVariant allSmallCaps
            , fontSize (Css.rem 0.9)
            , color (rgba 255 255 255 0.8)
            ]
        ]
        [ text s ]


accountSectionValue s =
    div
        [ css
            [ fontSize (pct 120)
            , marginBottom (Css.rem 1)
            ]
        ]
        [ text s ]


accountSectionInput { currentValue, handler } =
    input
        [ type_ "text"
        , css
            [ inputStyles
            , backgroundColor (hex "E4D6E3")
            , color (hex "302633")
            , marginBottom (Css.rem 1)
            , Css.maxWidth (Css.rem 20)
            ]
        , onInput handler
        , value currentValue
        ]
        []


accountSettingsView : AccountModel -> Html Msg
accountSettingsView model =
    div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "auto 1fr"
            , Css.property "grid-column-gap" "2.2rem"

            -- , border3 (px 1) solid (hex "eee")
            ]
        ]
    <|
        case model of
            ShowAccount ->
                viewAccountSettings

            EditAccount submodel ->
                editAccountSettings submodel

            ChangePassword submodel ->
                changePasswordSettings submodel


viewAccountSettings : List (Html Msg)
viewAccountSettings =
    [ userAvatar
    , div []
        [ accountSectionLabel "Display Name"
        , accountSectionValue "Geronimo"
        , accountSectionLabel "Username"
        , accountSectionValue "geronimo"
        , accountSectionLabel "Email"
        , accountSectionValue "geronimo@roll2d6.org"
        , div []
            [ defaultButton
                [ onClick
                    (SwitchSettingsTab
                        (AccountTab
                            (EditAccount
                                { displayName = "Geronimo"
                                , username = "geronimo"
                                , email = "geronimo@roll2d6.org"
                                , password = ""
                                }
                            )
                        )
                    )
                ]
                [ text "Edit Account" ]
            , defaultButton
                [ css [ marginLeft (Css.rem 1) ]
                , onClick
                    (SwitchSettingsTab
                        (AccountTab
                            (ChangePassword
                                { displayName = "Geronimo"
                                , username = "geronimo"
                                , email = "geronimo@roll2d6.org"
                                , newPassword = ""
                                , currentPassword = ""
                                }
                            )
                        )
                    )
                ]
                [ text "Change Password" ]
            ]
        ]
    ]


editAccountSettings : EditAccountForm -> List (Html Msg)
editAccountSettings { displayName, username, email, password } =
    [ userAvatar
    , div []
        [ accountSectionLabel "Display Name"
        , accountSectionInput
            { currentValue = displayName
            , handler = UpdateAccountDisplayName
            }
        , accountSectionLabel "Username"
        , accountSectionValue username
        , accountSectionLabel "Email"
        , accountSectionInput
            { currentValue = email
            , handler = UpdateAccountEmail
            }
        , accountSectionLabel "Current Password (Required)"
        , accountSectionInput
            { currentValue = password
            , handler = UpdateAccountCurrentPassword
            }
        , div []
            [ defaultButton
                [ onClick (SwitchSettingsTab (AccountTab defaultAccountModel)) ]
                [ text "Cancel" ]
            , primaryButton (hex "0D8624")
                [ css [ marginLeft (Css.rem 1) ] ]
                [ text "Save Changes" ]
            ]
        ]
    ]


changePasswordSettings : ChangePasswordForm -> List (Html Msg)
changePasswordSettings { displayName, username, email, newPassword, currentPassword } =
    [ userAvatar
    , div []
        [ accountSectionLabel "Display Name"
        , accountSectionValue displayName
        , accountSectionLabel "Username"
        , accountSectionValue username
        , accountSectionLabel "Email"
        , accountSectionValue email
        , accountSectionLabel "New Password"
        , accountSectionInput
            { currentValue = newPassword
            , handler = UpdatePasswordNewPassword
            }
        , accountSectionLabel "Current Password (Required)"
        , accountSectionInput
            { currentValue = currentPassword
            , handler = UpdatePasswordCurrentPassword
            }
        , div []
            [ defaultButton
                [ onClick (SwitchSettingsTab (AccountTab defaultAccountModel)) ]
                [ text "Cancel" ]
            , primaryButton (hex "0D8624")
                [ css [ marginLeft (Css.rem 1) ] ]
                [ text "Change Password" ]
            ]
        ]
    ]


userAvatar : Html Msg
userAvatar =
    img
        [ css
            [ borderRadius (pct 50)
            , Css.height (Css.rem 6.67) -- 100px when base is 15px
            , Css.width (Css.rem 6.67)
            , backgroundColor (rgba 0 0 0 0.2)
            ]
        , src "/lib/fox-avatar-2.jpg"
        ]
        []



------------------------------------------------------------
-- Game List
------------------------------------------------------------


maybeGameListView :
    { r
        | newGameForm : NewGameForm
        , games : WebData (List Game.GameSummary)
    }
    -> Html Msg
maybeGameListView model =
    case model.games of
        NotAsked ->
            div [] [ text "Not Asked" ]

        Loading ->
            div [] [ text "Loading" ]

        Failure err ->
            div [] [ text "Request Failed" ]

        Success games ->
            gameListView model.newGameForm games


gameListView : NewGameForm -> List Game.GameSummary -> Html Msg
gameListView newGameForm games =
    let
        rows =
            tbody []
                (games
                    |> List.reverse
                    |> List.map gamePreview
                )

        styledTh =
            styled th
                [ borderBottom3 (px 1) solid (rgba 255 255 255 0.7) ]

        headers =
            tr
                [ css
                    [ fontSize (Css.em 0.9)
                    , textAlign left
                    , color (rgba 255 255 255 0.7)
                    ]
                ]
                [ styledTh [] [ text "Title" ]
                , styledTh [] [ text "Game System" ]
                , styledTh [] [ text "Owner" ]
                , styledTh [] [ text "" ]
                ]
    in
    div
        [ css
            [ Css.maxWidth (Css.em 54)
            , margin2 (px 0) auto
            ]
        ]
        [ Html.Styled.table
            [ css
                [ Css.width (pct 100)
                , borderSpacing2 (px 0) (Css.em 1)
                ]
            ]
            [ colgroup []
                [ col [ css [ Css.width (pct 40) ] ] []
                , col [ css [ Css.width (pct 25) ] ] []
                , col [ css [ Css.width (pct 25) ] ] []
                , col [ css [ Css.width (pct 10) ] ] []
                ]
            , headers
            , rows
            ]
        , newGameView newGameForm
        ]


gamePreview : Game.GameSummary -> Html Msg
gamePreview { id, title, gameType } =
    tr [ css [ margin2 (Css.em 1) (Css.em 0) ] ]
        [ td
            [ css
                [ marginRight (Css.em 1)
                , paddingRight (Css.em 1)
                ]
            ]
            [ text title ]
        , td [ css [ paddingRight (Css.em 1) ] ]
            [ text (Game.showGameType gameType) ]
        , td [ css [ paddingRight (Css.em 1) ] ]
            [ userBadge ]
        , td [ css [ textAlign right ] ]
            [ defaultButton
                [ onClick (LoadGame id) ]
                [ text "Play" ]
            ]
        ]



------------------------------------------------------------
-- New Game
------------------------------------------------------------


newGameButton =
    defaultButton
        [ type_ "button"
        , onClick OpenNewGameForm
        ]
        [ text "Create new game" ]


newGameView : NewGameForm -> Html Msg
newGameView newGameForm =
    case newGameForm of
        NewGameFormNone ->
            newGameButton

        NewGameForm newGameSettings ->
            div [] [ newGameFormView newGameSettings ]


newGameFormView : NewGameFormModel -> Html Msg
newGameFormView { title, gameType } =
    div
        [ css
            [ backgroundColor (rgba 255 255 255 0.2)
            , padding (Css.em 1)
            , borderRadius (Css.em 0.2)
            , displayFlex
            , alignItems end
            ]
        ]
        [ div
            [ css
                [ Css.width (pct 45) -- flex (int 1)
                , marginRight (Css.em 1)
                ]
            ]
            [ formInputLabel "Title"
            , input
                [ type_ "text"
                , css [ inputStyles ]
                , onInput UpdateNewGameTitle
                , value title
                ]
                []
            ]
        , div
            [ css
                [ Css.width (pct 30)
                , marginRight (Css.em 1)
                ]
            ]
            [ formInputLabel "Game System"
            , radioInputList
                [ css
                    [ overflowY scroll
                    , Css.height (px 26.5)
                    ]
                ]
                { toMsg = UpdateNewGameType
                , options = Game.gameTypeOptions
                , selected = gameType
                , showOption = Game.showGameType
                }
            ]
        , div
            [ css
                [ Css.width (pct 15)
                , textAlign right
                ]
            ]
            [ newGameFormSubmitButton ]
        , div
            [ css
                [ Css.width (pct 10)
                , textAlign right
                ]
            ]
            [ closeFormButton ]
        ]


newGameFormSubmitButton : Html Msg
newGameFormSubmitButton =
    button
        [ css
            [ whiteSpace noWrap
            , padding2 (Css.em 0.25) (Css.em 0.5)
            , backgroundColor (hex "0D8624")
            , border (px 0)
            , color (hex "eee")
            , borderRadius (px 4)
            , cursor pointer
            ]
        , onClick NewGame
        ]
        [ text "Create Game" ]


closeFormButton : Html Msg
closeFormButton =
    button
        [ css
            [ whiteSpace noWrap
            , padding (Css.em 0.5)
            , backgroundColor transparent
            , border (px 0)
            , color (hex "eee")
            , cursor pointer
            , Css.property "transform" "translate(1em, -1.5em)"

            -- , hover
            --       [ backgroundColor (hex "eee")
            --       , color (hex "302633")
            --       ]
            ]
        , onClick CloseNewGameForm
        ]
        [ Icons.xCircle ]


emptyNewGameForm : NewGameForm
emptyNewGameForm =
    NewGameForm
        { title = ""
        , gameType = Game.Fate
        }


inputStyles : Css.Style
inputStyles =
    batch
        [ Css.width (pct 100)
        , border (px 0)
        , borderRadius (px 4)
        , padding2 (Css.em 0.25) (Css.em 0.5)
        , flex (int 1)
        ]


formInputLabel : String -> Html msg
formInputLabel title =
    div
        [ css
            [ fontSize (Css.em 0.9)
            , color (hex "eee")
            ]
        ]
        [ text title ]


radioInputList :
    List (Attribute msg)
    ->
        { toMsg : a -> msg
        , options : List a
        , selected : a
        , showOption : a -> String
        }
    -> Html msg
radioInputList attrs { toMsg, options, selected, showOption } =
    let
        optionView option =
            label
                [ css
                    [ display block

                    -- , marginBottom (Css.em 0.25)
                    , fontSize (Css.em 1)
                    , padding2 (Css.em 0.1) (Css.em 0.6)

                    -- , borderRadius (Css.em 0.25)
                    , userSelect_none
                    , cursor pointer
                    , if selected == option then
                        batch
                            [ backgroundColor (hex "302633")
                            , color (hex "fff")
                            ]

                      else
                        batch
                            [ hover
                                [ backgroundColor (hex "302633")
                                ]
                            ]
                    ]
                ]
                [ input
                    [ type_ "radio"
                    , name "option"
                    , onInput (always (toMsg option))
                    , HA.checked (selected == option)
                    , css
                        [ position absolute
                        , appearance_none
                        , opacity (int 0)
                        , Css.height (px 0)
                        , Css.width (px 0)
                        ]
                    ]
                    []
                , text (showOption option)
                ]
    in
    div attrs (List.map optionView options)
