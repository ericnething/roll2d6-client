module Login exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Task
import Login.Types exposing (..)
import Json.Decode
import API


update : Msg -> Model -> (Model, Cmd ConsumerMsg)
update msg model =
    case msg of
        Login ->
            ( model
            , API.login
                { email = model.email
                , password = model.password
                })

        LoginResponse value ->
            ( model
            , Task.perform identity (Task.succeed LoadLobby))

        Register ->
            ( model
            , API.register
                { username = model.username
                , email = model.email
                , password = model.password
                })

        RegisterResponse value ->
            ( model
            , Cmd.none)

        UpdateEmail email ->
            ({ model | email = email }, Cmd.none)

        UpdatePassword password ->
            ({ model | password = password }, Cmd.none)

        UpdateUsername username ->
            ({ model | username = username }, Cmd.none)

        ChangeTab tab ->
            ({ model | tab = tab }, Cmd.none)

view : Model -> Html ConsumerMsg
view model = Html.Styled.map LocalMsg <|
    div [ css
          [ color (hex "eee")
          , backgroundColor (hex "36393f")
          , Css.minHeight (vh 100)
          , displayFlex
          , alignItems center
          , justifyContent spaceAround
          ]
        ]
        [ formView model
        ]

tabView : String -> Tab -> Bool -> Html Msg
tabView title tab isActive =
    button
    [ css
      [ padding2 (Css.em 1) (Css.em 0.8)
      , color (hex "333")
      , border (px 0)
      , borderTopWidth (Css.em 0.2)
      , borderStyle solid
      , cursor pointer
      , backgroundColor transparent
      , hover
          [ opacity (num 1)
          ]
      , if
            isActive
       then
           Css.batch
               [ borderColor (hex "87CEFA")
               , color (hex "87CEFA")
               ]
       else
           Css.batch
               [ borderColor transparent
               , color (hex "eee")
               , opacity (num 0.6)
               ]
      ]
    , onClick (ChangeTab tab)
    ] [ text title ]

formView : Model -> Html Msg
formView model =
    div [ css
          [ maxWidth (Css.em 20)
          , padding (Css.em 0.5)
          , Css.property "transform" "translateY(-12vh)"
          ]
        ]
        [ h1[ css
              [ textAlign center
              , margin2 (Css.em 1.5) (Css.em 0)
              ]
            ]
              [ text "Fate RPG" ]
        , div [ css
                [ displayFlex
                , Css.width (pct 100)
                , marginBottom (Css.em 1)
                ]
              ]
              [ tabView
                    "Login"
                    LoginTab
                    (model.tab == LoginTab)
              , tabView
                    "Create an account"
                        RegisterTab
                        (model.tab == RegisterTab)
              ]
        , if model.tab == RegisterTab
          then usernameInputView model
          else text ""
        , emailInputView model
        , passwordInputView model
        , buttonView model.tab
        ]

buttonView : Tab -> Html Msg
buttonView tab =
    let
        (title, handler) =
            case tab of
                LoginTab ->
                    ("Login", Login)
                RegisterTab ->
                    ("Create account", Register)
    in
        button
        [ type_ "button"
        , onClick handler
        , css
              [ display block
              , padding2 (Css.em 0.25) (Css.em 0.5)
              , backgroundColor (hex "DE650E")
              , borderRadius (Css.em 0.1)
              , border (px 0)
              , color (hex "eee")
              , cursor pointer
              ]
        ] [ text title ]

usernameInputView : Model -> Html Msg
usernameInputView model =
    formFieldView
    { id = "username"
    , title = "Username"
    , type_ = "text"
    , onInput = UpdateUsername
    , value = model.username
    }

emailInputView : Model -> Html Msg
emailInputView model =
    formFieldView
    { id = "email"
    , title = "Email"
    , type_ = "text"
    , onInput = UpdateEmail
    , value = model.email
    }

passwordInputView : Model -> Html Msg
passwordInputView model =
    formFieldView
    { id = "password"
    , title = "Password"
    , type_ = "password"
    , onInput = UpdatePassword
    , value = model.password
    }

type alias FormField =
    { id : String
    , title : String
    , type_ : String
    , onInput : String -> Msg
    , value : String
    }

formFieldView : FormField -> Html Msg
formFieldView ff =
    div []
        [ label
              [ for ff.id
              , css
                    [ display block
                    , paddingBottom (Css.em 0.25)
                    ]
              ]
              [ text ff.title ]
        , input
              [ type_ ff.type_
              , id ff.id
              , onInput ff.onInput
              , value ff.value
              , css
                  [ display block
                  , Css.width (pct 100)
                  , marginBottom (Css.em 0.8)
                  , border (px 0)
                  , lineHeight (num 1.2)
                  , padding2 (Css.em 0.25) (Css.em 0.5)
                  , borderRadius (Css.em 0.1)
                  ]
              ] []         
        ]
