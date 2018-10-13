module Main exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
-- import EveryDict exposing (EveryDict)
import Css exposing (..)
import Array exposing (Array)

main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


-- Utility

catMaybes : List (Maybe a) -> List a
catMaybes =
    let
        g ma acc =
            case ma of
                Just a  -> a :: acc
                Nothing -> acc
    in
        List.foldr g []


-- Model

type alias CharacterSheet = 
    { name : String
    , aspects : Array Aspect
    , skills : Array Skill
    , refresh : Int
    , fatePoints : Int
    , stunts : Array Stunt
    , stress : Array StressTrack
    , consequences : Array Consequence
    }

type Severity
    = Mild
    | Moderate
    | Severe

showSeverity : Severity -> String
showSeverity severity =
    case severity of
        Mild     -> "Mild (-2)"
        Moderate -> "Moderate (-4)"
        Severe   -> "Severe (-6)"

type Aspect
    = Aspect String

type Consequence
    = Consequence Severity String

type Skill = Skill String

type Stunt = Stunt String String
-- type Stunt = Stunt String TextArea

type StressBox
    = StressBox Int Bool

type StressTrack
    = StressTrack String (Array StressBox)

type alias Model = CharacterSheet

initialModel : Model
initialModel =
    { name = ""
    , aspects
          = Array.fromList
            [ Aspect "Disciple of the Ivory Shroud"
            , Aspect "The Manners of a Goat"
            , Aspect "I Owe Old Finn Everything"
            , Aspect "Smashing is Always an Option"
            , Aspect "An Eye for an Eye"
            ]
    , skills
          = Array.fromList
            [ Skill "Great (+4) Fight"
            , Skill "Good (+3) Athletics, Physique"
            , Skill "Fair (+2) Stealth, Provoke, Rapport"
            , Skill "Average (+1) Crafts, Shoot, Deceive, Will"
            ]
    , refresh = 3
    , fatePoints = 0
    , stunts =
        Array.fromList
            [ Stunt
              "Fire Blast (Forceful)"
              "You can shoot or exhale fire. Whenever you succeed with style on a Forceful attack, you may forgo the boost to place an On Fire aspect with a free invoke on the defender or a nearby object. This effect only works if the target could believably catch fire. In addition, you can never become Unarmed."
            , Stunt
                "Poisoned Weapon (Sneaky/Quick)"
                "You use a poisoned weapon such as dagger, arrows, or darts. Once per scene, when you Sneakily or Quickly attack and deal 2 stress or more, you can force the defender to absorb 2 stress from your attack as a mild consequence. Some targets—robots, inanimate objects, and so on—are immune to poison."
            , Stunt
                "Strong Legs (Quick)"
                "You have powerful legs that are especially suited for running or jumping. During contests and cliffhangers, +2 when Quickly over-coming physical obstacles or creating advantages."
            ]
    , stress =
        Array.fromList
            [ StressTrack
              "Physical"
              (Array.fromList
                   [ StressBox 1 False
                   , StressBox 2 False
                   , StressBox 3 False
                   ])
            , StressTrack
              "Mental"
              (Array.fromList
                   [ StressBox 1 False
                   , StressBox 2 False
                   ])
            ]
    , consequences =
        Array.fromList
            [ Consequence Mild ""
            , Consequence Moderate ""
            , Consequence Severe ""
            ]
    }

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)


-- Update

type Msg
    = NoOp
    | UpdateName String
      
    -- Aspects
    | UpdateAspect Int String
    | AddNewAspect String
    | RemoveAspect Int

    -- Skills
    | UpdateSkill Int String
    | AddNewSkill String
    | RemoveSkill Int

    -- Stunts
    | UpdateStunt Int Stunt
    | AddNewStunt String String
    | RemoveStunt Int

    -- Stress
    | ToggleStressBox Int Int StressBox

    -- Consequences
    | UpdateConsequence Int Consequence


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)

        UpdateName name ->
            ({ model | name = name }
            , Cmd.none)

        UpdateAspect index title ->
            ({ model
                 | aspects
                   = Array.set index (Aspect title) model.aspects
             }
            , Cmd.none)

        AddNewAspect title ->
            ({ model
                 | aspects
                   = Array.push (Aspect title) model.aspects
             }
            , Cmd.none)

        RemoveAspect index ->
            ({ model
                 | aspects
                   = removeIndexFromArray index model.aspects
             }
            , Cmd.none)

        UpdateSkill index title ->
            ({ model
                 | skills
                   = Array.set index (Skill title) model.skills
             }
            , Cmd.none)

        AddNewSkill title ->
            ({ model
                 | skills
                   = Array.push (Skill title) model.skills
             }
            , Cmd.none)

        RemoveSkill index ->
            ({ model
                 | skills
                   = removeIndexFromArray index model.skills
             }
            , Cmd.none)


        UpdateStunt index stunt ->
            ({ model
                 | stunts
                   = Array.set index stunt model.stunts
             }
            , Cmd.none)

        AddNewStunt title description ->
            ({ model
                 | stunts
                   = Array.push (Stunt title description) model.stunts
             }
            , Cmd.none)

        RemoveStunt index ->
            ({ model
                 | stunts
                   = removeIndexFromArray index model.stunts
             }
            , Cmd.none)

        ToggleStressBox trackIndex index stressBox ->
            let 
                update =
                    case Array.get trackIndex model.stress of
                        Nothing ->
                            model
                        Just (StressTrack title boxes) ->
                            { model
                                | stress
                                  = Array.set
                                  trackIndex
                                  (StressTrack
                                       title
                                       (Array.set index stressBox boxes))
                                  model.stress
                            }
            in
                (update, Cmd.none)

        UpdateConsequence index consequence ->
            ({ model
                 | consequences
                   = Array.set index consequence model.consequences
             }
            , Cmd.none)


removeIndexFromArray : Int -> Array a -> Array a
removeIndexFromArray index array =
    array
        |> Array.toIndexedList
        |> List.filter (\(idx, _) -> idx /= index)
        |> List.map Tuple.second
        |> Array.fromList

-- View

view : Model -> Html Msg
view model =
    div [ css
          [ maxWidth (Css.em 32)
          ]
        ]
        [ textInput
              "Name"
              UpdateName
              model.name
        , aspectView model.aspects
        , skillView model.skills
        , stuntView model.stunts
        , stressView model.stress
        , consequenceView model.consequences
        ]

inputStyles =
    [ Css.width (pct 100)
    , border3 (px 1) solid transparent
    , borderRadius (px 4)
    , padding (Css.em 0.25)
    , flex (int 1)
    , focus
        [ border3 (px 1) solid (hex "888")
        ]
    , hover
        [ border3 (px 1) solid (hex "888")
        ]
    ]

textInput : String -> (String -> Msg) -> String -> Html Msg
textInput title msg currentValue =
    div []
        [ label [] [ text title ]
        , input [ type_ "text"
                , css []
                , onInput msg
                , value currentValue
               ] []
        ]

aspectView : Array Aspect -> Html Msg
aspectView aspects =
    div []
        [ h2 [] [ text "Aspects" ]
        , div []
            <| Array.toList
                <| Array.indexedMap
                    aspectInput
                    aspects
        , button
              [ onClick (AddNewAspect "") ]
              [ text "New Aspect" ]
        ]

aspectInput : Int -> Aspect -> Html Msg
aspectInput index (Aspect title) =
    div []
        [ input [ type_ "text"
                , css inputStyles
                , onInput (UpdateAspect index)
                , value title
                , placeholder <| "Aspect #" ++ toString (index + 1)
               ] []
        , if
              index >= 5
          then
              button
              [ onClick (RemoveAspect index)]
              [ text "Remove" ]
          else
              span [] []
        ]


-- Skill View

skillView : Array Skill -> Html Msg
skillView skills =
    div []
        [ h2 [] [ text "Skills" ]
        , div []
            <| Array.toList
                <| Array.indexedMap
                    skillInput
                    skills
        , button
              [ onClick (AddNewSkill "") ]
              [ text "New Skill Rating" ]
        ]

skillInput : Int -> Skill -> Html Msg
skillInput index (Skill title) =
    div [ css [ displayFlex ]]
        [ input [ type_ "text"
                , css (inputStyles ++
                       [ flex (int 1)
                        
                       ])
                , onInput (UpdateSkill index)
                , value title
               ] []
        , button
              [ onClick (RemoveSkill index)]
              [ text "Remove" ]
        ]


-- Stunt View

stuntView : Array Stunt -> Html Msg
stuntView stunts =
    div []
        [ h2 [] [ text "Stunts" ]
        , div []
            <| Array.toList
                <| Array.indexedMap
                    stuntInput
                    stunts
        , button
              [ onClick (AddNewStunt "" "") ]
              [ text "New Stunt" ]
        ]

stuntInput : Int -> Stunt -> Html Msg
stuntInput index (Stunt title description) =
    div []
        [ input [ type_ "text"
                , css (inputStyles ++
                       [ display block
                       , fontWeight bold
                       ])
                , onInput
                      (\newTitle ->
                           UpdateStunt
                           index
                           (Stunt newTitle description))
                , value title
               ] []
        , textarea
              [ onInput
                    (\newDescription ->
                         UpdateStunt
                         index
                         (Stunt title newDescription))
              , value description
              , rows 1
              , css [ display block
                    , resize none
                    , border3 (px 1) solid transparent
                    , borderRadius (px 4)
                    , padding (Css.em 0.25)
                    , Css.width (pct 100)
                    , focus
                        [ border3 (px 1) solid (hex "888")
                        ]
                    , hover
                          [ border3 (px 1) solid (hex "888")
                          ]
                    ]
              ]
              []
        , button
              [ onClick (RemoveStunt index)]
              [ text "Remove" ]
        ]


-- Stress View

stressView : Array StressTrack -> Html Msg
stressView stressTracks =
    div []
        ([ h2 [] [ text "Stress" ] ]
           ++ Array.toList
               (Array.indexedMap
                    stressTrackView
                    stressTracks))

stressTrackView : Int -> StressTrack -> Html Msg
stressTrackView trackIndex (StressTrack title stressBoxes) =
    div []
        [ div [] [ text title ]
        , stressBoxView trackIndex stressBoxes
        ]
        

stressBoxView : Int -> Array StressBox -> Html Msg
stressBoxView trackIndex stressBoxes =
    div [ css [ displayFlex ] ]
        (Array.toList
             (Array.indexedMap
                  (stressInput trackIndex)
                  stressBoxes))

stressInput : Int -> Int -> StressBox -> Html Msg
stressInput trackIndex index (StressBox points isChecked) =
    let
        identifier =
            "stress-track-" ++ toString trackIndex ++
            "-box-" ++ toString index ++
            "-points-" ++ toString points

        checkedStyles =
            batch <|
                if
                    isChecked
                then
                    [ backgroundColor (hex "333")
                    , color (hex "fff")
                    ]
                else
                    [ backgroundColor (hex "fff")
                    , color (hex "333")
                    ]
    in
        div []
            [ label
                  [ for identifier
                  , css
                        [ fontSize (Css.em 1.1)
                        , border3 (px 2) solid (hex "333")
                        , padding2 (Css.em 0.25) (Css.em 0.75)
                        , display inlineBlock
                        , margin2 (px 0) (Css.em 0.25)
                        , borderRadius (px 4)
                        , fontWeight bold
                        , userSelect_none
                        , checkedStyles
                        ]
                  ]
                  [ text (toString points) ]
            , input [ type_ "checkbox"
                    , id identifier
                    , HA.checked isChecked
                    , onCheck
                          (always
                               (ToggleStressBox
                                    trackIndex index
                                    (StressBox
                                         points
                                         (not isChecked))))
                    , css
                          [ position absolute
                          , opacity (num 0)
                          , Css.height (px 0)
                          , Css.width (px 0)
                          ]
                    ] []
            -- , button
            --       [ onClick (RemoveSkill index)]
            --       [ text "Remove" ]
            ]


-- Consequences
consequenceView : Array Consequence -> Html Msg
consequenceView consequences =
    div []
        [ h2 [] [ text "Consequences" ]
        , div []
            <| Array.toList
                <| Array.indexedMap
                    consequenceInput
                    consequences
        -- , button
        --       [ onClick (AddNewAspect "") ]
        --       [ text "New Aspect" ]
        ]

consequenceInput : Int -> Consequence -> Html Msg
consequenceInput index (Consequence severity title) =
    div [ css [ displayFlex ] ]
        [ span [] [ text (showSeverity severity) ]
        , input [ type_ "text"
                , css inputStyles
                , onInput
                      (\newTitle ->
                           (UpdateConsequence
                            index
                            (Consequence severity newTitle)))
                , value title
                -- , placeholder <| "Aspect #" ++ toString (index + 1)
               ] []
        -- , if
        --       index >= 5
        --   then
        --       button
        --       [ onClick (RemoveAspect index)]
        --       [ text "Remove" ]
        --   else
        --       span [] []
        ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- Extra Styles

userSelect_none : Css.Style
userSelect_none =
    batch
        [ Css.property "-webkit-touch-callout" "none" -- iOS Safari
        , Css.property "-webkit-user-select" "none" -- Safari
        , Css.property "-khtml-user-select" "none" -- Konqueror HTML
        , Css.property "-moz-user-select" "none" -- Firefox
        , Css.property "-ms-user-select" "none" -- Internet Explorer/Edge
        , Css.property "user-select" "none"]
