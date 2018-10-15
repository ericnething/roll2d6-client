module Main exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Array exposing (Array)
import List.Extra exposing (takeWhile, dropWhile, stableSortWith)

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

listDifference : List a -> List a -> List a
listDifference xs ys =
    let             
        removeFirst a bs =
            List.append
            (takeWhile (\b -> b /= a) bs)
            (List.drop 1 <| dropWhile (\b -> b /= a) bs)
    in
        List.foldl (\y acc -> removeFirst y acc) xs ys
        

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
    , conditions : Array Condition
    }

-- Aspects

type Aspect
    = Aspect String

-- Consequences

type Severity
    = Mild
    | Moderate
    | Severe
    | Extreme

showSeverity : Severity -> String
showSeverity severity =
    case severity of
        Mild     -> "Mild -2"
        Moderate -> "Moderate -4"
        Severe   -> "Severe -6"
        Extreme  -> "Extreme -8"

consequenceSeverityList =
    [ Mild
    , Mild
    , Moderate
    , Severe
    , Extreme
    ]

severityToInt : Severity -> Int
severityToInt severity =
    case severity of
        Mild     -> -2
        Moderate -> -4
        Severe   -> -6
        Extreme  -> -8

type Consequence
    = Consequence Severity String

-- Skills

type SkillRating
    = Legendary
    | Epic
    | Fantastic
    | Superb
    | Great
    | Good
    | Fair
    | Average
    -- | Mediocre
    | Poor
    | Terrible

skillRatingList =
    [ Legendary
    , Epic
    , Fantastic
    , Superb
    , Great
    , Good
    , Fair
    , Average
    , Poor
    , Terrible
    ]

skillRatingToInt : SkillRating -> Int
skillRatingToInt rating =
    case rating of
        Legendary ->  8
        Epic      ->  7
        Fantastic ->  6
        Superb    ->  5
        Great     ->  4
        Good      ->  3
        Fair      ->  2
        Average   ->  1
        -- Mediocre  ->  0
        Poor      -> -1
        Terrible  -> -2

showSkillRating : SkillRating -> String
showSkillRating rating =
    case rating of
        Legendary -> "Legendary +8"
        Epic      -> "Epic +7"
        Fantastic -> "Fantastic +6"
        Superb    -> "Superb +5"
        Great     -> "Great +4"
        Good      -> "Good +3"
        Fair      -> "Fair +2"
        Average   -> "Average +1"
        -- Mediocre  -> "Mediocre (+0)"
        Poor      -> "Poor -1"
        Terrible  -> "Terrible -2"

type Skill = Skill SkillRating String
          
-- Stunts

type Stunt = Stunt String String

-- Stress

type StressBox
    = StressBox Int Bool

type StressTrack
    = StressTrack String (Array StressBox)

-- Conditions

type Condition
    = Condition String (Array StressBox)


type EditMode
    = EditModeNone
    | EditModeStress
    | EditModeConditions

type alias Model =
    { characterSheet : CharacterSheet
    , editMode : EditMode
    }

initialModel : Model
initialModel =
    { characterSheet = initialCharacterSheet
    , editMode = EditModeNone
    }

initialCharacterSheet : CharacterSheet
initialCharacterSheet =
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
            [ Skill Great "Fight"
            , Skill Good "Athletics, Physique"
            , Skill Fair "Stealth, Provoke, Rapport"
            , Skill Average  "Crafts, Shoot, Deceive, Will"
            ]
    , refresh = 3
    , fatePoints = 0
    , stunts =
        Array.fromList
            [ Stunt
              "Fire Blast"
              "You can shoot or exhale fire. Whenever you succeed with style on a Forceful attack, you may forgo the boost to place an On Fire aspect with a free invoke on the defender or a nearby object. This effect only works if the target could believably catch fire. In addition, you can never become Unarmed."
            , Stunt
                "Poisoned Weapon"
                "You use a poisoned weapon such as dagger, arrows, or darts. Once per scene, when you Sneakily or Quickly attack and deal 2 stress or more, you can force the defender to absorb 2 stress from your attack as a mild consequence. Some targets—robots, inanimate objects, and so on—are immune to poison."
            , Stunt
                "Strong Legs"
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
    , conditions =
        Array.fromList
            [ Condition
              "Bruised"
              (Array.fromList [ StressBox 2 False])
            , Condition
              "In Peril"
              (Array.fromList [ StressBox 4 False ])
            , Condition
              "Indebted"
              (Array.fromList
                   [ StressBox 1 False
                   , StressBox 1 False
                   , StressBox 1 False
                   , StressBox 1 False
                   ])
            ]
    }

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)


-- Update

type alias Index = Int

type Msg
    = NoOp
    | UpdateName String
      
    -- Aspects
    | UpdateAspect Int String
    | AddNewAspect String
    | RemoveAspect Int

    -- Skills
    | UpdateSkill Int Skill
    | AddNewSkill Skill
    | RemoveSkill Int

    -- Stunts
    | UpdateStunt Int Stunt
    | AddNewStunt String String
    | RemoveStunt Int

    -- Stress Tracks
    | UpdateStressTrack Index StressTrack
    | AddNewStressTrack StressTrack
    | RemoveStressTrack Int

    -- Stress Boxes
    | UpdateStressBox Int Int StressBox
    | AddStressBox Index StressBox
    | RemoveStressBox Index

    -- Consequences
    | UpdateConsequence Int Consequence
    | AddNewConsequence Consequence
    | RemoveConsequence Int

    -- Conditions
    | UpdateCondition Int Condition
    | AddNewCondition Condition
    | RemoveCondition Int
    | UpdateConditionBox Int Int StressBox
    | AddConditionBox Index StressBox
    | RemoveConditionBox Index

    -- Edit Mode
    | ToggleEditMode EditMode


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToggleEditMode editMode ->
            ({ model
                 | editMode
                   = if
                       model.editMode /= editMode
                     then
                         editMode
                     else
                         EditModeNone
                         
             }, Cmd.none)

        msg0 ->
            let
                (characterSheet, cmd) =
                    updateCharacterSheet msg0 model.characterSheet
            in
                ({ model | characterSheet = characterSheet }
                , cmd)

updateCharacterSheet : Msg -> CharacterSheet
                     -> (CharacterSheet, Cmd Msg)
updateCharacterSheet msg model =
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

        UpdateSkill index skill ->
            ({ model
                 | skills
                   = Array.set index skill model.skills
             }
            , Cmd.none)

        AddNewSkill skill ->
            ({ model
                 | skills
                   = Array.push skill model.skills
                       |> Array.toList
                       |> List.sortBy
                          (\(Skill rating _) ->
                               skillRatingToInt rating)
                       |> List.reverse
                       |> Array.fromList
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

        UpdateStressBox trackIndex index stressBox ->
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

        AddNewStressTrack stressTrack ->
            ({ model
                 | stress
                   = Array.push stressTrack model.stress
             }, Cmd.none)

        RemoveStressTrack index ->
            ({ model
                 | stress
                   = removeIndexFromArray index model.stress
             }, Cmd.none)

        UpdateStressTrack index stressTrack ->
            ({ model
                 | stress
                   = Array.set index stressTrack model.stress
             }, Cmd.none)

        AddStressBox trackIndex stressBox ->
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
                                       (Array.push stressBox boxes))
                                  model.stress
                            }
            in
                (update, Cmd.none)

        RemoveStressBox trackIndex ->
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
                                       (Array.slice 0 -1 boxes))
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

        AddNewConsequence newConsequence ->
            ({ model
                 | consequences
                   = Array.push newConsequence model.consequences
                       |> Array.toList
                       |> stableSortWith
                          (\(Consequence a _) (Consequence b _) ->
                               Basics.compare
                               (severityToInt b)
                               (severityToInt a))
                       |> Array.fromList
             }, Cmd.none)

        RemoveConsequence index ->
            ({ model
                 | consequences
                   = removeIndexFromArray index model.consequences
            }, Cmd.none)

        UpdateConditionBox trackIndex index stressBox ->
            let 
                update =
                    case Array.get trackIndex model.conditions of
                        Nothing ->
                            model
                        Just (Condition title boxes) ->
                            { model
                                | conditions
                                  = Array.set
                                  trackIndex
                                  (Condition
                                       title
                                       (Array.set index stressBox boxes))
                                  model.conditions
                            }
            in
                (update, Cmd.none)

        AddConditionBox trackIndex stressBox ->
            let
                update =
                    case Array.get trackIndex model.conditions of
                        Nothing ->
                            model
                        Just (Condition title boxes) ->
                            { model
                                | conditions
                                  = Array.set
                                  trackIndex
                                  (Condition
                                       title
                                       (Array.push stressBox boxes))
                                  model.conditions
                            }
            in
                (update, Cmd.none)

        RemoveConditionBox trackIndex ->
            let
                update =
                    case Array.get trackIndex model.conditions of
                        Nothing ->
                            model
                        Just (Condition title boxes) ->
                            { model
                                | conditions
                                  = Array.set
                                  trackIndex
                                  (Condition
                                       title
                                       (Array.slice 0 -1 boxes))
                                  model.conditions
                            }
            in
                (update, Cmd.none)
                    
        UpdateCondition trackIndex condition ->
            ({ model
                 | conditions
                   = Array.set trackIndex condition model.conditions
             }, Cmd.none)

        AddNewCondition condition ->
            ({ model
                 | conditions
                   = Array.push condition model.conditions
             }, Cmd.none)

        RemoveCondition index ->
            ({ model
                 | conditions
                   = removeIndexFromArray index model.conditions
             }, Cmd.none)

        _ -> (model, Cmd.none)

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
        [ nameView model.characterSheet.name
        , aspectView model.characterSheet.aspects
        , skillView model.characterSheet.skills
        , stuntView model.characterSheet.stunts
        , stressView
            model.characterSheet.stress
            (EditModeStress == model.editMode)
        , consequenceView model.characterSheet.consequences
        , conditionsView
            model.characterSheet.conditions
            (EditModeConditions == model.editMode)
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

nameView : String -> Html Msg
nameView name =
    textInput "Name" UpdateName name

aspectView : Array Aspect -> Html Msg
aspectView aspects =
    div []
        [ h2 [] [ text "Aspects" ]
        , div []
            <| Array.toList
                <| Array.indexedMap
                    aspectInput
                    aspects
        , defaultButton
              [ onClick (AddNewAspect "")
              , css
                  [ marginTop (Css.em 1) ]
              ]
              [ text "New Aspect" ]
        ]

aspectInput : Int -> Aspect -> Html Msg
aspectInput index (Aspect title) =
    div [ css
          [ displayFlex
          , alignItems center
          ]
        ]
        [ input [ type_ "text"
                , css [ inputStyles ]
                , onInput (UpdateAspect index)
                , value title
                , placeholder <| "Aspect #" ++ toString (index + 1)
               ] []
        , if
              index >= 5
          then
              defaultButton
              [ onClick (RemoveAspect index)
              , css
                    [ marginLeft (Css.em 0.5) ]
              ]
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
        , skillRatingButtonList skills
        ]

skillRatingButtonList : Array Skill -> Html Msg
skillRatingButtonList skills =
    div [ css
          [ displayFlex
          , alignItems center
          , flexWrap Css.wrap
          , marginTop (Css.em 1)
          ]
        ]
        <| List.map
            skillRatingButton
            (List.filter
                 (\rating ->
                      skills
                        |> Array.toList
                        |> List.map (\(Skill a _) -> a)
                        |> List.member rating
                        |> not)
                 skillRatingList)

skillRatingButton : SkillRating -> Html Msg
skillRatingButton rating =
    defaultButton
    [ css
      [ marginBottom (Css.em 0.5)
      , marginRight (Css.em 0.5)   
      ]
    , onClick (AddNewSkill (Skill rating ""))
    ]
    [ text (showSkillRating rating) ]

skillInput : Int -> Skill -> Html Msg
skillInput index (Skill rating title) =
    div [ css
          [ displayFlex
          , alignItems center
          ]
        ]
        [ span
              [ css
                [ fontWeight bold
                , marginRight (Css.em 0.5)
                , whiteSpace noWrap
                ]
              ]
              [ text (showSkillRating rating) ]
        , input [ type_ "text"
                , css
                      [ flex (int 1)
                      , inputStyles
                      ]
                , onInput
                      (\newTitle ->
                           UpdateSkill
                           index
                           (Skill rating newTitle))
                , value title
               ] []
        , defaultButton
              [ onClick (RemoveSkill index)
              , css
                  [ marginLeft (Css.em 0.5) ]
              ]
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
        , defaultButton
              [ onClick (AddNewStunt "" "") ]
              [ text "New Stunt" ]
        ]

stuntInput : Int -> Stunt -> Html Msg
stuntInput index (Stunt title description) =
    div []
        [ input [ type_ "text"
                , css
                      [ display block
                      , fontWeight bold
                      , inputStyles
                      ]
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
              , rows 5
              , css [ display block
                    , resize none
                    -- , border3 (px 1) solid transparent
                    , border3 (px 1) solid (hex "888")
                    , borderRadius (px 4)
                    , padding (Css.em 0.25)
                    , Css.width (pct 100)
                    -- , focus
                    --       [ border3 (px 1) solid (hex "888")
                    --     ]
                    -- , hover
                    --       [ border3 (px 1) solid (hex "888")
                    --       ]
                    ]
              ]
              []
        , defaultButton
              [ onClick (RemoveStunt index)]
              [ text "Remove" ]
        ]


-- Stress View

stressView : Array StressTrack -> Bool -> Html Msg
stressView stressTracks editModeActive =
    let
        view =
            if
                editModeActive
            then
                [ 
                -- , button
                --       [ onClick (ToggleEditMode EditModeStress) ]
                --       [ text "Toggle Edit Mode" ]
                div [] (Array.toList
                            (Array.indexedMap
                                 editStressTrackView
                                 stressTracks))
                , defaultButton
                      [ onClick
                            (AddNewStressTrack
                                 (StressTrack
                                      "New Stress Track"
                                      (Array.fromList
                                           [ StressBox 1 False ])))
                      ]
                      [ text "Add new stress track" ]
                ]
            else
                [ div [] (Array.toList
                              (Array.indexedMap
                                   stressTrackView
                                   stressTracks))
                ]
    in
        div []
            ([ div [ css
                    [ displayFlex
                    , alignItems center
                    ]
                  ]
                  [ div [ css
                          [ fontWeight bold
                          , fontSize (Css.em 1.1)
                          ]
                        ] [ text "Stress" ]
                  , toggleSwitch EditModeStress editModeActive
                  ]
             ] ++ view)
            

stressTrackView : Int -> StressTrack -> Html Msg
stressTrackView trackIndex (StressTrack title stressBoxes) =
    div []
        [ div [] [ text title ]
        , stressBoxView
            UpdateStressBox
            trackIndex
            stressBoxes
        ]

stressBoxView : (Int -> Int -> StressBox -> Msg)
              -> Int -> Array StressBox
              -> Html Msg
stressBoxView toggleStressBox trackIndex stressBoxes =
    div [ css [ displayFlex
              , flexWrap Css.wrap
              ]
        ]
        (Array.toList
             (Array.indexedMap
                  (stressInput toggleStressBox trackIndex)
                  stressBoxes))

stressInput : (Int -> Int -> StressBox -> Msg)
            -> Int
            -> Int
            -> StressBox
            -> Html Msg
stressInput toggleStressBox trackIndex index (StressBox points isChecked) =
    label
    [ css
      [ fontSize (Css.em 1.1)
      , border3 (px 2) solid (hex "333")
      , padding2 (Css.em 0.25) (Css.em 0.75)
      , display inlineBlock
      , margin2 (px 0) (Css.em 0.25)
      , borderRadius (px 4)
      , fontWeight bold
      , userSelect_none
      , if
            isChecked
       then
           batch
           [ backgroundColor (hex "333")
           , color (hex "fff")
           ]
       else
           batch
           [ backgroundColor (hex "fff")
           , color (hex "333")
           ]
      ]
    ]
    [ text (toString points)
    , input [ type_ "checkbox"
            , HA.checked isChecked
            , onCheck
                  (always
                       (toggleStressBox
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
    ]


editStressTrackView : Int -> StressTrack -> Html Msg
editStressTrackView trackIndex (StressTrack title stressBoxes) =
    div []
        [ input
              [ type_ "text"
              , css [ inputStyles ]
              , value title
              , onInput
                    (\newTitle ->
                         UpdateStressTrack
                         trackIndex
                         (StressTrack newTitle stressBoxes))
              ] []
        , editStressBoxView
            AddStressBox
            RemoveStressBox
            UpdateStressBox
            trackIndex
            stressBoxes
        , defaultButton
              [ onClick (RemoveStressTrack trackIndex)
              ]
              [ text "Remove" ]
        ]

editStressBoxView : (Int -> StressBox -> Msg)
                  -> (Int -> Msg)
                  -> (Int -> Int -> StressBox -> Msg)
                  -> Int
                  -> Array StressBox
                  -> Html Msg
editStressBoxView addBox removeBox updateBox trackIndex stressBoxes =
    div [ css [ displayFlex
              , flexWrap Css.wrap
              , alignItems center
              ]
        ]
        (Array.toList
             (Array.indexedMap
                  (editStressInput updateBox trackIndex)
                  stressBoxes) ++
        [ removeBoxButton
              [ onClick (removeBox trackIndex) ]
              [ text "✕" ]
        , addNewBoxButton
              [ onClick (addBox trackIndex (StressBox 1 False)) ]
              [ text "+" ]
        ])

removeBoxButton =
    styled button
        [ backgroundColor (hex "eee")
        , border (px 0)
        , color (hex "888")
        , borderRadius (px 4)
        , display inlineBlock
        , padding2 (Css.em 0.5) (Css.em 1)
        , cursor pointer
        , marginRight (Css.em 0.5)
        , hover
              [ backgroundColor (hex "DC143C")
              , color (hex "fff")
              ]
        , Css.property
            "transition"
            "background-color 0.2s, color 0.2s"
        ]
        
addNewBoxButton =
    styled button
        [ backgroundColor (hex "eee")
        -- , border3 (px 1) dashed (hex "888")
        , border (px 0)
        , color (hex "888")
        , borderRadius (px 4)
        , display inlineBlock
        , padding2 (Css.em 0.5) (Css.em 1)
        , cursor pointer
        , marginRight (Css.em 0.5)
        , hover
              [ backgroundColor (hex "1E90FF")
              , color (hex "fff")
              -- , borderColor transparent
              ]
        , Css.property
            "transition"
            "background-color 0.2s, color 0.2s"
        ]
        
editStressInput : (Int -> Int -> StressBox -> Msg)
                -> Int
                -> Int
                -> StressBox
                -> Html Msg
editStressInput updateBox trackIndex index (StressBox points isChecked) =
    input [ type_ "number"
          , value (toString points)
          , onInput
                (\newPoints ->
                     (updateBox
                          trackIndex index
                          (StressBox
                               -- (Result.withDefault
                               --      points
                               --      (String.toInt newPoints))
                               (stringToNatWithDefault
                                    points
                                    newPoints)
                               isChecked)))
          , css
                [ fontSize (Css.em 1.1)
                , border3 (px 2) solid (hex "333")
                , padding2 (Css.em 0.25) (Css.em 0.75)
                , display inline
                , marginRight (Css.em 0.5)
                , borderRadius (px 4)
                , fontWeight bold
                , backgroundColor (hex "fff")
                , color (hex "333")
                , Css.width (Css.em 3.5)
                ]
          ] []

toggleSwitch : EditMode -> Bool -> Html Msg
toggleSwitch mode isActive =
    span [ css
           [ displayFlex
           , alignItems center
           ]
         ]
        [ label [ css
                 [ position relative
                 , display inlineBlock
                 , Css.width (px 60)
                 , Css.height (px 34)
                 , Css.property "transform" "scale(0.65)"
                 ]
                ]
              [ input
                    [ type_ "checkbox"
                    , HA.checked isActive
                    , onCheck (always (ToggleEditMode mode))
                    , css [ display none ]
                    ] []
              , span
                    [ css
                      [ position absolute
                      , cursor pointer
                      , top (px 0)
                      , left (px 0)
                      , right (px 0)
                      , bottom (px 0)
                      , if isActive
                        then
                            batch
                            [ backgroundColor (hex "2196F3")
                            , before
                                  [ Css.property
                                        "-webkit-transform"
                                        "translateX(26px)"
                                  , Css.property
                                      "-ms-transform"
                                      "translateX(26px)"
                                  , Css.property
                                      "transform"
                                      "translateX(26px)"
                                  ]
                            ]
                        else backgroundColor (hex "ccc")
                      , Css.property "-webkit-transition" "0.2s"
                      , Css.property "transition" "0.2s ease"
                      , borderRadius (px 34)
                      , before
                            [ position absolute
                            , Css.property "content" "\"\""
                            , Css.height (px 26)
                            , Css.width (px 26)
                            , left (px 4)
                            , bottom (px 4)
                            , backgroundColor (hex "fff")
                            , Css.property "-webkit-transition" "0.2s"
                            , Css.property "transition" "0.2s ease"
                            , borderRadius (pct 50)
                            ]
                      ]
                    ] []
              ]
        , span [ css
                     [ fontSize (pct 80)
                     , color (hex ("888"))
                     ]
               ]
            [ text (if isActive
                    then "Unlocked"
                    else "Locked")
            ]
        ]

stringToNatWithDefault : Int -> String -> Int
stringToNatWithDefault default value =
    (String.toInt value)
        |> Result.withDefault default
        |> Basics.max 1

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
        , consequenceButtonList consequences
        ]

consequenceInput : Int -> Consequence -> Html Msg
consequenceInput index (Consequence severity title) =
    div [ css [ displayFlex
              , alignItems center
              ]
        ]
        [ span [ css
                 [ fontWeight bold
                 , marginRight (Css.em 0.5)
                 , whiteSpace noWrap
                 ]
               ]
              [ text (showSeverity severity) ]
        , input [ type_ "text"
                , css [ flex (int 1)
                      , inputStyles
                      ]
                , onInput
                      (\newTitle ->
                           (UpdateConsequence
                            index
                            (Consequence severity newTitle)))
                , value title
               ] []
        , defaultButton
              [ onClick (RemoveConsequence index)
              , css
                  [ marginLeft (Css.em 0.5) ]
              ]
              [ text "Remove" ]
        ]

consequenceButtonList : Array Consequence -> Html Msg
consequenceButtonList consequences =
    div [ css
          [ displayFlex
          , alignItems center
          , flexWrap Css.wrap
          , marginTop (Css.em 1)
          ]
        ]
        <| List.map
            consequenceSeverityButton
            (listDifference
                 consequenceSeverityList
                 (List.map
                      (\(Consequence a _) -> a)
                      (Array.toList consequences)))
            -- (List.filter
            --      (\severity ->
            --           consequences
            --             |> Array.toList
            --             |> List.map (\(Consequence a _) -> a)
            --             |> List.member severity
            --             |> not)
            --      consequenceSeverityList)

consequenceSeverityButton : Severity -> Html Msg
consequenceSeverityButton severity =
    defaultButton
    [ css
      [ marginBottom (Css.em 0.5)
      , marginRight (Css.em 0.5)   
      ]
    , onClick (AddNewConsequence (Consequence severity ""))
    ]
    [ text (showSeverity severity) ]


-- Conditions

conditionsView : Array Condition -> Bool -> Html Msg
conditionsView conditions editModeActive =
    let
        view =
            if
                editModeActive
            then
                [ div [] (Array.toList
                            (Array.indexedMap
                                 editConditionView
                                 conditions))
                , defaultButton
                      [ onClick
                            (AddNewCondition
                                 (Condition
                                      "New Condition"
                                      (Array.fromList
                                           [ StressBox 1 False ])))
                      ]
                      [ text "Add New Condition" ]
                ]
            else
                [ div [] (Array.toList
                              (Array.indexedMap
                                   conditionView
                                   conditions))
                ]
    in
        div []
            ([ div [ css
                    [ displayFlex
                    , alignItems center
                    ]
                  ]
                  [ div [ css
                          [ fontWeight bold
                          , fontSize (Css.em 1.1)
                          ]
                        ] [ text "Conditions" ]
                  , toggleSwitch EditModeConditions editModeActive
                  ]
             ] ++ view)


conditionView : Int -> Condition -> Html Msg
conditionView trackIndex (Condition title stressBoxes) =
    div [ css [ displayFlex
              , alignItems center
              , marginTop (Css.em 0.5)
              ]
        ]
        [ stressBoxView
              UpdateConditionBox
              trackIndex
              stressBoxes
        , div []
            [ text title ]
        ]

editConditionView : Int -> Condition -> Html Msg
editConditionView trackIndex (Condition title stressBoxes) =
    div []
        [ input
              [ type_ "text"
              , css [ inputStyles ]
              , value title
              , onInput
                    (\newTitle ->
                         UpdateCondition
                         trackIndex
                         (Condition newTitle stressBoxes))
              ] []
        , editStressBoxView
            AddConditionBox
            RemoveConditionBox
            UpdateConditionBox
            trackIndex
            stressBoxes
        , defaultButton
              [ onClick (RemoveCondition trackIndex)
              ]
              [ text "Remove" ]
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
        , Css.property "user-select" "none"
        ]

inputStyles : Css.Style
inputStyles =
    batch
    [ Css.width (pct 100)
    -- , border3 (px 1) solid transparent
    , border3 (px 1) solid (hex "888")
    , borderRadius (px 4)
    , padding (Css.em 0.25)
    , flex (int 1)
    -- , focus
    --     [ border3 (px 1) solid (hex "888")
    --     ]
    -- , hover
    --     [ border3 (px 1) solid (hex "888")
    --     ]
    ]


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
