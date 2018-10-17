module CharacterSheet.View exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Array exposing (Array)
import Util exposing
    ( listDifference
    , stringToNatWithDefaultNonZero
    , stringToNatWithDefault
    )
import CharacterSheet.Model exposing (..)
import CharacterSheet.Update exposing (..)


editView : Model -> Html Msg
editView model =
    div [ css
          [ maxWidth (Css.em 32)
          ]
        ]
        [ nameView model.characterSheet.name
        , descriptionView model.characterSheet.description
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
        , refreshView model.characterSheet.refresh
        , fatePointsView model.characterSheet.fatePoints
        ]

refreshView : Int -> Html Msg
refreshView points =
    div [ css
          [ displayFlex
          , alignItems center
          , marginTop (Css.em 1)
          ]
        ]
        [ label
              [ css
                [ display block
                , fontSize (Css.em 1.1)
                , fontWeight bold
                , marginRight (Css.em 0.5)
                ]
              ]
              [ text "Refresh" ]
        , input
              [ type_ "number"
              , css [ inputStyles
                    , Css.width (Css.em 3)
                    , flex none
                    ]
              , onInput
                    (\newPoints ->
                         UpdateRefresh
                         (stringToNatWithDefaultNonZero
                              points
                              newPoints))
              , value (toString points)
              ] []
        ]

fatePointsView : Int -> Html Msg
fatePointsView points =
    div [ css
          [ displayFlex
          , alignItems center
          , marginTop (Css.em 1)
          ]
        ]
        [ label
              [ css
                [ display block
                , fontSize (Css.em 1.1)
                , fontWeight bold
                , marginRight (Css.em 0.5)
                ]
              ]
              [ text "Fate Points" ]
        , input
              [ type_ "number"
              , css [ inputStyles
                    , Css.width (Css.em 3)
                    , flex none
                    ]
              , onInput
                    (\newPoints ->
                         UpdateFatePoints
                         (stringToNatWithDefault
                              points
                              newPoints))
              , value (toString points)
              ] []
        ]        

nameView : String -> Html Msg
nameView name =
    div []
        [ label [ css
                  [ display block
                  , fontSize (Css.em 1.1)
                  , fontWeight (int 500)
                  , marginTop (Css.em 1)
                  ]
                ]
              [ text "Name" ]
        , input [ type_ "text"
                , css [ inputStyles ]
                , onInput UpdateName
                , value name
               ] []
        ]

descriptionView : String -> Html Msg
descriptionView description =
    div []
        [ label [ css
                  [ display block
                  , fontSize (Css.em 1.1)
                  , fontWeight (int 500)
                  , marginTop (Css.em 1)
                  ]
                ]
              [ text "Description" ]
        , textarea
              [ rows 5
              , css [ inputStyles ]
              , onInput UpdateDescription
              , value description
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
        , defaultButton
              [ onClick (AddNewAspect "")
              , css
                  [ marginTop (Css.em 1) ]
              ]
              [ text "Add New Aspect" ]
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
        , defaultButton
              [ onClick (RemoveAspect index)
              , css
                    [ marginLeft (Css.em 0.5) ]
              ]
              [ text "Remove" ]
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
              [ text "Add New Stunt" ]
        ]

stuntInput : Int -> Stunt -> Html Msg
stuntInput index (Stunt title description) =
    div [ css
          [ marginBottom (Css.em 1.25)
          ]
        ]
        [ input [ type_ "text"
                , css
                      [ display block
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
                    , border3 (px 1) solid (hex "888")
                    , borderRadius (px 4)
                    , padding (Css.em 0.25)
                    , Css.width (pct 100)
                    ]
              ]
              []
        , defaultButton
              [ onClick (RemoveStunt index)
              , css
                  [ marginTop (Css.em 0.5)
                  ]
              ]
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
                [ p [] [ text "Use the Up and Down arrows on your keyboard to change the value of a stress box." ]
                , div []
                    (Array.toList
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
                    , marginTop (Css.em 1)
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
      [ fontSize (Css.em 1)
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
    div [ css
          [ marginBottom (Css.em 1) ]
        ]
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
              , margin2 (Css.em 0.5) (px 0)
              ]
        ]
        (Array.toList
             (Array.indexedMap
                  (editStressInput updateBox trackIndex)
                  stressBoxes) ++
        [ if Array.length stressBoxes > 1
          then
              removeBoxButton
              [ onClick (removeBox trackIndex) ]
              [ text "✕" ]
          else
              text ""
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
        , padding2 (Css.em 0.35) (Css.em 0.8)
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
        , border (px 0)
        , color (hex "888")
        , borderRadius (px 4)
        , display inlineBlock
        , padding2 (Css.em 0.35) (Css.em 0.8)
        , cursor pointer
        , marginRight (Css.em 0.5)
        , hover
              [ backgroundColor (hex "1E90FF")
              , color (hex "fff")
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
                               (stringToNatWithDefaultNonZero
                                    points
                                    newPoints)
                               isChecked)))
          , css
                [ fontSize (Css.em 1)
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
                [ p [] [ text "Use the Up and Down arrows on your keyboard to change the value of a shift box." ]
                , div []
                    (Array.toList
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
                    , marginTop (Css.em 1)
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
    div [ css
          [ marginBottom (Css.em 1) ]
        ]
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
    , border3 (px 1) solid (hex "888")
    , borderRadius (px 4)
    , padding (Css.em 0.25)
    , flex (int 1)
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



-- Read Only Views

readOnlyView : Model -> Html Msg
readOnlyView model =
    div [ css
          [ maxWidth (Css.em 24) ]
        ]
        [ div [ css
                [ fontWeight bold ]
              ]
              [ text model.characterSheet.name ]
        , div [] [ text model.characterSheet.description ]
        , readOnlyAspectView model.characterSheet.aspects
        , readOnlySkillsView model.characterSheet.skills
        , readOnlyStuntsView model.characterSheet.stunts
        , readOnlyStressView model.characterSheet.stress
        , readOnlyConsequencesView model.characterSheet.consequences
        , readOnlyConditionsView model.characterSheet.conditions
        , readOnlyRefreshView model.characterSheet.refresh
        , fatePointsView model.characterSheet.fatePoints
        ]

readOnlyAspectView : Array Aspect -> Html Msg
readOnlyAspectView aspects =
    div []
        [ h2 [] [ text "Aspects" ]
        , div []
            <| Array.toList
                <| Array.map
                    (\(Aspect title) -> div [] [ text title ])
                    aspects
        ]

readOnlySkillsView : Array Skill -> Html Msg
readOnlySkillsView skills =
    let
        skillView (Skill rating title) =
            div []
                [ span
                      [ css
                        [ fontWeight bold
                        , marginRight (Css.em 0.5)
                        , whiteSpace noWrap
                        ]
                      ]
                      [ text (showSkillRating rating) ]
                , span []
                    [ text title ]
                ]
    in
        div []
            [ h2 [] [ text "Skills" ]
            , div []
                <| Array.toList
                    <| Array.map
                        skillView
                            skills
            ]

readOnlyStuntsView : Array Stunt -> Html Msg
readOnlyStuntsView stunts =
    let
        stuntView (Stunt title description) =
            div [ css
                  [ marginBottom (Css.em 0.5)
                  ]
                ]
                [ span [ css
                         [ fontWeight bold
                         ]
                       ]
                      [ text (title ++ ": ")]
                , span [] [ text description ]
                ]
    in
        div []
            [ h2 [] [ text "Stunts" ]
            , div []
                <| Array.toList
                    <| Array.map
                        stuntView
                        stunts
            ]

readOnlyStressView : Array StressTrack -> Html Msg
readOnlyStressView stressTracks =
    div []
        [ div [ css
                [ fontWeight bold
                , fontSize (Css.em 1.1)
                , marginTop (Css.em 1)
                ]
              ] [ text "Stress" ]
        , div [] (Array.toList
                      (Array.indexedMap
                           stressTrackView
                           stressTracks))
        ]

readOnlyConsequencesView : Array Consequence -> Html Msg
readOnlyConsequencesView consequences =
    let
        consequenceView (Consequence severity title) =
            div [ css
                  [ displayFlex
                  , alignItems center
                  ]
                ]
                [ div [ css
                        [ fontWeight bold
                        , marginRight (Css.em 0.5)
                        ]
                      ]
                      [ text (showSeverity severity) ]
                , div []
                    [ text title ]
                ]
    in
        if
            Array.isEmpty consequences
        then
            text ""
        else
            div []
                [ h2 [] [ text "Consequences" ]
                , div []
                    <| Array.toList
                        <| Array.map
                            consequenceView
                            consequences
                ]


readOnlyConditionsView : Array Condition -> Html Msg
readOnlyConditionsView conditions =
    if
        Array.isEmpty conditions
    then
        text ""
    else
        div []
            [ div [ css
                    [ fontWeight bold
                    , fontSize (Css.em 1.1)
                    , marginTop (Css.em 1)
                    ]
                  ] [ text "Conditions" ]
            , div [] (Array.toList
                          (Array.indexedMap
                               conditionView
                                   conditions))
            ]


readOnlyRefreshView : Int -> Html Msg
readOnlyRefreshView points =
    div [ css
              [ marginTop (Css.em 1)]
        ]
        [ span
              [ css
                [ fontSize (Css.em 1.1)
                , fontWeight bold
                ]
              ]
              [ text "Refresh: " ]
        , span
              []
              [ text (toString points) ]
        ]

