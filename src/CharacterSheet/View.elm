module CharacterSheet.View
    exposing
    ( editView
    , view
    , defaultButton
    , inputStyles
    )

import Array exposing (Array)
import CharacterSheet.Types exposing (..)
import CharacterSheet.Update exposing (..)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Util
    exposing
        ( listDifference
        , stringToNatWithDefault
        , stringToNatWithDefaultNonZero
        )


editView : Model -> Html Msg
editView model =
    div
        [ css
            [ maxWidth (Css.em 32)
            ]
        ]
        [ editNameView model.name
        , editDescriptionView model.description
        , editAspectView model.aspects
        , editSkillView model.skills
        , editStressView model.stress
        , editConsequenceView model.consequences
        , editConditionsView model.conditions
        , div
            [ css
                [ displayFlex
                , justifyContent spaceAround
                ]
            ]
            [ editRefreshView model.refresh
            , fatePointsView model.fatePoints
            ]
        , editStuntView model.stunts
        ]


editRefreshView : Int -> Html Msg
editRefreshView points =
    let
        decrementButton =
            defaultButton
                [ onClick
                    (UpdateRefresh
                        (Basics.max 1 (points - 1))
                    )
                , if points <= 1 then
                    css [ Css.property "visibility" "hidden" ]

                  else
                    css [ opacity (int 0) ]
                ]
                [ text "-" ]

        incrementButton =
            defaultButton
                [ onClick (UpdateRefresh (points + 1))
                , css [ opacity (int 0) ]
                ]
                [ text "+" ]
    in
    div
        [ css
            [ Css.width (pct 50)
            , marginTop (Css.em 1)
            ]
        , class "reveal-buttons-on-hover"
        ]
        [ sectionLabel "Refresh"
        , div
            [ css
                [ whiteSpace noWrap
                ]
            ]
            [ decrementButton
            , span
                [ css
                    [ fontSize (Css.em 1.3)
                    , margin2 (px 0) (Css.em 0.25)
                    ]
                ]
                [ text (String.fromInt points) ]
            , incrementButton
            ]
        ]


fatePointsView : Int -> Html Msg
fatePointsView points =
    let
        decrementButton =
            defaultButton
                [ onClick
                    (UpdateFatePoints
                        (Basics.max 0 (points - 1))
                    )
                , if points < 1 then
                    css [ Css.property "visibility" "hidden" ]

                  else
                    css [ opacity (int 0) ]
                ]
                [ text "-" ]

        incrementButton =
            defaultButton
                [ onClick (UpdateFatePoints (points + 1))
                , css [ opacity (int 0) ]
                ]
                [ text "+" ]
    in
    div
        [ css
            [ Css.width (pct 50)
            , marginTop (Css.em 1)
            ]
        , class "reveal-buttons-on-hover"
        ]
        [ sectionLabel "Fate Points"
        , div
            [ css
                [ whiteSpace noWrap
                , marginLeft (Css.em 0.6)
                ]
            ]
            [ decrementButton
            , span
                [ css
                    [ fontSize (Css.em 1.3)
                    , margin2 (px 0) (Css.em 0.25)
                    ]
                ]
                [ text (String.fromInt points) ]
            , incrementButton
            ]
        ]


editNameView : String -> Html Msg
editNameView name =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Name"
        , input
            [ type_ "text"
            , css [ inputStyles ]
            , onInput UpdateName
            , value name
            ]
            []
        ]


editDescriptionView : String -> Html Msg
editDescriptionView description =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Description"
        , textarea
            [ rows 5
            , css [ inputStyles ]
            , onInput UpdateDescription
            , value description
            ]
            []
        ]


editAspectView : Array Aspect -> Html Msg
editAspectView aspects =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Aspects"
        , div [] <|
            Array.toList <|
                Array.indexedMap
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
aspectInput index (Aspect title invokes) =
    div
        [ css
            [ displayFlex
            , alignItems center
            ]
        ]
        [ input
            [ type_ "text"
            , css [ inputStyles ]
            , onInput
                (\newTitle ->
                    UpdateAspect
                        index
                        (Aspect newTitle invokes)
                )
            , value title
            , placeholder <| "Aspect #" ++ String.fromInt (index + 1)
            ]
            []
        , defaultButton
            [ onClick (RemoveAspect index)
            , css
                [ marginLeft (Css.em 0.5) ]
            ]
            [ text "Remove" ]
        ]



-- Skill View


editSkillView : Array Skill -> Html Msg
editSkillView skills =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Skills"
        , div [] <|
            Array.toList <|
                Array.indexedMap
                    skillInput
                    skills
        , skillRatingButtonList skills
        ]


skillRatingButtonList : Array Skill -> Html Msg
skillRatingButtonList skills =
    div
        [ css
            [ displayFlex
            , alignItems center
            , flexWrap Css.wrap
            , marginTop (Css.em 1)
            ]
        ]
    <|
        List.map
            skillRatingButton
            (List.filter
                (\rating ->
                    skills
                        |> Array.toList
                        |> List.map (\(Skill a _) -> a)
                        |> List.member rating
                        |> not
                )
                skillRatingList
            )


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
    div
        [ css
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
        , input
            [ type_ "text"
            , css
                [ flex (int 1)
                , inputStyles
                ]
            , onInput
                (\newTitle ->
                    UpdateSkill
                        index
                        (Skill rating newTitle)
                )
            , value title
            ]
            []
        , defaultButton
            [ onClick (RemoveSkill index)
            , css
                [ marginLeft (Css.em 0.5) ]
            ]
            [ text "Remove" ]
        ]



-- Stunt View


editStuntView : Array Stunt -> Html Msg
editStuntView stunts =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Stunts"
        , div [] <|
            Array.toList <|
                Array.indexedMap
                    stuntInput
                    stunts
        , defaultButton
            [ onClick (AddNewStunt "" "") ]
            [ text "Add New Stunt" ]
        ]


stuntInput : Int -> Stunt -> Html Msg
stuntInput index (Stunt title description) =
    div
        [ css
            [ marginBottom (Css.em 1.25)
            ]
        ]
        [ input
            [ type_ "text"
            , css
                [ display block
                , inputStyles
                ]
            , onInput
                (\newTitle ->
                    UpdateStunt
                        index
                        (Stunt newTitle description)
                )
            , value title
            ]
            []
        , textarea
            [ onInput
                (\newDescription ->
                    UpdateStunt
                        index
                        (Stunt title newDescription)
                )
            , value description
            , rows 5
            , css
                [ display block
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


editStressView : Array StressTrack -> Html Msg
editStressView stressTracks =
    div []
    [ div [ css
            [ displayFlex
            , alignItems center
            , marginTop (Css.em 1)
            ]
          ] 
          [ sectionLabel "Stress" ]
    , p [] [ text "Use the Up and Down arrows on your keyboard to change the value of a stress box." ]
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
                               [ StressBox 1 False ]
                          )
                     )
                )
          ]
          [ text "Add new stress track" ] 
    ]


stressTrackView : Int -> StressTrack -> Html Msg
stressTrackView trackIndex (StressTrack title stressBoxes) =
    div []
        [ div [] [ text title ]
        , stressBoxView
            UpdateStressBox
            trackIndex
            stressBoxes
        ]


stressBoxView :
    (Int -> Int -> StressBox -> Msg)
    -> Int
    -> Array StressBox
    -> Html Msg
stressBoxView toggleStressBox trackIndex stressBoxes =
    div
        [ css
            [ displayFlex
            , flexWrap Css.wrap
            ]
        ]
        (Array.toList
            (Array.indexedMap
                (stressInput toggleStressBox trackIndex)
                stressBoxes
            )
        )


stressInput :
    (Int -> Int -> StressBox -> Msg)
    -> Int
    -> Int
    -> StressBox
    -> Html Msg
stressInput toggleStressBox trackIndex index (StressBox points isChecked) =
    label
        [ css
            [ fontSize (Css.em 1)
            , border3 (px 1) solid (hex "333")
            , padding2 (Css.em 0.1) (Css.em 0.6)
            , display inlineBlock
            , marginRight (Css.em 0.2)
            , borderRadius (px 4)
            , fontWeight (int 500)
            , userSelect_none
            , cursor pointer
            , if isChecked then
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
        [ text (String.fromInt points)
        , input
            [ type_ "checkbox"
            , HA.checked isChecked
            , onCheck
                (always
                    (toggleStressBox
                        trackIndex
                        index
                        (StressBox
                            points
                            (not isChecked)
                        )
                    )
                )
            , css
                [ position absolute
                , appearance_none
                , opacity (int 0)
                , Css.height (px 0)
                , Css.width (px 0)
                ]
            ]
            []
        ]


editStressTrackView : Int -> StressTrack -> Html Msg
editStressTrackView trackIndex (StressTrack title stressBoxes) =
    div
        [ css
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
                        (StressTrack newTitle stressBoxes)
                )
            ]
            []
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


editStressBoxView :
    (Int -> StressBox -> Msg)
    -> (Int -> Msg)
    -> (Int -> Int -> StressBox -> Msg)
    -> Int
    -> Array StressBox
    -> Html Msg
editStressBoxView addBox removeBox updateBox trackIndex stressBoxes =
    div
        [ css
            [ displayFlex
            , flexWrap Css.wrap
            , alignItems center
            , margin2 (Css.em 0.5) (px 0)
            ]
        ]
        (Array.toList
            (Array.indexedMap
                (editStressInput updateBox trackIndex)
                stressBoxes
            )
            ++ [ if Array.length stressBoxes > 1 then
                    removeBoxButton
                        [ onClick (removeBox trackIndex) ]
                        [ text "✕" ]

                 else
                    text ""
               , addNewBoxButton
                    [ onClick (addBox trackIndex (StressBox 1 False)) ]
                    [ text "+" ]
               ]
        )


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


editStressInput :
    (Int -> Int -> StressBox -> Msg)
    -> Int
    -> Int
    -> StressBox
    -> Html Msg
editStressInput updateBox trackIndex index (StressBox points isChecked) =
    input
        [ type_ "number"
        , value (String.fromInt points)
        , onInput
            (\newPoints ->
                updateBox
                    trackIndex
                    index
                    (StressBox
                        (stringToNatWithDefaultNonZero
                            points
                            newPoints
                        )
                        isChecked
                    )
            )
        , css
            [ fontSize (Css.em 1)
            , border3 (px 1) solid (hex "333")
            , padding2 (Css.em 0.25) (Css.em 0.5)
            , display inline
            , marginRight (Css.em 0.5)
            , borderRadius (px 4)
            , fontWeight bold
            , backgroundColor (hex "fff")
            , color (hex "333")
            , Css.width (Css.em 3.5)
            ]
        ]
        []


-- toggleSwitch : EditMode -> Bool -> Html Msg
-- toggleSwitch mode isActive =
--     span
--         [ css
--             [ displayFlex
--             , alignItems center
--             ]
--         ]
--         [ label
--             [ css
--                 [ position relative
--                 , display inlineBlock
--                 , Css.width (px 60)
--                 , Css.height (px 34)
--                 , Css.property "transform" "scale(0.65)"
--                 ]
--             ]
--             [ input
--                 [ type_ "checkbox"
--                 , HA.checked isActive
--                 , onCheck (always (ToggleEditMode mode))
--                 , css [ display none ]
--                 ]
--                 []
--             , span
--                 [ css
--                     [ position absolute
--                     , cursor pointer
--                     , top (px 0)
--                     , left (px 0)
--                     , right (px 0)
--                     , bottom (px 0)
--                     , if isActive then
--                         batch
--                             [ backgroundColor (hex "2196F3")
--                             , before
--                                 [ Css.property
--                                     "-webkit-transform"
--                                     "translateX(26px)"
--                                 , Css.property
--                                     "-ms-transform"
--                                     "translateX(26px)"
--                                 , Css.property
--                                     "transform"
--                                     "translateX(26px)"
--                                 ]
--                             ]

--                       else
--                         backgroundColor (hex "ccc")
--                     , Css.property "-webkit-transition" "0.2s"
--                     , Css.property "transition" "0.2s ease"
--                     , borderRadius (px 34)
--                     , before
--                         [ position absolute
--                         , Css.property "content" "\"\""
--                         , Css.height (px 26)
--                         , Css.width (px 26)
--                         , left (px 4)
--                         , bottom (px 4)
--                         , backgroundColor (hex "fff")
--                         , Css.property "-webkit-transition" "0.2s"
--                         , Css.property "transition" "0.2s ease"
--                         , borderRadius (pct 50)
--                         ]
--                     ]
--                 ]
--                 []
--             ]
--         , span
--             [ css
--                 [ fontSize (pct 80)
--                 , color (hex "888")
--                 ]
--             ]
--             [ text
--                 (if isActive then
--                     "Unlocked"

--                  else
--                     "Locked"
--                 )
--             ]
--         ]



-- Consequences


editConsequenceView : Array Consequence -> Html Msg
editConsequenceView consequences =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Consequences"
        , div [] <|
            Array.toList <|
                Array.indexedMap
                    consequenceInput
                    consequences
        , consequenceButtonList consequences
        ]


consequenceInput : Int -> Consequence -> Html Msg
consequenceInput index (Consequence severity title) =
    div
        [ css
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
            [ text (showSeverity severity) ]
        , input
            [ type_ "text"
            , css
                [ flex (int 1)
                , inputStyles
                ]
            , onInput
                (\newTitle ->
                    UpdateConsequence
                        index
                        (Consequence severity newTitle)
                )
            , value title
            ]
            []
        , defaultButton
            [ onClick (RemoveConsequence index)
            , css
                [ marginLeft (Css.em 0.5) ]
            ]
            [ text "Remove" ]
        ]


consequenceButtonList : Array Consequence -> Html Msg
consequenceButtonList consequences =
    div
        [ css
            [ displayFlex
            , alignItems center
            , flexWrap Css.wrap
            , marginTop (Css.em 1)
            ]
        ]
    <|
        List.map
            consequenceSeverityButton
            (listDifference
                consequenceSeverityList
                (List.map
                    (\(Consequence a _) -> a)
                    (Array.toList consequences)
                )
            )


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


editConditionsView : Array Condition -> Html Msg
editConditionsView conditions =
    div []
    [ div [ css
            [ displayFlex
            , alignItems center
            , marginTop (Css.em 1)
            ]
          ]
          [ sectionLabel "Conditions" ]
    ,  p [] [ text "Use the Up and Down arrows on your keyboard to change the value of a shift box." ]
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
                               [ StressBox 1 False ]
                          )
                     )
                )
          ]
          [ text "Add New Condition" ]
    ]


conditionView : Int -> Condition -> Html Msg
conditionView trackIndex (Condition title stressBoxes) =
    div
        [ css
            [ displayFlex
            , alignItems center
            , flexWrap Css.wrap
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
    div
        [ css
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
                        (Condition newTitle stressBoxes)
                )
            ]
            []
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


view : Model -> Html Msg
view model =
    div
        [ css
            [ padding3 (px 0) (Css.em 1) (Css.em 1)
            , overflowWrap breakWord
            ]
        ]
        [ nameView model.name
        , descriptionView model.description
        , aspectView model.aspects
        , skillsView model.skills
        , stressView model.stress
        , consequencesView model.consequences
        , conditionsView model.conditions
        , div
            [ css
                [ displayFlex
                , justifyContent spaceAround
                ]
            ]
            [ refreshView model.refresh
            , fatePointsView model.fatePoints
            ]
        , stuntsView model.stunts
        ]


sectionLabel : String -> Html Msg
sectionLabel title =
    div
        [ css
            [ fontSize (Css.em 1)
            , color (hex "555")
            , Css.property "font-variant" "small-caps"
            , Css.property "letter-spacing" "0.1em"
            , fontWeight bold
            ]
        ]
        [ text title ]


nameView : String -> Html Msg
nameView name =
    div
        [ css
            [ fontWeight bold
            , fontSize (Css.em 1.2)
            , position sticky
            , top (px 0)
            , backgroundColor (hex "fff")
            , borderBottom3 (px 1) solid (hex "ccc")
            , marginBottom (Css.em 0.25)
            , padding3 (Css.em 0.5) (px 0) (Css.em 0.25)
            ]
        ]
        [ text name ]


descriptionView : String -> Html Msg
descriptionView description =
    div [] [ text description ]


aspectView : Array Aspect -> Html Msg
aspectView aspects =
    let
        aspectView_ index (Aspect title invokes) =
            div
                [ css
                    [ displayFlex
                    , alignItems center
                    , justifyContent spaceBetween
                    ]
                , class "reveal-buttons-on-hover"
                ]
                [ span [] [ text title ]
                , invokesView index (Aspect title invokes)
                ]
    in
    if Array.isEmpty aspects then
        text ""

    else
        div
            [ css
                [ marginTop (Css.em 1) ]
            ]
            [ sectionLabel "Aspects"
            , div [] <|
                Array.toList <|
                    Array.indexedMap
                        aspectView_
                        aspects
            ]


invokesView : Int -> Aspect -> Html Msg
invokesView index (Aspect title invokes) =
    let
        invokeButton =
            styled defaultButton
                [ opacity (int 0)
                , hover
                    [ opacity (int 1) ]
                ]

        addInvokeButton =
            invokeButton
                [ onClick
                    (UpdateAspect
                        index
                        (Aspect title (invokes + 1))
                    )
                ]
                [ text "+" ]

        removeInvokeButton =
            invokeButton
                [ onClick
                    (UpdateAspect
                        index
                        (Aspect title (invokes - 1))
                    )
                ]
                [ text "-" ]

        content =
            if invokes > 0 then
                [ removeInvokeButton
                , span
                    [ css
                        [ backgroundColor (hex "663399")
                        , color (hex "fff")
                        , borderRadius (px 999)
                        , padding2 (Css.em 0.25) (Css.em 0.6)
                        , fontSize (Css.em 0.75)
                        ]
                    ]
                    [ text (String.fromInt invokes) ]
                , addInvokeButton
                ]

            else
                [ addInvokeButton ]
    in
    span
        [ css
            [ whiteSpace noWrap
            , userSelect_none
            ]
        ]
        content


skillsView : Array Skill -> Html Msg
skillsView skills =
    let
        skillView_ (Skill rating title) =
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
    if Array.isEmpty skills then
        text ""

    else
        div
            [ css
                [ marginTop (Css.em 1) ]
            ]
            [ sectionLabel "Skills"
            , div [] <|
                Array.toList <|
                    Array.map
                        skillView_
                        skills
            ]


stuntsView : Array Stunt -> Html Msg
stuntsView stunts =
    let
        stuntView_ (Stunt title description) =
            div
                [ css
                    [ marginBottom (Css.em 0.5)
                    ]
                ]
                [ span
                    [ css
                        [ fontWeight bold
                        ]
                    ]
                    [ text (title ++ ": ") ]
                , span [] [ text description ]
                ]
    in
    if Array.isEmpty stunts then
        text ""

    else
        div
            [ css
                [ marginTop (Css.em 1) ]
            ]
            [ sectionLabel "Stunts"
            , div [] <|
                Array.toList <|
                    Array.map
                        stuntView_
                        stunts
            ]


stressView : Array StressTrack -> Html Msg
stressView stressTracks =
    if Array.isEmpty stressTracks then
        text ""

    else
        div
            [ css
                [ marginTop (Css.em 1) ]
            ]
            [ sectionLabel "Stress"
            , div []
                (Array.toList
                    (Array.indexedMap
                        stressTrackView
                        stressTracks
                    )
                )
            ]


consequencesView : Array Consequence -> Html Msg
consequencesView consequences =
    let
        consequenceView_ (Consequence severity title) =
            div
                [ css
                    [ displayFlex
                    , alignItems center
                    ]
                ]
                [ div
                    [ css
                        [ fontWeight bold
                        , marginRight (Css.em 0.5)
                        ]
                    ]
                    [ text (showSeverity severity) ]
                , div []
                    [ text title ]
                ]
    in
    if Array.isEmpty consequences then
        text ""

    else
        div
            [ css
                [ marginTop (Css.em 1) ]
            ]
            [ sectionLabel "Consequences"
            , div [] <|
                Array.toList <|
                    Array.map
                        consequenceView_
                        consequences
            ]


conditionsView : Array Condition -> Html Msg
conditionsView conditions =
    if Array.isEmpty conditions then
        text ""

    else
        div
            [ css
                [ marginTop (Css.em 1) ]
            ]
            [ sectionLabel "Conditions"
            , div []
                (Array.toList
                    (Array.indexedMap
                        conditionView
                        conditions
                    )
                )
            ]


refreshView : Int -> Html Msg
refreshView points =
    div
        [ css
            [ Css.width (pct 50)
            , marginTop (Css.em 1)
            ]
        ]
        [ sectionLabel "Refresh"
        , div
            [ css
                [ fontSize (Css.em 1.2)
                , marginLeft (Css.em 1.4)
                ]
            ]
            [ text (String.fromInt points) ]
        ]


appearance_none : Style
appearance_none =
    batch
        [ Css.property "-webkit-appearance" "none"
        , Css.property "-moz-appearance" "none"
        , Css.property "appearance" "none"
        ]
