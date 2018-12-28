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

module RedMarkets.CharacterSheet.View
    exposing
    ( editView
    , view
    , compactView
    , inputStyles
    , sectionLabel
    , defaultButton
    )

import Array exposing (Array)
import RedMarkets.CharacterSheet.Types exposing (..)
import Css exposing (..)
import Util.Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Util
    exposing
        ( listDifference
        , stringToNatWithDefault
        , stringToNatWithDefaultNonZero
        , arrayAll
        , arrayAny
        )


editView : Model -> Html Msg
editView model =
    div []
        [ editNameView model.name
        , editDescriptionView model.description
        , editCrewView model.crew
        , editWeakSpotView model.weakSpot
        , editSoftSpotView model.softSpot
        , editToughSpotView model.toughSpot
        , editPotentialsView model
        , editDependantsView model.dependants
        , editReferencesView model.references
        , editThreatsView model
        , editWoundsView model
        , editGearListView model.gear
        , editNotesView model.notes
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

editCrewView : String -> Html Msg
editCrewView crew =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Crew"
        , input
            [ type_ "text"
            , css [ inputStyles ]
            , onInput UpdateCrew
            , value crew
            ]
            []
        ]


editWeakSpotView : String -> Html Msg
editWeakSpotView weakSpot =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Weak Spot"
        , input
            [ type_ "text"
            , css [ inputStyles ]
            , onInput UpdateWeakSpot
            , value weakSpot
            ]
            []
        ]


editSoftSpotView : String -> Html Msg
editSoftSpotView softSpot =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Soft Spot"
        , input
            [ type_ "text"
            , css [ inputStyles ]
            , onInput UpdateSoftSpot
            , value softSpot
            ]
            []
        ]


editToughSpotView : String -> Html Msg
editToughSpotView toughSpot =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Tough Spot"
        , input
            [ type_ "text"
            , css [ inputStyles ]
            , onInput UpdateToughSpot
            , value toughSpot
            ]
            []
        ]


editPotentialsView : { r |
                       str : Potential
                     , spd : Potential
                     , adp : Potential
                     , int : Potential
                     , cha : Potential
                     , wil : Potential
                     }
                   -> Html Msg
editPotentialsView { str, spd, adp, int, cha, wil } =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Potentials"
        ]


editDependantsView : Array Relationship -> Html Msg
editDependantsView dependants =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Dependants"
        ]


editReferencesView : Array Relationship -> Html Msg
editReferencesView references =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "References"
        ]


editThreatsView : { r |
                    detachment : Threat
                  , stress : Threat
                  , trauma : Threat
                  }
                -> Html Msg
editThreatsView { detachment, stress, trauma } =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Threats"
        ]


editWoundsView : { r |
                   rightLegWounds : Array Wound
                 , leftLegWounds : Array Wound
                 , rightArmWounds : Array Wound
                 , leftArmWounds : Array Wound
                 , torsoWounds : Array Wound
                 , headWounds : Array Wound
                 }
               -> Html Msg
editWoundsView { rightLegWounds
               , leftLegWounds
               , rightArmWounds
               , leftArmWounds
               , torsoWounds
               , headWounds
               } =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Wounds"
        ]


editGearListView : Array Gear -> Html Msg
editGearListView gear =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Gear"
        , div [] (Array.toList (Array.map editGearView gear))
        ]


editGearView : Gear -> Html Msg
editGearView { title
             , charges
             , upkeep
             , effect
             , qualities
             , upgrades
             } =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ div [] [ text title ]
        ]


editNotesView : String -> Html Msg
editNotesView notes =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
        [ sectionLabel "Notes"
        , textarea
            [ rows 10
            , css [ inputStyles ]
            , onInput UpdateNotes
            , value notes
            ]
            []
        ]



-- Extra Styles

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


compactView : Model -> Html Msg
compactView model =
    div
        [ css
            [ padding3 (px 0) (Css.em 1) (Css.em 1)
            , overflowWrap breakWord
            ]
        ]
        [ nameView model.name
        , descriptionView model.description
        , div [ css
                [ marginTop (Css.em 1) ]
              ]
              [ crewView model.crew
              , weakSpotView model.weakSpot
              , softSpotView model.softSpot
              , toughSpotView model.toughSpot
              ]
        ]


view : Model -> Html Msg
view model =
    div
        [ css
            [ overflowWrap breakWord
            ]
        ]
        [ nameView model.name
        , descriptionView model.description
        , div [ css
                [ marginTop (Css.em 1) ]
              ]
              [ crewView model.crew
              , weakSpotView model.weakSpot
              , softSpotView model.softSpot
              , toughSpotView model.toughSpot
              ]
        , potentialsView model
        , dependantsView model.dependants
        , referencesView model.references
        , threatsView model
        , woundsView model
        , gearListView model.gear
        , notesView model.notes
        ]


sectionLabel : String -> Html msg
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
            , backgroundColor (hex "fff")
            , borderBottom3 (px 1) solid (hex "ccc")
            , marginBottom (Css.em 0.25)
            , padding3 (Css.em 0.5) (px 0) (Css.em 0.25)
            ]
        ]
        [ text name ]


descriptionView : String -> Html Msg
descriptionView description =
    div [ css
          [ whiteSpace preWrap
          ]
        ]
        [ text description ]


crewView : String -> Html Msg
crewView crew =
    div []
    [ span [ css [ fontWeight bold ]
           ] [ text "Crew: " ]
    , text crew
    ]


weakSpotView : String -> Html Msg
weakSpotView weakSpot =
    div []
    [ span [ css [ fontWeight bold ]
           ] [ text "Weak Spot: " ]
    , text weakSpot
    ]


softSpotView : String -> Html Msg
softSpotView softSpot =
    div []
    [ span [ css [ fontWeight bold ]
           ] [ text "Soft Spot: " ]
    , text softSpot
    ]


toughSpotView : String -> Html Msg
toughSpotView toughSpot =
    div []
    [ span [ css [ fontWeight bold ]
           ] [ text "Tough Spot: " ]
    , text toughSpot
    ]


potentialsView : { r |
                   str : Potential
                 , spd : Potential
                 , adp : Potential
                 , int : Potential
                 , cha : Potential
                 , wil : Potential
                 }
               -> Html Msg
potentialsView { str, spd, adp, int, cha, wil } =
    let
        potentials =
            [ ("Strength" , str)
            , ("Speed", spd)
            , ("Adaptability", adp)
            , ("Intelligence", int)
            , ("Charm", cha)
            , ("Will", wil)
            ]
    in
        div [ css
              [ marginTop (Css.em 1)
              , Css.property "display" "grid"
              , Css.property "grid-template-columns" "repeat(2, 1fr)"
              , Css.property "grid-gap" "0.65em"
              ]
            ]
        (List.map (\(name, p) -> potentialView name p) potentials)


potentialView : String -> Potential -> Html Msg
potentialView name (Potential rating skills) =
    div []
    [ div [ css
            [ fontSize (Css.em 1.1)
            , fontWeight bold
            ]
          ]
          [ text (name ++ " " ++ (String.fromInt rating)) ]
    , div []
        (Array.toList (Array.map skillView skills))
    ]


skillView : Skill -> Html Msg
skillView (Skill name rating) =
    div [] [ text (name ++ " " ++ (String.fromInt rating)) ]


dependantsView : Array Relationship -> Html Msg
dependantsView dependants =
    div [ css
          [ marginTop (Css.em 1) ]
        ]
    [ sectionLabel "Dependants"
    , div [] (Array.toList (Array.map relationshipView dependants))
    ]


referencesView : Array Relationship -> Html Msg
referencesView references =
    div [ css
          [ marginTop (Css.em 1) ]
        ]
    [ sectionLabel "References"
    , div [] (Array.toList (Array.map relationshipView references))
    ]


relationshipView : Relationship -> Html Msg
relationshipView (Relationship person status) =
    div [ css
          [Css.property "display" "grid"
          , Css.property "grid-template-columns" "2fr 1fr"
          , Css.property "grid-gap" "1em"
          ]
        ]
    [ div [] [ text person ]
    , div [] [ text (showRelationStatus status) ]
    ]


threatsView : { r |
                detachment : Threat
              , stress : Threat
              , trauma : Threat
              }
            -> Html Msg
threatsView { detachment, stress, trauma } =
    let
        threats =
            [ ("Detachment", detachment)
            , ("Stress", stress)
            , ("Trauma", trauma)
            ]
    in
        div [ css
              [ marginTop (Css.em 1) ]
            ]
        [ sectionLabel "Threats"
        , div []
            (List.map (\(name, t) -> threatView name t) threats)
        ]

threatView : String -> Threat -> Html Msg
threatView title (Threat rating) =
    let
        tag =
            if rating >= 0 && rating < 5
            then
                ""
            else
                if rating >= 5 && rating < 10
                then
                    "(Crack)"
                else
                    if rating >= 10 && rating < 15
                    then
                        "(Crumble)"
                    else
                        if rating == 15
                        then
                            "(Break)"
                        else
                            ""
    in
    div []
        [ text
              (title ++ ": " ++
               (String.fromInt rating) ++ " " ++ tag)
        ]


woundsView : { r |
               rightLegWounds : Array Wound
             , leftLegWounds : Array Wound
             , rightArmWounds : Array Wound
             , leftArmWounds : Array Wound
             , torsoWounds : Array Wound
             , headWounds : Array Wound
             }
           -> Html Msg
woundsView { rightLegWounds
           , leftLegWounds
           , rightArmWounds
           , leftArmWounds
           , torsoWounds
           , headWounds
           } =
    let
        wounds =
            [ (Head, headWounds)
            , (RightArm, rightArmWounds)
            , (Torso, torsoWounds)
            , (LeftArm, leftArmWounds)
            , (RightLeg, rightLegWounds)
            , (LeftLeg, leftLegWounds)
            ]
    in
        div [ css
              [ marginTop (Css.em 1) ]
            ]
        [ sectionLabel "Wounds"
        , div [ css
                [ Css.property "display" "grid"
                , Css.property
                    "grid-template-columns"
                    "repeat(3, 1fr)"
                , Css.property
                    "grid-template-rows"
                    "repeat(3, auto)"
                , Css.property "grid-row-gap" "1em"
                ]
              ]
            (List.map
                 (\(loc, w) -> woundLocationView loc w)
                 wounds)
        , div [ css
                [ marginTop (Css.em 1) ]
              ]
            (List.map
                 (\(loc, w) -> woundStatusEffect loc w)
                 wounds)
        ]


woundLocationView : WoundLocation -> Array Wound -> Html Msg
woundLocationView location wounds =
    let
        gridPosition =
            case location of
                Head ->
                    batch
                    [ Css.property "grid-row" "1"
                    , Css.property "grid-column" "2/3"
                    ]

                RightArm ->
                    batch
                    [ Css.property "grid-row" "2"
                    , Css.property "grid-column" "1/2"
                    ]

                Torso ->
                    batch
                    [ Css.property "grid-row" "2"
                    , Css.property "grid-column" "2/3"
                    ]

                LeftArm ->
                    batch
                    [ Css.property "grid-row" "2"
                    , Css.property "grid-column" "3"
                    ]

                RightLeg ->
                    batch
                    [ Css.property "grid-row" "3"
                    , Css.property "grid-column" "1/2"
                    ]

                LeftLeg ->
                    batch
                    [ Css.property "grid-row" "3"
                    , Css.property "grid-column" "3"
                    ]
    in
    div [ css [ gridPosition ] ]
    [ div [] [ text (showWoundLocation location) ]
    , div [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "repeat(5, 1.5em)"
            , Css.property "grid-auto-rows" "auto"
            , Css.property "grid-gap" "0.2em"
            ]
          ]
        (Array.toList
             (Array.indexedMap (woundView location) wounds))
    ]


woundStatusEffect : WoundLocation -> Array Wound -> Html Msg
woundStatusEffect location wounds =
    -- if there is at least one empty wound slot
    if arrayAny ((==) NoWound) wounds
    then
        -- there are no status effects
        text ""
    else
        -- if all slots are filled with kill wounds
        if arrayAll ((==) Kill) wounds
        then
            -- there is a kill status effect
            killStatusEffect location
        else
            -- otherwise, there is a stun status effect
            stunStatusEffect location

stunStatusEffect : WoundLocation -> Html Msg
stunStatusEffect location =
    let
        titleStyles =
            css
            [ fontWeight bold
            ]

        hobbled at =
            [ div [ titleStyles ]
                  [ text ("Hobbled (" ++ at ++ ")") ]
            , bulletedListView
              [ text "No athletics checks possible until partially healed, but character can still move"
              ]
            ]

        winged at =
            [ div [ titleStyles ]
                  [ text ("Winged (" ++ at ++ ")") ]
            , bulletedListView
              [ text "No cumbersome weapons or gear available"
              , text "If this hand is dominant, all checks with the other arm are at Precision requirements"
              ]
            ]

        gassed =
            [ div [ titleStyles ]
                  [ text "Gassed (Torso)" ]
            , bulletedListView
              [ text "The character becomes Gassed"
              ]
            ]

        unconscious =
            [ div [ titleStyles ]
                  [ text "Unconscious (Head)" ]
            , bulletedListView
              [ text "The character is Unconscious"
              ]
            ]
    in
    case location of
        Head ->
            div [] unconscious

        RightArm ->
            div [] (winged "Right Arm")

        Torso ->
            div [] gassed

        LeftArm ->
            div [] (winged "Left Arm")

        RightLeg ->
            div [] (hobbled "Right Leg")

        LeftLeg ->
            div [] (hobbled "Left Leg")


killStatusEffect : WoundLocation -> Html Msg
killStatusEffect location =
    let
        titleStyles =
            css
            [ fontWeight bold
            ]

        lamed at =
            [ div [ titleStyles ]
                  [ text ("Lamed (" ++ at ++ ")") ]
            , bulletedListView
              [ text "Bleeding out"
              , text "No athletics checks possible until partially healed, and character can't move without assistance"
              ]
            ]

        maimed at =
            [ div [ titleStyles ]
                  [ text ("Maimed (" ++ at ++ ")") ]
            , bulletedListView
              [ text "No cumbersome weapons or gear available"
              , text "If this hand is dominant, all checks with the other arm are at Precision requirements"
              ]
            ]

        death at =
            [ div [ titleStyles ]
                  [ text ("Death (" ++ at ++ ")") ]
            , bulletedListView
              [ text "The character Dies."
              ]
            ]
    in
    case location of
        Head ->
            div [] (death "Head")

        RightArm ->
            div [] (maimed "Right Arm")

        Torso ->
            div [] (death "Torso")

        LeftArm ->
            div [] (maimed "Left Arm")

        RightLeg ->
            div [] (lamed "Right Leg")

        LeftLeg ->
            div [] (lamed "Left Leg")


bulletedListView : List (Html msg) -> Html msg
bulletedListView items =
    ul []
    (List.map
         (\item ->
              li [ css
                   [ marginBottom (Css.em 0.25) ]
                 ]
              [ item ])
         items)


woundView : WoundLocation -> Index -> Wound -> Html Msg
woundView location index wound =
    label
        [ css
            [ fontSize (Css.em 1)
            , Css.width (Css.em 1.5)
            , Css.height (Css.em 1.5)
            , border3 (px 1) solid (hex "333")
            , fontWeight (int 500)
            , userSelect_none
            , cursor pointer
            , case wound of
                  NoWound ->
                      batch
                      [ backgroundColor transparent
                      ]
                  Stun ->
                      batch
                      [ Css.property
                          "background"
                          "linear-gradient(135deg, #333 50%, transparent 50%)"
                      ]
                  Kill ->
                      batch
                      [ backgroundColor (hex "333")
                      ]
            ]
        ]
        [ text ""
        , input
            [ type_ "checkbox"
            , onCheck
                (always
                    (UpdateWound
                         location
                         index
                         (woundSucc wound))
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


gearListView : Array Gear -> Html Msg
gearListView gear =
    div
        [ css
            [ marginTop (Css.em 1) ]
        ]
    [ sectionLabel "Gear"
    , div [] (Array.toList (Array.indexedMap gearView gear))
    ]


gearView : Index -> Gear -> Html Msg
gearView gearIndex { title
               , charges
               , upkeep
               , effect
               , qualities
               , upgrades
               } =
    div
        [ css
          [ marginBottom (Css.em 1)
          ]
        ]
        [ div [ css
                [ fontSize (Css.em 1.1)
                , fontWeight bold
                ]
              ]
              [ text title ]
        , div [ css
                [ displayFlex
                , alignItems center
                ]
              ]
            [ div [ css
                    [ marginRight (Css.em 0.65)
                    ]
                  ]
                  [ text "Charges: " ]
            , gearChargesView gearIndex charges
            ]
        , div []
            [ span [] [ text "Upkeep: "]
            , text (String.fromInt upkeep)
            ]
        , div []
            [ span [ css [ fontWeight bold ] ]
                  [ text "Effect: "]
            , text effect
            ]
        , div []
            [ span [ css [ fontWeight bold ] ]
                  [ text "Qualities: "]
            , text qualities
            ]
        , div []
            [ span [ css [ fontWeight bold ] ]
                  [ text "Upgrades: "]
            , text upgrades
            ]
        ]


gearChargesView : Index -> Array Charge -> Html Msg
gearChargesView gearIndex charges =
    div [ css
          [ Css.property "display" "grid"
          , Css.property "grid-template-columns" "repeat(10, 1.2em)"
          , Css.property "grid-auto-rows" "1.2em"
          , Css.property "grid-gap" "0.25em"
          ]
        ]
    (Array.toList
         (Array.indexedMap (chargeInputView gearIndex) charges))


chargeInputView : Index -> Index -> Charge -> Html Msg
chargeInputView gearIndex chargeIndex charge =
    label
        [ css
            [ borderRadius (pct 50)
            , cursor pointer
            , case charge of
                  Charge ->
                      batch
                      [ backgroundColor (hex "663399")
                      , border3 (px 1) solid transparent
                      ]
                  NoCharge ->
                      batch
                      [ backgroundColor transparent
                      , border3 (px 1) dashed (hex "333")
                      ]
            ]
        ]
        [ text ""
        , input
            [ type_ "checkbox"
            , onCheck
                (always
                    (UpdateGearCharge
                         { gearIndex = gearIndex
                         , chargeIndex = chargeIndex
                         , charge = toggleCharge charge
                         })
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


toggleCharge : Charge -> Charge
toggleCharge charge =
    case charge of
        Charge ->
            NoCharge

        NoCharge ->
            Charge


notesView : String -> Html Msg
notesView notes =
    div
        [ css
            [ marginTop (Css.em 1)
            ]
        ]
        [ sectionLabel "Notes"
        , div [ css
                [ whiteSpace preWrap
                ]
              ]
              [ text notes ]
        ]


integerInput : { toMsg : Int -> msg
               , mMinBound : Maybe Int
               , mMaxBound : Maybe Int
               , currentValue : Int
               }
             -> Html msg
integerInput { toMsg
             , mMinBound
             , mMaxBound
             , currentValue
             } =
    let
        minBound =
            case mMinBound of
                Just min -> min
                Nothing -> Basics.round (-1/0)

        maxBound =
            case mMaxBound of
                Just max -> max
                Nothing -> Basics.round (1/0)

        decrementButton =
            defaultButton
                [ onClick
                    (toMsg
                        (Basics.max minBound (currentValue - 1))
                    )
                , if currentValue <= minBound then
                    css [ Css.property "visibility" "hidden" ]
                  else
                    css [ opacity (int 0) ]
                ]
                [ text "-" ]

        incrementButton =
            defaultButton
                [ onClick
                      (toMsg
                           (Basics.min maxBound (currentValue + 1))
                      )
                , if currentValue >= maxBound then
                    css [ Css.property "visibility" "hidden" ]
                  else
                    css [ opacity (int 0) ]
                ]
                [ text "+" ]
    in
        div [ css
              [ whiteSpace noWrap
              , userSelect_none
              , textAlign center
              ]
            , class "reveal-buttons-on-hover"
            ]
        [ decrementButton
        , span
              [ css
                [ fontSize (Css.em 1.3)
                , margin2 (px 0) (Css.em 0.25)
                ]
              ]
              [ text (String.fromInt currentValue) ]
        , incrementButton
        ]
