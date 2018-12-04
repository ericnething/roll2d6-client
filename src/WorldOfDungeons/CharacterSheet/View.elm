module WorldOfDungeons.CharacterSheet.View
    exposing
    ( editView
    , view
    )

import Array exposing (Array)
import WorldOfDungeons.CharacterSheet.Types exposing (..)
import Css exposing (..)
import Util.Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)

view : Model -> Html Msg
view model =
    div [ css
          [ padding3 (px 0) (Css.em 1) (Css.em 1)
          , overflowWrap breakWord
          ]
        ]
    [ nameView model
    , div [ css
            [ displayFlex
            , justifyContent spaceBetween
            , textAlign center
            ]
          ]
          [ classAndLevelView model
          , hitDiceView model
          , hitPointsView model
          ]
    , attributesView model
    , skillsView model
    , abilitiesView model
    , weaponsView model
    , div [ css
            [ displayFlex
            , justifyContent spaceBetween
            , textAlign center
            ]
          ]
        [ armorView model
        , shieldView model
        , totalArmorView model
        ]
    , equipmentView model
    , notesView model
    , div [ css
            [ displayFlex
            , justifyContent spaceBetween
            ]
          ]
          [ coinView model
          , xpView model
          ]
    ]

nameView : {r | name : String } -> Html Msg
nameView { name } =
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

classAndLevelView : {r | class : String, level : Int } -> Html Msg
classAndLevelView { class, level } =
    div []
        [ sectionLabel "Class/Level"
        , div []
            [ text (class ++ " " ++ String.fromInt level)
            ]
        ]

attributesView : Attributes r -> Html Msg
attributesView { str, dex, con, int, wis, cha } =
    let
        attributes =
            [ ("STR", str)
            , ("DEX", dex)
            , ("CON", con)
            , ("INT", int)
            , ("WIS", wis)
            , ("CHA", cha)
            ]

        attributeView (name, attr) =
            div
            [ css
              [ textAlign center
              , border3 (Css.em 0.075) solid (hex "555")
              , padding (Css.em 0.3)
              , borderRadius2 (Css.em 0.6) (px 0)
              ]
            ]
            [ div [ css
                    [ fontSize (Css.em 1.3)
                    ]
                  ]
                [ text (String.fromInt attr) ]
            , div [ css
                    [ Css.property "font-variant" "small-caps"
                    , letterSpacing (Css.em 0.15)
                    , fontWeight bold
                    ]
                  ]
                  [ text name ]
            ]
    in
        div []
            [ sectionLabel "Attributes"
            , div
              [ css
                [ displayFlex
                , alignItems center
                , justifyContent spaceAround
                -- , lineHeight (num 1)
                , margin3 (Css.em 0.25) (px 0) (Css.em 1)
                ]
              ]
              (List.map attributeView attributes)
            ]



skillsView : {r | skills : String } -> Html Msg
skillsView { skills } =
    div []
        [ sectionLabel "Skills"
        , skills
            |> text
        ]

abilitiesView : {r | abilities : Array Ability } -> Html Msg
abilitiesView { abilities } =
    div []
        [ sectionLabel "Abilities"
        , div []
            (abilities
            |> Array.map abilityView
            |> Array.toList)
        ]

abilityView : Ability -> Html Msg
abilityView (Ability name description) =
    div [ css
          [ marginBottom (Css.em 0.5)
          ]
        ]
        [ span [ css
                 [ fontWeight bold
                 ]
               ]
              [ text (name ++ ": ") ]
        , text description
        ]

weaponsView : {r | weapons : String } -> Html Msg
weaponsView { weapons } =
    div []
        [ sectionLabel "Weapons"
        , text weapons
        ]

equipmentView : {r | equipment : String } -> Html Msg
equipmentView { equipment } =
    div []
        [ sectionLabel "Equipment"
        , text equipment
        ]

armorView : {r | armor : Armor } -> Html Msg
armorView { armor } =
    div []
        [ sectionLabel "Armor & Speed"
        , div [] [ text (showArmor armor) ]
        ]

shieldView : {r | shield : Shield } -> Html Msg
shieldView { shield } =
    div []
        [ sectionLabel "Shield"
        , div []
            [ text (showShield shield) ]
        ]

totalArmorView : { r |
                   armor : Armor
                 , shield : Shield
                 , bonusArmor : Int
                 }
               -> Html Msg
totalArmorView { armor, shield, bonusArmor } =
    div []
        [ sectionLabel "Total Armor"
        , div [ css
                [ fontSize (Css.em 1.3) ]
              ]
            [ text (String.fromInt
                        (armorToInt armor +
                         shieldToInt shield +
                         bonusArmor))
            ]
        ]

hitDiceView : {r | hitDice : Int, con : Int } -> Html Msg
hitDiceView { hitDice, con } =
    div []
        [ sectionLabel "Hit Dice"
        , div [] [ text (String.fromInt (hitDice + con)) ]
        ]

hitPointsView : {r | hitPoints : Maybe Int } -> Html Msg
hitPointsView { hitPoints } =
    div []
        [ sectionLabel "Hit Points"
        , div [ css
                [ fontSize (Css.em 1.3) ]
              ]
            [ text (showMaybeInt "0" hitPoints)
            ]
        ]

coinView : {r | coin : Maybe Int } -> Html Msg
coinView { coin } =
    div [ css [ Css.width (pct 50) ] ]
        [ sectionLabel "Coin"
        , div [ css
                [ fontSize (Css.em 1.3)
                , backgroundColor (hex "eee")
                , padding2 (px 0) (Css.em 0.35)
                , Css.width (pct 90)
                ]
              ]
            [ text (showMaybeInt "0" coin) ]
        ]

xpView : {r | xp : Maybe Int } -> Html Msg
xpView { xp } =
    div [ css [ Css.width (pct 50) ] ]
        [ sectionLabel "XP"
        , div [ css
                [ fontSize (Css.em 1.3)
                , backgroundColor (hex "eee")
                , padding2 (px 0) (Css.em 0.35)
                , Css.width (pct 90)
                ]
              ]
            [ text (showMaybeInt "0" xp) ]
        ]

notesView : {r | notes : String } -> Html Msg
notesView { notes } =
    div []
        [ sectionLabel "Notes"
        , text notes
        ]


--------------------------------------------------
-- Edit View
--------------------------------------------------

editView : Model -> Html Msg
editView model =
    div [ css
          [ overflowWrap breakWord
          ]
        ]
    [ editNameView model
    , div [ css
            [ displayFlex
            ]
          ]
        [ editClassView model
        , editLevelView model
        ]
    , div [ css
            [ displayFlex
            ]
          ]
          [ editHitPointsView model
          , editHitDiceView model
          ]
    , editAttributesView model
    , editSkillsView model
    , editAbilitiesView model
    , editWeaponsView model
    , div [ css
            [ displayFlex
            , justifyContent spaceBetween
            ]
          ]
        [ editArmorView model
        , editShieldView model
        , editBonusArmorView model
        ]
    , editEquipmentView model
    , editNotesView model
    , div [ css
            [ displayFlex
            , justifyContent spaceBetween
            ]
          ]
        [ editCoinView model
        , editXpView model
        ]
    ]

editNameView : {r | name : String } -> Html Msg
editNameView { name } =
    div []
    [ sectionLabel "Name"
    , input
          [ type_ "text"
            , css [ inputStyles ]
            , onInput UpdateName
            , value name
          ] []
    ]

editClassView : {r | class : String } -> Html Msg
editClassView { class } =
    div [ css
          [ Css.width (pct 50)
          ]
        ]
    [ sectionLabel "Class"
    , input
          [ type_ "text"
            , css [ inputStyles ]
            , onInput UpdateClass
            , value class
          ] []
    ]

editLevelView : {r | level : Int } -> Html Msg
editLevelView { level } =
    div
        [ css
            [ Css.width (pct 50)
            , textAlign center
            ]
        , class "reveal-buttons-on-hover"
        ]
        [ sectionLabel "Level"
        , integerInput
              { onClickHandler = UpdateLevel
              , mMinBound = Just 1
              , mMaxBound = Just 10
              , currentValue = level
              }
        ]

editHitDiceView : {r | hitDice : Int, con : Int } -> Html Msg
editHitDiceView { hitDice, con } =
    let
        decrementButton =
            defaultButton
                [ onClick
                    (UpdateHitDice
                        (Basics.max (1 + con) (hitDice - 1))
                    )
                , if hitDice <= (1 + con) then
                    css [ Css.property "visibility" "hidden" ]
                  else
                    css [ opacity (int 0) ]
                ]
                [ text "-" ]

        incrementButton =
            defaultButton
                [ onClick (UpdateHitDice (hitDice + 1))
                , if hitDice >= 6 then
                    css [ Css.property "visibility" "hidden" ]
                  else
                    css [ opacity (int 0) ]
                ]
                [ text "+" ]
    in
    div
        [ css
            [ Css.width (pct 50)
            , textAlign center
            , userSelect_none
            ]
        , class "reveal-buttons-on-hover"
        ]
        [ sectionLabel "Hit Dice"
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
                [ text (String.fromInt (hitDice + con)) ]
            , incrementButton
            ]
        ]


editHitPointsView : {r | hitPoints : Maybe Int } -> Html Msg
editHitPointsView { hitPoints } =
    div [ css
          [ Css.width (pct 50)
          ]
        ]
    [ sectionLabel "Hit Points"
    , input
          [ type_ "text"
            , css [ inputStyles ]
            , onInput (UpdateHitPoints << filterToDigits)
            , value (showMaybeInt "" hitPoints)
          ] []
    ]

editAttributesView : Attributes r -> Html Msg
editAttributesView { str, dex, con, int, wis, cha } =
    div []
    [ sectionLabel "Attributes"
    , div [ css
            [ displayFlex
            , justifyContent spaceBetween
            , flexWrap Css.wrap
            ]
          ]
          [ editAttributeView "STR" str UpdateStr
          , editAttributeView "DEX" dex UpdateDex
          , editAttributeView "CON" con UpdateCon
          , editAttributeView "INT" int UpdateInt
          , editAttributeView "WIS" wis UpdateWis
          , editAttributeView "CHA" cha UpdateCha
          ]
    ]

editAttributeView : String -> Int -> (Int -> Msg) -> Html Msg
editAttributeView name value updateAttr =
    div
        [ css
            [ Css.width (pct 16.5)
            , textAlign center
            , margin2 (Css.em 0.8) (px 0)
            , userSelect_none
            ]
        , class "reveal-buttons-on-hover"
        ]
        [ integerInput
              { onClickHandler = updateAttr
              , mMinBound = Just 0
              , mMaxBound = Just 3
              , currentValue = value
              }
        , div [ css
                [ Css.property "font-variant" "small-caps"
                , letterSpacing (Css.em 0.15)
                , fontWeight bold
                ]
              ]
              [ text name ]
        ]


editSkillsView : {r | skills : String } -> Html Msg
editSkillsView { skills } =
    div []
    [ sectionLabel "Skills"
    , input
          [ type_ "text"
            , css [ inputStyles ]
            , onInput UpdateSkills
            , value skills
          ] []
    ]

editAbilitiesView : {r | abilities : Array Ability } -> Html Msg
editAbilitiesView { abilities } =
    div []
    [ sectionLabel "Abilities"
    , div []
        (Array.toList
             (Array.indexedMap
                  editAbilityView
                  abilities))
    , defaultButton
        [ onClick (AddNewAbility (Ability "" ""))
        ]
        [ text "Add new ability" ]
    ]

editAbilityView : Index -> Ability -> Html Msg
editAbilityView index (Ability title description) =
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
                    UpdateAbility
                        index
                        (Ability newTitle description)
                )
            , value title
            ]
            []
        , textarea
            [ onInput
                (\newDescription ->
                    UpdateAbility
                        index
                        (Ability title newDescription)
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
            [ onClick (RemoveAbility index)
            , css
                [ marginTop (Css.em 0.5)
                ]
            ]
            [ text "Remove" ]
        ]

editWeaponsView : {r | weapons : String } -> Html Msg
editWeaponsView { weapons } =
    div []
    [ sectionLabel "Weapons"
    , textarea
          [ rows 3
          , css [ inputStyles ]
          , onInput UpdateWeapons
          , value weapons
          ] []
    ]

editArmorView : {r | armor : Armor } -> Html Msg
editArmorView { armor } =
    let
        armorOptionView option =
            label [ css
                    [ display block
                    , marginBottom (Css.em 0.25)
                    , fontSize (Css.em 1)
                    , padding2 (Css.em 0.1) (Css.em 0.6)
                    , borderRadius (Css.em 0.25)
                    , userSelect_none
                    , cursor pointer
                    , if (armor == option) then
                          batch
                          [ backgroundColor (hex "333")
                          , color (hex "fff")
                          ]
                      else
                          batch
                          [ hover
                            [ backgroundColor (hex "eee")
                            ]
                          ]
                    ]
                  ]
            [ input
                  [ type_ "radio"
                  , name "armor-option"
                  , onInput (always (UpdateArmor option))
                  , HA.checked (armor == option)
                  , css
                        [ position absolute
                        , appearance_none
                        , opacity (int 0)
                        , Css.height (px 0)
                        , Css.width (px 0)
                        ]
                  ] []
            , text (showArmor option)
            ]
    in
        div [ css
              [ Css.width (pct 30)
              ]
            ]
            [ sectionLabel "Armor & Speed"
            , div []
                (List.map armorOptionView armorOptions)
            ]

editShieldView : {r | shield : Shield } -> Html Msg
editShieldView { shield } =
    let
        shieldOptionView option =
            label [ css
                    [ display block
                    , marginBottom (Css.em 0.25)
                    , fontSize (Css.em 1)
                    , padding2 (Css.em 0.1) (Css.em 0.6)
                    , borderRadius (Css.em 0.25)
                    , userSelect_none
                    , cursor pointer
                    , if (shield == option) then
                          batch
                          [ backgroundColor (hex "333")
                          , color (hex "fff")
                          ]
                      else
                          batch
                          [ hover
                            [ backgroundColor (hex "eee")
                            ]
                          ]
                    ]
                  ]
            [ input
                  [ type_ "radio"
                  , name "shield-option"
                  , onInput (always (UpdateShield option))
                  , HA.checked (shield == option)
                  , css
                        [ position absolute
                        , appearance_none
                        , opacity (int 0)
                        , Css.height (px 0)
                        , Css.width (px 0)
                        ]
                  ] []
            , text (showShield option)
            ]
    in
        div [ css
              [ Css.width (pct 25)
              ]
            ]
            [ sectionLabel "Shield"
            , div []
                (List.map shieldOptionView shieldOptions)
            ]

editBonusArmorView : {r | bonusArmor : Int } -> Html Msg
editBonusArmorView { bonusArmor } =
    div [ css [ Css.width (pct 30) ]
        , class "reveal-buttons-on-hover"
        ]
    [ sectionLabel "Bonus Armor"
    , integerInput
          { onClickHandler = UpdateBonusArmor
          , mMinBound = Just 0
          , mMaxBound = Nothing
          , currentValue = bonusArmor
          }
    ]


editEquipmentView : {r | equipment : String } -> Html Msg
editEquipmentView { equipment } =
    div []
    [ sectionLabel "Equipment"
    , textarea
          [ rows 5
          , css [ inputStyles ]
          , onInput UpdateEquipment
          , value equipment
          ] []
    ]

editNotesView : {r | notes : String } -> Html Msg
editNotesView { notes } =
    div []
    [ sectionLabel "Notes"
    , textarea
          [ rows 5
          , css [ inputStyles ]
          , onInput UpdateNotes
          , value notes
          ] []
    ]

editCoinView : {r | coin : Maybe Int } -> Html Msg
editCoinView { coin } =
    div [ css
          [ Css.width (pct 45)
          ]
        ]
    [ sectionLabel "Coin"
    , input
          [ type_ "text"
            , css [ inputStyles ]
            , onInput (UpdateCoin << filterToDigits)
            , value (showMaybeInt "" coin)
          ] []
    ]

editXpView : {r | xp : Maybe Int } -> Html Msg
editXpView { xp } =
    div [ css
          [ Css.width (pct 45)
          ]
        ]
    [ sectionLabel "XP"
    , input
          [ type_ "text"
            , css [ inputStyles ]
            , onInput (UpdateXp << filterToDigits)
            , value (showMaybeInt "" xp)
          ] []
    ]

--------------------------------------------------
-- Styles and Helpers
--------------------------------------------------

sectionLabel : String -> Html msg
sectionLabel title =
    div
        [ css
            [ fontSize (Css.em 1)
            , color (hex "555")
            , Css.property "font-variant" "small-caps"
            , Css.property "letter-spacing" "0.1em"
            , fontWeight bold
            , marginTop (Css.em 1)
            ]
        ]
        [ text title ]

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

integerInput : { onClickHandler : (Int -> msg)
               , mMinBound : Maybe Int
               , mMaxBound : Maybe Int
               , currentValue : Int
               }
             -> Html msg
integerInput { onClickHandler
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
                    (onClickHandler
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
                      (onClickHandler
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

--------------------------------------------------
-- Utils
--------------------------------------------------

filterToDigits : String -> Maybe Int
filterToDigits string =
    String.toInt (String.filter Char.isDigit string)

showMaybeInt : String -> Maybe Int -> String
showMaybeInt def mInt =
    case mInt of
        Just int -> String.fromInt int
        Nothing -> def

