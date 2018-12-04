module WorldOfDungeons.CharacterSheet.Encode
    exposing
    (encodeCharacterSheet)

import Array exposing (Array)
import WorldOfDungeons.CharacterSheet.Types exposing (..)
import Json.Encode exposing (..)

encodeCharacterSheet : CharacterSheet -> Value
encodeCharacterSheet sheet =
    object
        [ ( "name", string sheet.name )
        , ( "class", string sheet.class )
        , ( "level", int sheet.level )
        , ( "str", int sheet.str )
        , ( "dex", int sheet.dex )
        , ( "con", int sheet.con )
        , ( "int", int sheet.int )
        , ( "wis", int sheet.wis )
        , ( "cha", int sheet.cha )
        , ( "skills", string sheet.skills)
        , ( "abilities", array encodeAbility sheet.abilities )
        , ( "weapons", string sheet.weapons)
        , ( "equipment", string sheet.equipment )
        , ( "armor", encodeArmor sheet.armor )
        , ( "shield", encodeShield sheet.shield )
        , ( "bonusArmor", int sheet.bonusArmor )
        , ( "hitDice", int sheet.hitDice )
        , ( "hitPoints", maybeInt sheet.hitPoints )
        , ( "coin", maybeInt sheet.coin )
        , ( "xp", maybeInt sheet.xp )
        , ( "notes", string sheet.notes )
        ]

encodeAbility : Ability -> Value
encodeAbility (Ability title description) =
    object
        [ ( "title", string title )
        , ( "description", string description )
        ]

encodeArmor : Armor -> Value
encodeArmor armor =
    case armor of
        NoArmor -> string "NoArmor"
        LightArmor -> string "LightArmor"
        FullArmor -> string "FullArmor"

encodeShield : Shield -> Value
encodeShield shield =
    case shield of
        NoShield -> string "NoShield"
        Shield -> string "Shield"

maybeInt : Maybe Int -> Value
maybeInt mInt =
    case mInt of
        Just n -> int n
        Nothing -> null
