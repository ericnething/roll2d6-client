module WorldOfDungeons.CharacterSheet.Decode exposing
    ( decodeCharacterSheet
    )

import Array exposing (Array)
import WorldOfDungeons.CharacterSheet.Types exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeCharacterSheet : Decoder CharacterSheet
decodeCharacterSheet =
    Decode.succeed CharacterSheet
        |> required "name" string
        |> required "class" string
        |> required "level" int
        |> required "str" int
        |> required "dex" int
        |> required "con" int
        |> required "int" int
        |> required "wis" int
        |> required "cha" int
        |> required "skills" string
        |> required "abilities" (array decodeAbility)
        |> required "weapons" string
        |> required "equipment" string
        |> required "armor" decodeArmor
        |> required "shield" decodeShield
        |> required "bonusArmor" int
        |> required "hitDice" int
        |> required "hitPoints" (maybe int)
        |> required "coin" (maybe int)
        |> required "xp" (maybe int)
        |> required "notes" string

decodeAbility : Decoder Ability
decodeAbility =
    Decode.succeed Ability
        |> required "title" string
        |> required "description" string

decodeArmor : Decoder Armor
decodeArmor =
    let
        toArmor armor =
            case armor of
                "NoArmor" -> succeed NoArmor
                "LightArmor" -> succeed LightArmor
                "FullArmor" -> succeed FullArmor
                _ -> fail "Not a valid Armor constructor"
    in
        string |> andThen toArmor

decodeShield : Decoder Shield
decodeShield =
    let
        toShield shield =
            case shield of
                "NoShield" -> succeed NoShield
                "Shield" -> succeed Shield
                _ -> fail "Not a valid Shield constructor"
    in
        string |> andThen toShield
