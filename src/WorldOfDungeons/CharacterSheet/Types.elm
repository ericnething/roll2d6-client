module WorldOfDungeons.CharacterSheet.Types exposing (..)

import Array exposing (Array)

type alias Model = CharacterSheet

type alias CharacterSheet =
    { name : String
    , class : String
    , level : Int
    , str : Int
    , dex : Int
    , con : Int
    , int : Int
    , wis : Int
    , cha : Int
    , skills : String
    , abilities : Array Ability
    , weapons : String
    , equipment : String
    , armor : Armor
    , shield : Shield
    , bonusArmor : Int
    , hitDice : Int
    , hitPoints : Maybe Int
    , coin : Maybe Int
    , xp : Maybe Int
    , notes : String
    }

type alias Attributes r =
    { r |
      str : Int
    , dex : Int
    , con : Int
    , int : Int
    , wis : Int
    , cha : Int
    }

blankCharacterSheet =
    { name = "Einhildur"
    , class = "Ranger"
    , level = 1
    , str = 0
    , dex = 2
    , con = 0
    , int = 1
    , wis = 2
    , cha = 0
    , skills = "Survival, Athletics"
    , abilities =
        Array.fromList
            [ Ability
                  "Pet"
                  "You have a loyal and effective animal companion."
            , Ability
                  "Scout"
                  "When you scout ahead, you always spot the target before it spots you."
            ]
    , weapons = "Bow (d6+1), Handaxe (d6)"
    , equipment = "20ft rope, travel rations, waterskin, grappling hook"
    , armor = NoArmor
    , shield = NoShield
    , bonusArmor = 0
    , hitDice = 1
    , hitPoints = Just 3
    , coin = Just 9
    , xp = Just 0
    , notes = "Here are some notes."
    }

type Class
    = Fighter
    | Thief
    | Cleric
    | Wizard
    | Ranger

showClass : Class -> String
showClass class =
    case class of
        Fighter -> "Fighter"
        Thief   -> "Thief"
        Cleric  -> "Cleric"
        Wizard  -> "Wizard"
        Ranger  -> "Ranger"

type Skill
    = Athletics
    | Awareness
    | Deception
    | Decipher
    | Heal
    | Leadership
    | Lore
    | Stealth
    | Survival

showSkill : Skill -> String
showSkill skill =
    case skill of
        Athletics  -> "Athletics"
        Awareness  -> "Awareness"
        Deception  -> "Deception"
        Decipher   -> "Decipher"
        Heal       -> "Heal"
        Leadership -> "Leadership"
        Lore       -> "Lore"
        Stealth    -> "Stealth"
        Survival   -> "Survival"

type Ability = Ability String String

-- type Ability
--     = Bless
--     | Cure
--     | Turn
--     | Vision
--     | Hardy
--     | Skirmish
--     | Slay
--     | Tough
--     | Backstab
--     | Reflexes
--     | Lucky
--     | Tinker
--     | Cantrips
--     | Command
--     | Ritual
--     | Summon
--     | Pet
--     | Scout
--     | Volley
--     | Wild

-- showAbility : Ability -> String
-- showAbility ability =
--     case ability of
--         Bless -> "Bless"
--         Cure -> "Cure"
--         Turn -> "Turn"
--         Vision -> "Vision"
--         Hardy -> "Hardy"
--         Skirmish -> "Skirmish"
--         Slay -> "Slay"
--         Tough -> "Tough"
--         Backstab -> "Backstab"
--         Reflexes -> "Reflexes"
--         Lucky -> "Lucky"
--         Tinker -> "Tinker"
--         Cantrips -> "Cantrips"
--         Command -> "Command"
--         Ritual -> "Ritual"
--         Summon -> "Summon"
--         Pet -> "Pet"
--         Scout -> "Scout"
--         Volley -> "Volley"
--         Wild -> "Wild"

type Armor
    = NoArmor
    | LightArmor
    | FullArmor

armorOptions =
    [ NoArmor
    , LightArmor
    , FullArmor
    ]

showArmor : Armor -> String
showArmor armor =
    case armor of
        NoArmor -> "None (Fast)"
        LightArmor -> "Light (Normal)"
        FullArmor -> "Full (Slow)"

armorToInt : Armor -> Int
armorToInt armor =
    case armor of
        NoArmor -> 0
        LightArmor -> 1
        FullArmor -> 2
    

type Shield
    = NoShield
    | Shield

shieldOptions =
    [ NoShield
    , Shield
    ]

showShield : Shield -> String
showShield shield =
    case shield of
        NoShield -> "None"
        Shield -> "+1"

shieldToInt : Shield -> Int
shieldToInt shield =
    case shield of
        NoShield -> 0
        Shield -> 1

type alias Index = Int

type Msg
    = UpdateName String
    | UpdateClass String
    | UpdateLevel Int
    | UpdateStr Int
    | UpdateDex Int
    | UpdateCon Int
    | UpdateInt Int
    | UpdateWis Int
    | UpdateCha Int
    | UpdateSkills String
    | UpdateAbility Index Ability
    | AddNewAbility Ability
    | RemoveAbility Index
    | UpdateWeapons String
    | UpdateEquipment String
    | UpdateArmor Armor
    | UpdateShield Shield
    | UpdateBonusArmor Int
    | UpdateHitDice Int
    | UpdateHitPoints (Maybe Int)
    | UpdateCoin (Maybe Int)
    | UpdateXp (Maybe Int)
    | UpdateNotes String

