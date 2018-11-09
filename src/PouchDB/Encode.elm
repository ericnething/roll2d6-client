module PouchDB.Encode exposing (encodeGame, encodeGameData)

import Array exposing (Array)
import CharacterSheet.Model
    exposing
        ( Aspect(..)
        , Condition(..)
        , Consequence(..)
        , Skill(..)
        , StressBox(..)
        , StressTrack(..)
        , Stunt(..)
        , severityToInt
        , skillRatingToInt
        )
import Game.Types as Game
import Json.Encode exposing (..)


encodeGame : Game.Model -> Value
encodeGame game =
    object
        [ ( "_id", string "game" )
        , ( "title", string game.title )
        , ( "characterSheets"
          , array
              encodeCharacterSheet
              game.characterSheets
          )
        ]


encodeGameData : Game.GameData -> Value
encodeGameData game =
    object
        [ ( "_id", string "game" )
        , ( "title", string game.title )
        , ( "characterSheets"
          , array
              encodeCharacterSheet
              game.characterSheets
          )
        ]


-- encodeCharacterSheetArray :
--     Array CharacterSheet.Model.Model
--     -> Array Value
-- encodeCharacterSheetArray sheets =
--     Array.map encodeCharacterSheet sheets


encodeCharacterSheet : CharacterSheet.Model.Model -> Value
encodeCharacterSheet { characterSheet } =
    object
        [ ( "name", string characterSheet.name )
        , ( "description", string characterSheet.description )
        , ( "aspects", array encodeAspect characterSheet.aspects )
        , ( "skills", array encodeSkill characterSheet.skills )
        , ( "refresh", int characterSheet.refresh )
        , ( "fatePoints", int characterSheet.fatePoints )
        , ( "stunts", array encodeStunt characterSheet.stunts )
        , ( "stress", array encodeStressTrack characterSheet.stress )
        , ( "consequences", array encodeConsequence characterSheet.consequences )
        , ( "conditions", array encodeCondition characterSheet.conditions )
        ]


-- encodeAspectArray : Array Aspect -> Array Value
-- encodeAspectArray aspects =
--     Array.map encodeAspect aspects


encodeAspect : Aspect -> Value
encodeAspect (Aspect title invokes) =
    object
        [ ( "type", string "Aspect" )
        , ( "ctor", string "Aspect" )
        , ( "title", string title )
        , ( "invokes", int invokes )
        ]


-- encodeSkillArray : Array Skill -> Array Value
-- encodeSkillArray skills =
--     Array.map encodeSkill skills


encodeSkill : Skill -> Value
encodeSkill (Skill rating name) =
    object
        [ ( "type", string "Skill" )
        , ( "ctor", string "Skill" )
        , ( "rating", int (skillRatingToInt rating) )
        , ( "name", string name )
        ]


-- encodeStuntArray : Array Stunt -> Array Value
-- encodeStuntArray stunts =
--     Array.map encodeStunt stunts


encodeStunt : Stunt -> Value
encodeStunt (Stunt title description) =
    object
        [ ( "type", string "Stunt" )
        , ( "ctor", string "Stunt" )
        , ( "title", string title )
        , ( "description", string description )
        ]


-- encodeConsequenceArray : Array Consequence -> Array Value
-- encodeConsequenceArray consequences =
--     Array.map encodeConsequence consequences


encodeConsequence : Consequence -> Value
encodeConsequence (Consequence severity title) =
    object
        [ ( "type", string "Consequence" )
        , ( "ctor", string "Consequence" )
        , ( "severity", int (severityToInt severity) )
        , ( "title", string title )
        ]


-- encodeConditionArray : Array Condition -> Array Value
-- encodeConditionArray conditions =
--     Array.map encodeCondition conditions


encodeCondition : Condition -> Value
encodeCondition (Condition title stressBoxes) =
    object
        [ ( "type", string "Condition" )
        , ( "ctor", string "Condition" )
        , ( "title", string title )
        , ( "stressBoxes", array encodeStressBox stressBoxes )
        ]


-- encodeStressBoxArray : Array StressBox -> Array Value
-- encodeStressBoxArray stressBoxes =
--     Array.map encodeStressBox stressBoxes


encodeStressBox : StressBox -> Value
encodeStressBox (StressBox value marked) =
    object
        [ ( "type", string "StressBox" )
        , ( "ctor", string "StressBox" )
        , ( "value", int value )
        , ( "marked", bool marked )
        ]


-- encodeStressTrackArray : Array StressTrack -> Array Value
-- encodeStressTrackArray stressTracks =
--     Array.map encodeStressTrack stressTracks


encodeStressTrack : StressTrack -> Value
encodeStressTrack (StressTrack title stressBoxes) =
    object
        [ ( "type", string "StressTrack" )
        , ( "ctor", string "StressTrack" )
        , ( "title", string title )
        , ( "stressBoxes", array encodeStressBox stressBoxes )
        ]
