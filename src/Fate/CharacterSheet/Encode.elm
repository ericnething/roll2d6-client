module Fate.CharacterSheet.Encode exposing
    ( encodeCharacterSheet
    , encodeAspect
    )

import Array exposing (Array)
import Fate.CharacterSheet.Types exposing (..)
import Json.Encode exposing (..)


--------------------------------------------------
-- Character Sheets
--------------------------------------------------

encodeCharacterSheet : CharacterSheet -> Value
encodeCharacterSheet characterSheet =
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

encodeAspect : Aspect -> Value
encodeAspect (Aspect title invokes) =
    object
        [ ( "title", string title )
        , ( "invokes", int invokes )
        ]

encodeSkill : Skill -> Value
encodeSkill (Skill rating name) =
    object
        [ ( "rating", int (skillRatingToInt rating) )
        , ( "name", string name )
        ]

encodeStunt : Stunt -> Value
encodeStunt (Stunt title description) =
    object
        [ ( "title", string title )
        , ( "description", string description )
        ]

encodeConsequence : Consequence -> Value
encodeConsequence (Consequence severity title invokes) =
    object
        [ ( "severity", int (severityToInt severity) )
        , ( "title", string title )
        , ( "invokes", int invokes )
        ]

encodeCondition : Condition -> Value
encodeCondition (Condition title stressBoxes) =
    object
        [ ( "title", string title )
        , ( "stressBoxes", array encodeStressBox stressBoxes )
        ]

encodeStressBox : StressBox -> Value
encodeStressBox (StressBox value marked) =
    object
        [ ( "value", int value )
        , ( "marked", bool marked )
        ]

encodeStressTrack : StressTrack -> Value
encodeStressTrack (StressTrack title stressBoxes) =
    object
        [ ( "title", string title )
        , ( "stressBoxes", array encodeStressBox stressBoxes )
        ]
