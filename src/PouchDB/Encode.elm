module PouchDB.Encode
    exposing
    ( encodeGame
    , encodeGameData
    , encodeChatMessage
    )

import Array exposing (Array)
import CharacterSheet.Types
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
import Maybe


--------------------------------------------------
-- Chat Messages and Dice Rolls
--------------------------------------------------

encodeChatMessage : Game.NewChatMessage -> Value
encodeChatMessage message =
    case message of
        Game.NewChatMessage body ->
            object
                [ ( "ctor", string "ChatMessage" )
                , ( "body", string body )
                ]
        Game.NewDiceRollMessage roll ->
            object
                [ ( "ctor", string "DiceRollMessage" )
                , ( "result", encodeDiceRoll roll )
                ]

encodeDiceRoll : Game.DiceRoll -> Value
encodeDiceRoll (Game.DiceRoll roll) =
    object
        [ ( "type", encodeDiceType roll.type_ )
        , ( "request", string roll.request )
        , ( "results", list encodeDiceResult roll.results )
        , ( "modifier", Maybe.withDefault null (Maybe.map int roll.modifier) )
        , ( "total", int roll.total )
        ]

encodeDiceType : Game.DiceType -> Value
encodeDiceType type_ =
    case type_ of
        Game.DFate -> string "fate"
        Game.D20 -> string "d20"
        Game.D6 -> string "d6"
        Game.DOther n -> string ("d" ++ String.fromInt n)

encodeDiceResult : Game.DiceResult -> Value
encodeDiceResult result =
    case result of
        Game.DFateResult face ->
            object
            [ ( "ctor", string "DFateResult" )
            , ( "face", encodeDFateFace face )
            ]
        Game.D20Result n ->
            object
            [ ( "ctor", string "D20Result" )
            , ( "face", int n )
            ]
        Game.D6Result n ->
            object
            [ ( "ctor", string "D6Result" )
            , ( "face", int n )
            ]
        Game.DOtherResult sides n ->
            object
            [ ( "ctor", string "DOtherResult" )
            , ( "sides", int sides )
            , ( "face", int n )
            ]

encodeDFateFace : Game.DFateFace -> Value
encodeDFateFace face =
    case face of
        Game.DFatePlus -> string "+"
        Game.DFateBlank -> string "b"
        Game.DFateMinus -> string "-"

--------------------------------------------------
-- Game Data
--------------------------------------------------

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

--------------------------------------------------
-- Character Sheets
--------------------------------------------------

encodeCharacterSheet : CharacterSheet.Types.Model -> Value
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
        [ ( "type", string "Aspect" )
        , ( "ctor", string "Aspect" )
        , ( "title", string title )
        , ( "invokes", int invokes )
        ]

encodeSkill : Skill -> Value
encodeSkill (Skill rating name) =
    object
        [ ( "type", string "Skill" )
        , ( "ctor", string "Skill" )
        , ( "rating", int (skillRatingToInt rating) )
        , ( "name", string name )
        ]

encodeStunt : Stunt -> Value
encodeStunt (Stunt title description) =
    object
        [ ( "type", string "Stunt" )
        , ( "ctor", string "Stunt" )
        , ( "title", string title )
        , ( "description", string description )
        ]

encodeConsequence : Consequence -> Value
encodeConsequence (Consequence severity title) =
    object
        [ ( "type", string "Consequence" )
        , ( "ctor", string "Consequence" )
        , ( "severity", int (severityToInt severity) )
        , ( "title", string title )
        ]

encodeCondition : Condition -> Value
encodeCondition (Condition title stressBoxes) =
    object
        [ ( "type", string "Condition" )
        , ( "ctor", string "Condition" )
        , ( "title", string title )
        , ( "stressBoxes", array encodeStressBox stressBoxes )
        ]

encodeStressBox : StressBox -> Value
encodeStressBox (StressBox value marked) =
    object
        [ ( "type", string "StressBox" )
        , ( "ctor", string "StressBox" )
        , ( "value", int value )
        , ( "marked", bool marked )
        ]

encodeStressTrack : StressTrack -> Value
encodeStressTrack (StressTrack title stressBoxes) =
    object
        [ ( "type", string "StressTrack" )
        , ( "ctor", string "StressTrack" )
        , ( "title", string title )
        , ( "stressBoxes", array encodeStressBox stressBoxes )
        ]
