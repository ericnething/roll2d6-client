module PouchDB.Decode exposing (decodeGame, decodeGameList)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Array exposing (Array)
import Lobby
import Game
import CharacterSheet.Model exposing
    ( Aspect(..)
    , Skill(..)
    , SkillRating(..)
    , skillRatingToInt
    , Stunt(..)
    , StressTrack(..)
    , StressBox(..)
    , Consequence(..)
    , Severity(..)
    , severityToInt
    , Condition(..)
    , CharacterSheet
    )

-- Game Metadata List

decodeGameList : Value -> Result String (List Lobby.GameMetadata)
decodeGameList value =
    decodeValue gameListDecoder value

gameListDecoder : Decoder (List Lobby.GameMetadata)
gameListDecoder =
    list gameMetadataDecoder

gameMetadataDecoder : Decoder Lobby.GameMetadata
gameMetadataDecoder =
    map2 Lobby.GameMetadata
        (field "_id" string)
        (field "title" string)

-- Game

decodeGame : Value -> Result String Game.Model
decodeGame value =
    decodeValue gameDecoder value

gameDecoder : Decoder Game.Model
gameDecoder =
    map3 (\a b c -> Game.Model a b c Game.OverlayNone)
    (field "_id" string)
    (field "title" string)
    (field "characterSheets" (array decodeCharacterSheetWrapper))

decodeCharacterSheet : Decoder CharacterSheet
decodeCharacterSheet =
    decode CharacterSheet
        |> required "name" string
        |> required "description" string
        |> required "aspects" (array decodeAspect)
        |> required "skills" (array decodeSkill)
        |> required "refresh" int
        |> required "fatePoints" int
        |> required "stunts" (array decodeStunt)
        |> required "stress" (array decodeStressTrack)
        |> required "consequences" (array decodeConsequence)
        |> required "conditions" (array decodeCondition)


decodeCharacterSheetWrapper : Decoder CharacterSheet.Model.Model
decodeCharacterSheetWrapper =
    map
    CharacterSheet.Model.defaultModel
    decodeCharacterSheet

-- decodeType : String -> Decoder (String)
decodeType expectedType =
    string
        |> andThen
           (\actualType ->
                if
                    actualType == expectedType
                then
                    succeed actualType
                else
                    fail ("Expected type "
                              ++ expectedType
                              ++ ", but got type "
                              ++ actualType ++ ".")
           )        

-- decodeConstructor : String -> Decoder String
decodeConstructor expectedConstructor =
    string
        |> andThen
           (\actualConstructor ->
                if
                    actualConstructor == expectedConstructor
                then
                    succeed actualConstructor
                else
                    fail ("Expected constructor "
                              ++ expectedConstructor
                              ++ ", but got constructor "
                              ++ actualConstructor ++ ".")
           )

decodeAspect : Decoder Aspect
decodeAspect =
    decode (\_ _ a b -> Aspect a b)
        |> required "type" (decodeType "Aspect")
        |> required "ctor" (decodeConstructor "Aspect")
        |> required "title" string
        |> required "invokes" int


decodeSkill : Decoder Skill
decodeSkill =
    decode (\_ _ a b -> Skill a b)
        |> required "type" (decodeType "Skill")
        |> required "ctor" (decodeConstructor "Skill")
        |> required "rating" (int |> andThen decodeSkillRating)
        |> required "name" string


decodeStunt : Decoder Stunt
decodeStunt =
    decode (\_ _ a b -> Stunt a b)
        |> required "type" (decodeType "Stunt")
        |> required "ctor" (decodeConstructor "Stunt")
        |> required "title" string
        |> required "description" string


decodeConsequence : Decoder Consequence
decodeConsequence =
    decode (\_ _ a b -> Consequence a b)
        |> required "type" (decodeType "Consequence")
        |> required "ctor" (decodeConstructor "Consequence")
        |> required "severity" (int |> andThen decodeSeverity)
        |> required "title" string


decodeCondition : Decoder Condition
decodeCondition =
    decode (\_ _ a b -> Condition a b)
        |> required "type" (decodeType "Condition")
        |> required "ctor" (decodeConstructor "Condition")
        |> required "title" string
        |> required "stressBoxes" (array decodeStressBox)


decodeStressBox : Decoder StressBox
decodeStressBox =
    decode (\_ _ a b -> StressBox a b)
        |> required "type" (decodeType "StressBox")
        |> required "ctor" (decodeConstructor "StressBox")
        |> required "value" int
        |> required "marked" bool


decodeStressTrack : Decoder StressTrack
decodeStressTrack =
    decode (\_ _ a b -> StressTrack a b)
        |> required "type" (decodeType "StressTrack")
        |> required "ctor" (decodeConstructor "StressTrack")
        |> required "title" string
        |> required "stressBoxes" (array decodeStressBox)


decodeSeverity : Int -> Decoder Severity
decodeSeverity int_ =
    case int_ of
        (-2) -> succeed Mild
        (-4) -> succeed Moderate
        (-6) -> succeed Severe
        (-8) -> succeed Extreme
        _    -> fail "Not a valid Severity"


decodeSkillRating : Int -> Decoder SkillRating
decodeSkillRating int_ =
    case int_ of
        8    -> succeed Legendary
        7    -> succeed Epic
        6    -> succeed Fantastic
        5    -> succeed Superb
        4    -> succeed Great
        3    -> succeed Good
        2    -> succeed Fair
        1    -> succeed Average
        -- 0    -> succeed Mediocre
        (-1) -> succeed Poor
        (-2) -> succeed Terrible
        _    -> fail "Not a valid SkillRating"
