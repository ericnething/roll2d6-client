module PouchDB.Decode exposing
    ( decodeGame
    , decodeGameData
    , decodeGameId
    , decodeGameList
    , gameIdDecoder
    , gameListDecoder
    , playerListDecoder
    , decodePlayerList
    , decodePlayerPresence
    , chatMessageListDecoder
    , decodeChatMessageList
    )

import Array exposing (Array)
import CharacterSheet.Types
    exposing
        ( Aspect(..)
        , CharacterSheet
        , Condition(..)
        , Consequence(..)
        , Severity(..)
        , Skill(..)
        , SkillRating(..)
        , StressBox(..)
        , StressTrack(..)
        , Stunt(..)
        , severityToInt
        , skillRatingToInt
        )
import Game.Types as Game
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Lobby.Types as Lobby
import RemoteData
import Time


--------------------------------------------------
-- Chat messages and dice rolls
--------------------------------------------------

decodeChatMessageList : Value -> Result Error (List Game.ChatMessage)
decodeChatMessageList value =
    decodeValue chatMessageListDecoder value

chatMessageListDecoder : Decoder (List Game.ChatMessage)
chatMessageListDecoder =
    list chatMessageDecoder

chatMessageDecoder : Decoder Game.ChatMessage
chatMessageDecoder =
    field "ctor" string
        |> andThen
           (\ctor ->
                case ctor of
                    "ChatMessage" ->
                        map4 (\a b c d ->
                                  Game.ChatMessage
                                  { timestamp = a
                                  , playerId = b
                                  , playerName = c
                                  , body = d
                                  }
                             )
                            (field "timestamp"
                               (map (Time.millisToPosix << round)
                                    float))
                            (field "playerId" int)
                            (field "playerName" string)
                            (field "body" string)
                    "DiceRollMessage" ->
                        map4 (\a b c d ->
                                  Game.DiceRollMessage
                                  { timestamp = a
                                  , playerId = b
                                  , playerName = c
                                  , result = d
                                  }
                             )
                            (field "timestamp"
                               (map (Time.millisToPosix << round)
                                    float))
                            (field "playerId" int)
                            (field "playerName" string)
                            (field "result" diceRollDecoder)
                    _ ->
                        fail ("Not a valid ChatMessage constructor: " ++ ctor)
           )

diceRollDecoder : Decoder Game.DiceRoll
diceRollDecoder =
    succeed (\a b c d e ->
                 Game.DiceRoll
                 { type_ = a
                 , request = b
                 , results = c
                 , modifier = d
                 , total = e
                 }
            )
        |> required "type" (string |> andThen diceTypeDecoder)
        |> required "request" string
        |> required "results" (list diceResultDecoder)
        |> optional "modifier" (map Just int) Nothing
        |> required "total" int

diceTypeDecoder : String -> Decoder Game.DiceType
diceTypeDecoder type_ =
    case type_ of
        "fate" -> succeed Game.DFate
        "d20" -> succeed Game.D20
        "d6" -> succeed Game.D6
        _ ->
            if String.startsWith "d" type_
            then
                case String.toInt (String.dropLeft 1 type_) of
                    Nothing ->
                        fail ("Not a valid dice type: " ++ type_)
                    Just sides ->
                        succeed (Game.DOther sides)
            else
                fail ("Not a valid dice type: " ++ type_)

diceResultDecoder : Decoder Game.DiceResult
diceResultDecoder =
    field "ctor" string
        |> andThen
           (\ctor ->
                case ctor of
                    "DFateResult" ->
                        succeed Game.DFateResult
                            |> required "face"
                               (string |> andThen dFateFaceDecoder)
                    "D20Result" ->
                        succeed Game.D20Result
                            |> required "face" int
                    "D6Result" ->
                        succeed Game.D6Result
                            |> required "face" int
                    "DOtherResult" ->
                        succeed Game.DOtherResult
                            |> required "sides" int
                            |> required "face" int
                    _ ->
                        fail ("Not a valid DiceResult constructor: " ++ ctor)
           )


dFateFaceDecoder : String -> Decoder Game.DFateFace
dFateFaceDecoder face =
    case face of
        "+" -> succeed Game.DFatePlus
        "b" -> succeed Game.DFateBlank
        "-" -> succeed Game.DFateMinus
        _ ->
            fail ("Not a valid DFateFace value: " ++ face)

--------------------------------------------------
-- Server Sent Events
--------------------------------------------------

decodePlayerPresence : Value
                     -> Result Error (List Game.PlayerPresence)
decodePlayerPresence value =
    decodeValue (list playerPresenceDecoder) value

playerPresenceDecoder : Decoder Game.PlayerPresence
playerPresenceDecoder =
    map2 Game.PlayerPresence
        (field "id" int)
        (field "presence" (string |> andThen presenceDecoder))

--------------------------------------------------
-- Player List
--------------------------------------------------

decodePlayerList : Value -> Result Error (List Game.Person)
decodePlayerList value =
    decodeValue playerListDecoder value

playerListDecoder : Decoder (List Game.Person)
playerListDecoder =
    list playerDecoder

playerDecoder : Decoder Game.Person
playerDecoder =
    map4 Game.Person
        (field "id" int)
        (field "access" (string |> andThen accessLevelDecoder))
        (field "username" string)
        (field "presence" (string |> andThen presenceDecoder))

accessLevelDecoder : String -> Decoder Game.AccessLevel
accessLevelDecoder access =
    case String.toLower access of
        "owner" ->
            succeed Game.Owner
        "game master" ->
            succeed Game.GameMaster
        "player" ->
            succeed Game.Player
        _ ->
            fail "Not a valid Access Level"

presenceDecoder : String -> Decoder Game.Presence
presenceDecoder presence =
    case String.toLower presence of
        "online" ->
            succeed Game.Online
        "offline" ->
            succeed Game.Offline
        _ ->
            fail "Not a valid Presence"

--------------------------------------------------
-- Game Metadata List
--------------------------------------------------

decodeGameList : Value -> Result Error (List Lobby.GameMetadata)
decodeGameList value =
    decodeValue gameListDecoder value


gameListDecoder : Decoder (List Lobby.GameMetadata)
gameListDecoder =
    list gameMetadataDecoder


gameMetadataDecoder : Decoder Lobby.GameMetadata
gameMetadataDecoder =
    map2 Lobby.GameMetadata
        (field "id" string)
        (field "title" string)


--------------------------------------------------
-- Game
--------------------------------------------------

decodeGameId : Value -> Result Error Game.GameId
decodeGameId value =
    decodeValue gameIdDecoder value


gameIdDecoder : Decoder Game.GameId
gameIdDecoder =
    string


decodeGame : Value -> Result Error Game.Model
decodeGame value =
    decodeValue gameDecoder value


gameDecoder : Decoder Game.Model
gameDecoder =
    map4
        (\ref id gameData eventSource ->
            { ref = ref
            , eventSource = eventSource
            , id = id
            , title = gameData.title
            , characterSheets = gameData.characterSheets
            , overlay = Game.OverlayNone
            , players = RemoteData.Loading
            , chatInput = ""
            , chatMessages = []
            }
        )
        (field "ref" value)
        (field "id" string)
        (field "game" gameDataDecoder)
        (field "eventSource" value)


decodeGameData : Value -> Result Error Game.GameData
decodeGameData value =
    decodeValue gameDataDecoder value


gameDataDecoder : Decoder Game.GameData
gameDataDecoder =
    map2 Game.GameData
        (field "title" string)
        (field "characterSheets" (array decodeCharacterSheet))


decodeCharacterSheet : Decoder CharacterSheet
decodeCharacterSheet =
    Decode.succeed CharacterSheet
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



-- decodeType : String -> Decoder (String)


decodeType expectedType =
    string
        |> andThen
            (\actualType ->
                if actualType == expectedType then
                    succeed actualType

                else
                    fail
                        ("Expected type "
                            ++ expectedType
                            ++ ", but got type "
                            ++ actualType
                            ++ "."
                        )
            )



-- decodeConstructor : String -> Decoder String


decodeConstructor expectedConstructor =
    string
        |> andThen
            (\actualConstructor ->
                if actualConstructor == expectedConstructor then
                    succeed actualConstructor

                else
                    fail
                        ("Expected constructor "
                            ++ expectedConstructor
                            ++ ", but got constructor "
                            ++ actualConstructor
                            ++ "."
                        )
            )


decodeAspect : Decoder Aspect
decodeAspect =
    Decode.succeed (\_ _ a b -> Aspect a b)
        |> required "type" (decodeType "Aspect")
        |> required "ctor" (decodeConstructor "Aspect")
        |> required "title" string
        |> required "invokes" int


decodeSkill : Decoder Skill
decodeSkill =
    Decode.succeed (\_ _ a b -> Skill a b)
        |> required "type" (decodeType "Skill")
        |> required "ctor" (decodeConstructor "Skill")
        |> required "rating" (int |> andThen decodeSkillRating)
        |> required "name" string


decodeStunt : Decoder Stunt
decodeStunt =
    Decode.succeed (\_ _ a b -> Stunt a b)
        |> required "type" (decodeType "Stunt")
        |> required "ctor" (decodeConstructor "Stunt")
        |> required "title" string
        |> required "description" string


decodeConsequence : Decoder Consequence
decodeConsequence =
    Decode.succeed (\_ _ a b -> Consequence a b)
        |> required "type" (decodeType "Consequence")
        |> required "ctor" (decodeConstructor "Consequence")
        |> required "severity" (int |> andThen decodeSeverity)
        |> required "title" string


decodeCondition : Decoder Condition
decodeCondition =
    Decode.succeed (\_ _ a b -> Condition a b)
        |> required "type" (decodeType "Condition")
        |> required "ctor" (decodeConstructor "Condition")
        |> required "title" string
        |> required "stressBoxes" (array decodeStressBox)


decodeStressBox : Decoder StressBox
decodeStressBox =
    Decode.succeed (\_ _ a b -> StressBox a b)
        |> required "type" (decodeType "StressBox")
        |> required "ctor" (decodeConstructor "StressBox")
        |> required "value" int
        |> required "marked" bool


decodeStressTrack : Decoder StressTrack
decodeStressTrack =
    Decode.succeed (\_ _ a b -> StressTrack a b)
        |> required "type" (decodeType "StressTrack")
        |> required "ctor" (decodeConstructor "StressTrack")
        |> required "title" string
        |> required "stressBoxes" (array decodeStressBox)


decodeSeverity : Int -> Decoder Severity
decodeSeverity int_ =
    -- Case expressions on negative integers are broken in compiler
    -- 0.19
    --
    -- case int_ of
    --     (-2) ->
    --         succeed Mild

    --     (-4) ->
    --         succeed Moderate

    --     (-6) ->
    --         succeed Severe

    --     (-8) ->
    --         succeed Extreme

    --     _ ->
    --         fail "Not a valid Severity"

    if int_ == -2
    then
        succeed Mild
    else
        if int_ == -4
        then
            succeed Moderate
        else
            if int_ == -6
            then
                succeed Severe
            else
                if int_ == -8
                then
                    succeed Extreme
                else
                    fail "Not a valid Severity"


decodeSkillRating : Int -> Decoder SkillRating
decodeSkillRating int_ =
    case int_ of
        8 ->
            succeed Legendary

        7 ->
            succeed Epic

        6 ->
            succeed Fantastic

        5 ->
            succeed Superb

        4 ->
            succeed Great

        3 ->
            succeed Good

        2 ->
            succeed Fair

        1 ->
            succeed Average

        -- 0    -> succeed Mediocre

        -- (-1) ->
        --     succeed Poor

        -- (-2) ->
        --     succeed Terrible

        -- _ ->
        --     fail "Not a valid SkillRating"

        other ->
            if other == -1
            then
                succeed Poor
            else
                if other == -2
                then
                    succeed Terrible
                else
                    fail "Not a valid SkillRating"
