module Game.Decode exposing
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
    , scrollDecoder
    )

import Array exposing (Array)
import Game.Types as Game
import Game.GameType as Game
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Lobby.Types as Lobby
import RemoteData
import Time

import Game.Sheet.Types as Sheet

import Fate
import Fate.CharacterSheet.Decode as Fate
import Fate.GameAspectSheet.Decode as Fate

import WorldOfDungeons
import WorldOfDungeons.CharacterSheet.Decode as WorldOfDungeons


--------------------------------------------------
-- Scroll events
--------------------------------------------------

scrollDecoder : (Int -> msg) -> Decoder msg
scrollDecoder toMsg =
    map toMsg (at ["target", "scrollLeft"] int)

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


decodeGame : Value -> Result Error (Game.Model)
decodeGame value =
    decodeValue gameDecoder value


gameDecoder : Decoder (Game.Model)
gameDecoder =
    map4 Game.emptyGameModel
        (field "ref" value)
        (field "id" string)
        (field "game" gameDataDecoder)
        (field "eventSource" value)


decodeGameData : Value -> Result Error (Game.GameData)
decodeGameData value =
    decodeValue gameDataDecoder value


gameDataDecoder : Decoder (Game.GameData)
gameDataDecoder =
    field "gameType" gameTypeDecoder
        |> andThen
           (\gameType ->
                map3 Game.GameData
                (field "title" string)
                (succeed gameType)
                (field "sheets" (array (sheetDecoder gameType)))
           )


sheetDecoder : Game.GameType -> Decoder (Sheet.SheetModel)
sheetDecoder gameType =
    case gameType of
        Game.Fate ->
            fateSheetDecoder

        Game.WorldOfDungeons ->
            worldOfDungeonsSheetDecoder

fateSheetDecoder : Decoder (Sheet.SheetModel)
fateSheetDecoder =
    oneOf
        [ Fate.decodeCharacterSheet
            |> Decode.map
               (Sheet.FateSheet << Fate.CharacterSheet)
        , Fate.decodeGameAspectSheet
            |> Decode.map
               (Sheet.FateSheet << Fate.GameAspectSheet)
        ]

worldOfDungeonsSheetDecoder : Decoder (Sheet.SheetModel)
worldOfDungeonsSheetDecoder =
    WorldOfDungeons.decodeCharacterSheet
        |> Decode.map
           (Sheet.WorldOfDungeonsSheet <<
                WorldOfDungeons.CharacterSheet)

gameTypeDecoder : Decoder Game.GameType
gameTypeDecoder =
    let
        toGameType gameType =
            case gameType of
                "fate" ->
                    succeed Game.Fate
                        
                "world-of-dungeons" ->
                    succeed Game.WorldOfDungeons
            
                _ ->
                    fail "Not a valid GameType"
    in
        string |> andThen toGameType
