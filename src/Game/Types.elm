module Game.Types exposing (..)

import Array exposing (Array)
import CharacterSheet
import Json.Decode exposing (Value)
import PouchDB exposing (PouchDBRef)
import RemoteData exposing (WebData)
import Http
import Time


type Overlay
    = EditCharacterSheet Int
    | EditGameSettings
    | InstantInvite (WebData String)
    | ShowPlayerList
    | OverlayNone


type AccessLevel
    = Owner
    | GameMaster
    | Player

type alias Person =
    { id : Int
    , accessLevel : AccessLevel
    , username : String
    , presence : Presence
    }

type alias EventSourceRef = Value

type alias Model =
    { ref : PouchDBRef
    , eventSource : EventSourceRef
    , id : GameId
    , title : String
    , characterSheets : Array CharacterSheet.Model
    , overlay : Overlay
    , players : WebData (List Person)
    , chatInput : String
    , chatMessages : List ChatMessage
    }


type alias GameData =
    { title : String
    , characterSheets : Array CharacterSheet.Model
    }


type alias GameId =
    String


mergeGameData : Model -> GameData -> Model
mergeGameData model gameData =
    { model
        | title = gameData.title
        , characterSheets = gameData.characterSheets
    }


initialModel : PouchDBRef
             -> EventSourceRef
             -> GameId
             -> String
             -> Model
initialModel ref eventSource id title =
    { ref = ref
    , eventSource = eventSource
    , id = id
    , title = title
    , characterSheets = Array.fromList []
    , overlay = OverlayNone
    , players = RemoteData.Loading
    , chatInput = ""
    , chatMessages = []
    }


emptyGameData : GameData
emptyGameData =
    { title = "New Game"
    , characterSheets = Array.fromList []
    }



type Presence
    = Online
    | Offline

type alias PlayerPresence =
    { id : Int
    , presence : Presence
    }

type ServerEvent
    = PlayerListUpdated (Result Json.Decode.Error (List Person))
    | PlayerPresenceUpdated (Result Json.Decode.Error (List PlayerPresence))


type NewChatMessage
    = NewChatMessage String
    | NewDiceRollMessage DiceRoll


type ChatMessage
    = ChatMessage
      { timestamp : Time.Posix
      , playerId : Int
      , playerName : String
      , body : String
      }
    | DiceRollMessage
      { timestamp : Time.Posix
      , playerId : Int
      , playerName : String
      , result : DiceRoll
      }

type DiceType
    = DFate
    | D20
    | D6
    | DOther Int

type DiceResult
    = DFateResult DFateFace
    | D20Result Int
    | D6Result Int
    | DOtherResult Int Int

type DFateFace
    = DFatePlus
    | DFateBlank
    | DFateMinus

type DiceRoll =
    DiceRoll
    { type_ : DiceType
    , request : String
    , results : List DiceResult
    , modifier : Maybe Int
    , total : Int
    }

type DiceRollRequest =
    DiceRollRequest
    { size : Int
    , type_ : DiceType
    , modifier : Maybe Int
    }

-- Update

type Msg
    = NoOp
    | CharacterSheetMsg Int CharacterSheet.Msg
    | AddCharacterSheet
    | RemoveCharacterSheet Int
    | UpdateGameTitle String
    | OpenOverlay Overlay
    | CloseOverlay
    | UpdateCurrentGame Value
    | ChangesReceived
    | ExitToLobby
    | CreateInvite
    | InviteCreated (WebData String)
    | PlayerList GameId (WebData (List Person))
    | ServerEventReceived ServerEvent
    | Ping
    | Pong
    | UpdateChatInput String
    | ResetChatInput
    | SendChatMessage NewChatMessage
    | KeyPressChatInput
    | DiceRollResult DiceRoll
    | ChatMessagesReceived (List ChatMessage)
    | ChatLogReceived (Result Http.Error (List ChatMessage))
