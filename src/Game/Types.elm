module Game.Types exposing (..)

import Array exposing (Array)
import Json.Decode exposing (Value)
import PouchDB exposing (PouchDBRef)
import RemoteData exposing (WebData)
import Http
import Time
import Game.Sheet.Types exposing (SheetMsg, SheetModel)
import Game.Sheets.Types as Sheets exposing (FullSheet)


type alias Index = Int

type Overlay
    = EditSheet Int
    | EditGameSettings
    | InstantInvite (WebData String)
    | ManagePlayers
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
    , gameType : GameType
    , id : GameId
    , title : String
    , sheets : Array (SheetModel)
    , fullSheet : Maybe FullSheet
    , overlay : Overlay
    , players : WebData (List Person)
    , chatInput : String
    , chatMessages : List ChatMessage
    , sheetsViewportX : Float
    }


emptyGameModel : PouchDBRef
               -> GameId
               -> GameData
               -> EventSourceRef
               -> Model
emptyGameModel ref id gameData eventSource =
    { ref = ref
    , eventSource = eventSource
    , gameType = gameData.gameType
    , id = id
    , title = gameData.title
    , sheets = gameData.sheets
    , fullSheet = Nothing
    , overlay = OverlayNone
    , players = RemoteData.Loading
    , chatInput = ""
    , chatMessages = []
    , sheetsViewportX = 0
    }

type GameType
    = Fate
    | WorldOfDungeons

gameTypeOptions : List GameType
gameTypeOptions =
    [ Fate
    , WorldOfDungeons
    ]

showGameType : GameType -> String
showGameType gameType =
    case gameType of
        Fate -> "Fate"
        WorldOfDungeons -> "World of Dungeons"

type alias GameData =
    { title : String
    , gameType : GameType
    , sheets : Array SheetModel
    }


type alias GameId =
    String


mergeGameData : Model -> GameData -> Model
mergeGameData model gameData =
    { model
        | title = gameData.title
        , sheets = gameData.sheets
    }


emptyGameData : GameType -> GameData
emptyGameData gameType =
    { title = "New Game"
    , gameType = gameType
    , sheets = Array.fromList []
    }


type Presence
    = Online
    | Offline

type alias PlayerPresence =
    { id : Int
    , presence : Presence
    }

type ServerEvent
    = PlayerListUpdated
        (Result Json.Decode.Error (List Person))
    | PlayerPresenceUpdated
        (Result Json.Decode.Error (List PlayerPresence))
    | ChatMessagesReceived
        (Result Json.Decode.Error (List ChatMessage))


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
    | SheetsMsg Sheets.Msg
    | UpdateGameTitle String
    | OpenOverlay Overlay
    | CloseOverlay
    | UpdateCurrentGame Value
    | ChangesReceived
    | ExitToLobby
    | CreateInvite
    | InviteCreated (WebData String)
    | PlayerList GameId (WebData (List Person))
    | RemovePlayer Int
    | PlayerRemoved GameId (Result Http.Error String)
    | ServerEventReceived ServerEvent
    | Ping
    | Pong
    | UpdateChatInput String
    | ResetChatInput
    | SendChatMessage NewChatMessage
    | KeyPressChatInput
    | DiceRollResult DiceRoll
    | ChatLogReceived (Result Http.Error (List ChatMessage))
