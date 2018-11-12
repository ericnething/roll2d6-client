module Game.Types exposing (..)

import Array exposing (Array)
import CharacterSheet
import Json.Decode exposing (Value)
import PouchDB exposing (PouchDBRef)
import RemoteData exposing (WebData)
import Http


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
    -- , presence : Presence
    }

type alias Model =
    { ref : PouchDBRef
    , id : GameId
    , title : String
    , characterSheets : Array CharacterSheet.Model
    , overlay : Overlay
    , players : WebData (List Person)
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


initialModel : PouchDBRef -> GameId -> String -> Model
initialModel ref id title =
    { ref = ref
    , id = id
    , title = title
    , characterSheets = Array.fromList []
    , overlay = OverlayNone
    , players = RemoteData.Loading
    }


emptyGameData : GameData
emptyGameData =
    { title = "New Game"
    , characterSheets = Array.fromList []
    }



type Presence
    = Online
    | Offline

type ServerEvent
    = PlayerListUpdated (Result Json.Decode.Error (List Person))
    -- | PlayerStatusChange
    --   { id : Int
    --   , status : Presence
    --   }

-- Update

type Msg
    = CharacterSheetMsg Int CharacterSheet.Msg
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
