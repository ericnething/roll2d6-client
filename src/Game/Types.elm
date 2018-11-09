module Game.Types
    exposing
    ( ConsumerMsg(..)
    , GameData
    , GameId
    , Model
    , Msg(..)
    , Overlay(..)
    , emptyGameData
    , initialModel
    , mergeGameData
    )

import Array exposing (Array)
import CharacterSheet
import Json.Decode exposing (Value)
import PouchDB exposing (PouchDBRef)


type Overlay
    = EditCharacterSheet Int
    | EditGameSettings
    | OverlayNone


type alias Model =
    { ref : PouchDBRef
    , id : GameId
    , title : String
    , characterSheets : Array CharacterSheet.Model
    , overlay : Overlay
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
    }


emptyGameData : GameData
emptyGameData =
    { title = "New Game"
    , characterSheets = Array.fromList []
    }



-- Update


type ConsumerMsg
    -- = ExitToLobby
    = LocalMsg Msg


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
