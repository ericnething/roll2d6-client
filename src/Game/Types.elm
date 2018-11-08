module Game.Types exposing (..)

import CharacterSheet
import Array exposing (Array)
import PouchDB exposing (PouchDBRef)
import Json.Decode exposing (Value)

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

type alias GameId = String


mergeGameData : Model -> GameData -> Model
mergeGameData model gameData =
    { model
        | title = gameData.title
        , characterSheets = gameData.characterSheets
    }

-- initialModel : Model
-- initialModel =
--     { title = "My First Game"
--     , characterSheets =
--           Array.fromList
--               [ CharacterSheet.initialModel
--                     initialCharacterSheet
--               , CharacterSheet.initialModel
--                     harryDresden_dfa
--               , CharacterSheet.initialModel
--                     sarissa_dfa
--               , CharacterSheet.initialModel
--                     tachyonSquadronShip
--               , CharacterSheet.initialModel
--                     initialCharacterSheet
--               , CharacterSheet.initialModel
--                     initialCharacterSheet
--               ]
--     , overlay = OverlayNone
--     }

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
    = ExitToLobby
    | LocalMsg Msg

type Msg
    = CharacterSheetMsg Int CharacterSheet.Msg
    | AddCharacterSheet
    | RemoveCharacterSheet Int
    | UpdateGameTitle String
    | OpenOverlay Overlay
    | CloseOverlay
    | UpdateCurrentGame Value
    | ChangesReceived      

