module Fate.GameAspectSheet.Types exposing (..)

import Array exposing (Array)
import Fate.CharacterSheet.Types exposing (Aspect(..))

type alias Model = GameAspectSheet

type alias GameAspectSheet =
    { scenes : Array Scene }

emptyGameAspectSheet : String -> GameAspectSheet
emptyGameAspectSheet id =
    { scenes = Array.fromList
          [
           { title = "Game Aspects"
           , aspects =
               Array.fromList
                   [ Aspect "First game aspect" 0
                   , Aspect "Second game aspect" 0
                   ]
           }
          ]
    }

type alias Scene =
    { title : String
    , aspects : Array Aspect
    }

type alias Index = Int

type Msg
    = UpdateSceneTitle Index String
    | UpdateAspect Index Index Aspect
    | AddNewAspect Index String
    | RemoveAspect Index Index
    | AddNewScene
    | RemoveScene Index
        
