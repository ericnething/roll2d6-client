module Fate.GameAspectSheet
    exposing
    ( Model
    , Msg
    , view
    , editView
    , update
    , blank
    )

import Fate.GameAspectSheet.Types as Types
import Fate.GameAspectSheet.View as View
import Fate.GameAspectSheet.Update as Update

type alias Msg
    = Types.Msg

type alias Model
    = Types.Model

view =
    View.view

editView =
    View.editView

update =
    Update.update

blank =
    Types.emptyGameAspectSheet
