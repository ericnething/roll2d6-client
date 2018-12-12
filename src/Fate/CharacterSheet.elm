module Fate.CharacterSheet
    exposing
    ( Model
    , Msg
    , editView
    , view
    , compactView
    , update
    )

import Fate.CharacterSheet.Types as Types
import Fate.CharacterSheet.View as View
import Fate.CharacterSheet.Update as Update

type alias Msg =
    Types.Msg

type alias Model =
    Types.Model

editView =
    View.editView

view =
    View.view

compactView =
    View.compactView

update =
    Update.update

