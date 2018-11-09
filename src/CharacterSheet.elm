module CharacterSheet exposing (Model, Msg, defaultButton, editView, initialModel, readOnlyView, update)

import CharacterSheet.Model as Model
import CharacterSheet.Update as Update
import CharacterSheet.View as View


type alias Model =
    Model.Model


type alias Msg =
    Update.Msg


update =
    Update.update


editView =
    View.editView


readOnlyView =
    View.readOnlyView


defaultButton =
    View.defaultButton


initialModel =
    Update.initialModel
