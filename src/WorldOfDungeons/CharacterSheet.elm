module WorldOfDungeons.CharacterSheet
    exposing
    ( Model
    , Msg
    , editView
    , view
    , update
    , blank
    )

import WorldOfDungeons.CharacterSheet.Types as Types
import WorldOfDungeons.CharacterSheet.View as View
import WorldOfDungeons.CharacterSheet.Update as Update

type alias Msg =
    Types.Msg

type alias Model =
    Types.Model

editView =
    View.editView

view =
    View.view

update =
    Update.update

blank =
    Types.blankCharacterSheet
