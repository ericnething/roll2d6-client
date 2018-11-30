module CharacterSheet
    exposing
    ( Model
    , Msg
    , defaultButton
    , editView
    , view
    , update
    )

import CharacterSheet.Types as CharacterSheet
import CharacterSheet.Update as CharacterSheet
import CharacterSheet.View as CharacterSheet


-- type alias Msg
--     = CharacterSheetMsg CharacterSheet.Msg
--     | GameAspectSheetMsg GameAspectSheet.Msg

-- type Model
--     = CharacterSheet CharacterSheet.Model
--     | GameAspectSheet GameAspectSheet.Model

-- update : Msg -> Model -> (Model, Cmd Msg)
-- update msg model =
--     case (msg, model) of
--         (CharacterSheetMsg submsg, CharacterSheet submodel) ->
--             let
--                 (newModel, cmd) =
--                     CharacterSheet.update submsg submodel
--             in
--                 (newModel, Cmd.map CharacterSheetMsg cmd)

--         (GameAspectSheetMsg submsg, GameAspectSheet submodel) ->
--             let
--                 (newModel, cmd) =
--                     GameAspectSheet.update submsg submodel
--             in
--                 (newModel, Cmd.map GameAspectSheetMsg cmd)

--         _ ->
--             (model, cmd)


type alias Msg =
    CharacterSheet.Msg

type alias Model =
    CharacterSheet.Model

update =
    CharacterSheet.update

editView =
    CharacterSheet.editView


view =
    CharacterSheet.view


defaultButton =
    CharacterSheet.defaultButton

