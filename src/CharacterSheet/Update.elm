module CharacterSheet.Update exposing (Index, Msg(..), initialModel, update, updateCharacterSheet)

import Array exposing (Array)
import CharacterSheet.Model exposing (..)
import List.Extra exposing (stableSortWith)
import Util exposing (removeIndexFromArray)


initialModel : CharacterSheet -> Model
initialModel sheet =
    { characterSheet = sheet
    , editMode = EditModeNone
    }


type alias Index =
    Int


type Msg
    = NoOp
    | UpdateName String
    | UpdateDescription String
    | UpdateRefresh Int
    | UpdateFatePoints Int
      -- Aspects
    | UpdateAspect Int Aspect
    | AddNewAspect String
    | RemoveAspect Int
      -- Skills
    | UpdateSkill Int Skill
    | AddNewSkill Skill
    | RemoveSkill Int
      -- Stunts
    | UpdateStunt Int Stunt
    | AddNewStunt String String
    | RemoveStunt Int
      -- Stress Tracks
    | UpdateStressTrack Index StressTrack
    | AddNewStressTrack StressTrack
    | RemoveStressTrack Int
      -- Stress Boxes
    | UpdateStressBox Int Int StressBox
    | AddStressBox Index StressBox
    | RemoveStressBox Index
      -- Consequences
    | UpdateConsequence Int Consequence
    | AddNewConsequence Consequence
    | RemoveConsequence Int
      -- Conditions
    | UpdateCondition Int Condition
    | AddNewCondition Condition
    | RemoveCondition Int
    | UpdateConditionBox Int Int StressBox
    | AddConditionBox Index StressBox
    | RemoveConditionBox Index
      -- Edit Mode
    | ToggleEditMode EditMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleEditMode editMode ->
            ( { model
                | editMode =
                    if model.editMode /= editMode then
                        editMode

                    else
                        EditModeNone
              }
            , Cmd.none
            )

        msg0 ->
            let
                ( characterSheet, cmd ) =
                    updateCharacterSheet msg0 model.characterSheet
            in
            ( { model | characterSheet = characterSheet }
            , cmd
            )


updateCharacterSheet :
    Msg
    -> CharacterSheet
    -> ( CharacterSheet, Cmd Msg )
updateCharacterSheet msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateName name ->
            ( { model | name = name }
            , Cmd.none
            )

        UpdateDescription description ->
            ( { model | description = description }
            , Cmd.none
            )

        UpdateRefresh points ->
            ( { model | refresh = points }
            , Cmd.none
            )

        UpdateFatePoints points ->
            ( { model | fatePoints = points }
            , Cmd.none
            )

        UpdateAspect index aspect ->
            ( { model
                | aspects =
                    Array.set index aspect model.aspects
              }
            , Cmd.none
            )

        AddNewAspect title ->
            ( { model
                | aspects =
                    Array.push (Aspect title 0) model.aspects
              }
            , Cmd.none
            )

        RemoveAspect index ->
            ( { model
                | aspects =
                    removeIndexFromArray index model.aspects
              }
            , Cmd.none
            )

        UpdateSkill index skill ->
            ( { model
                | skills =
                    Array.set index skill model.skills
              }
            , Cmd.none
            )

        AddNewSkill skill ->
            ( { model
                | skills =
                    Array.push skill model.skills
                        |> Array.toList
                        |> List.sortBy
                            (\(Skill rating _) ->
                                skillRatingToInt rating
                            )
                        |> List.reverse
                        |> Array.fromList
              }
            , Cmd.none
            )

        RemoveSkill index ->
            ( { model
                | skills =
                    removeIndexFromArray index model.skills
              }
            , Cmd.none
            )

        UpdateStunt index stunt ->
            ( { model
                | stunts =
                    Array.set index stunt model.stunts
              }
            , Cmd.none
            )

        AddNewStunt title description ->
            ( { model
                | stunts =
                    Array.push (Stunt title description) model.stunts
              }
            , Cmd.none
            )

        RemoveStunt index ->
            ( { model
                | stunts =
                    removeIndexFromArray index model.stunts
              }
            , Cmd.none
            )

        UpdateStressBox trackIndex index stressBox ->
            let
                newModel =
                    case Array.get trackIndex model.stress of
                        Nothing ->
                            model

                        Just (StressTrack title boxes) ->
                            { model
                                | stress =
                                    Array.set
                                        trackIndex
                                        (StressTrack
                                            title
                                            (Array.set index stressBox boxes)
                                        )
                                        model.stress
                            }
            in
            ( newModel, Cmd.none )

        AddNewStressTrack stressTrack ->
            ( { model
                | stress =
                    Array.push stressTrack model.stress
              }
            , Cmd.none
            )

        RemoveStressTrack index ->
            ( { model
                | stress =
                    removeIndexFromArray index model.stress
              }
            , Cmd.none
            )

        UpdateStressTrack index stressTrack ->
            ( { model
                | stress =
                    Array.set index stressTrack model.stress
              }
            , Cmd.none
            )

        AddStressBox trackIndex stressBox ->
            let
                newModel =
                    case Array.get trackIndex model.stress of
                        Nothing ->
                            model

                        Just (StressTrack title boxes) ->
                            { model
                                | stress =
                                    Array.set
                                        trackIndex
                                        (StressTrack
                                            title
                                            (Array.push stressBox boxes)
                                        )
                                        model.stress
                            }
            in
            ( newModel, Cmd.none )

        RemoveStressBox trackIndex ->
            let
                newModel =
                    case Array.get trackIndex model.stress of
                        Nothing ->
                            model

                        Just (StressTrack title boxes) ->
                            { model
                                | stress =
                                    Array.set
                                        trackIndex
                                        (StressTrack
                                            title
                                            (Array.slice 0 -1 boxes)
                                        )
                                        model.stress
                            }
            in
            ( newModel, Cmd.none )

        UpdateConsequence index consequence ->
            ( { model
                | consequences =
                    Array.set index consequence model.consequences
              }
            , Cmd.none
            )

        AddNewConsequence newConsequence ->
            ( { model
                | consequences =
                    Array.push newConsequence model.consequences
                        |> Array.toList
                        |> stableSortWith
                            (\(Consequence a _) (Consequence b _) ->
                                Basics.compare
                                    (severityToInt b)
                                    (severityToInt a)
                            )
                        |> Array.fromList
              }
            , Cmd.none
            )

        RemoveConsequence index ->
            ( { model
                | consequences =
                    removeIndexFromArray index model.consequences
              }
            , Cmd.none
            )

        UpdateConditionBox trackIndex index stressBox ->
            let
                newModel =
                    case Array.get trackIndex model.conditions of
                        Nothing ->
                            model

                        Just (Condition title boxes) ->
                            { model
                                | conditions =
                                    Array.set
                                        trackIndex
                                        (Condition
                                            title
                                            (Array.set index stressBox boxes)
                                        )
                                        model.conditions
                            }
            in
            ( newModel, Cmd.none )

        AddConditionBox trackIndex stressBox ->
            let
                newModel =
                    case Array.get trackIndex model.conditions of
                        Nothing ->
                            model

                        Just (Condition title boxes) ->
                            { model
                                | conditions =
                                    Array.set
                                        trackIndex
                                        (Condition
                                            title
                                            (Array.push stressBox boxes)
                                        )
                                        model.conditions
                            }
            in
            ( newModel, Cmd.none )

        RemoveConditionBox trackIndex ->
            let
                newModel =
                    case Array.get trackIndex model.conditions of
                        Nothing ->
                            model

                        Just (Condition title boxes) ->
                            { model
                                | conditions =
                                    Array.set
                                        trackIndex
                                        (Condition
                                            title
                                            (Array.slice 0 -1 boxes)
                                        )
                                        model.conditions
                            }
            in
            ( newModel, Cmd.none )

        UpdateCondition trackIndex condition ->
            ( { model
                | conditions =
                    Array.set trackIndex condition model.conditions
              }
            , Cmd.none
            )

        AddNewCondition condition ->
            ( { model
                | conditions =
                    Array.push condition model.conditions
              }
            , Cmd.none
            )

        RemoveCondition index ->
            ( { model
                | conditions =
                    removeIndexFromArray index model.conditions
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
