module Fate.CharacterSheet.Types exposing (..)

import Array exposing (Array)


type alias Model = CharacterSheet

type alias CharacterSheet =
    { name : String
    , description : String
    , aspects : Array Aspect
    , skills : Array Skill
    , refresh : Int
    , fatePoints : Int
    , stunts : Array Stunt
    , stress : Array StressTrack
    , consequences : Array Consequence
    , conditions : Array Condition
    }


-- Aspects


type Aspect
    = Aspect String Int



-- Consequences


type Severity
    = Mild
    | Moderate
    | Severe
    | Extreme


showSeverity : Severity -> String
showSeverity severity =
    case severity of
        Mild ->
            "Mild -2"

        Moderate ->
            "Moderate -4"

        Severe ->
            "Severe -6"

        Extreme ->
            "Extreme -8"


consequenceSeverityList =
    [ Mild
    , Mild
    , Moderate
    , Severe
    , Extreme
    ]


severityToInt : Severity -> Int
severityToInt severity =
    case severity of
        Mild ->
            -2

        Moderate ->
            -4

        Severe ->
            -6

        Extreme ->
            -8


type Consequence
    = Consequence Severity String



-- Skills


type SkillRating
    = Legendary
    | Epic
    | Fantastic
    | Superb
    | Great
    | Good
    | Fair
    | Average
      -- | Mediocre
    | Poor
    | Terrible


skillRatingList =
    [ Legendary
    , Epic
    , Fantastic
    , Superb
    , Great
    , Good
    , Fair
    , Average
    , Poor
    , Terrible
    ]


skillRatingToInt : SkillRating -> Int
skillRatingToInt rating =
    case rating of
        Legendary ->
            8

        Epic ->
            7

        Fantastic ->
            6

        Superb ->
            5

        Great ->
            4

        Good ->
            3

        Fair ->
            2

        Average ->
            1

        -- Mediocre  ->  0
        Poor ->
            -1

        Terrible ->
            -2


showSkillRating : SkillRating -> String
showSkillRating rating =
    case rating of
        Legendary ->
            "Legendary +8"

        Epic ->
            "Epic +7"

        Fantastic ->
            "Fantastic +6"

        Superb ->
            "Superb +5"

        Great ->
            "Great +4"

        Good ->
            "Good +3"

        Fair ->
            "Fair +2"

        Average ->
            "Average +1"

        -- Mediocre  -> "Mediocre (+0)"
        Poor ->
            "Poor -1"

        Terrible ->
            "Terrible -2"


type Skill
    = Skill SkillRating String



-- Stunts


type Stunt
    = Stunt String String



-- Stress


type StressBox
    = StressBox Int Bool


type StressTrack
    = StressTrack String (Array StressBox)



-- Conditions


type Condition
    = Condition String (Array StressBox)



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
