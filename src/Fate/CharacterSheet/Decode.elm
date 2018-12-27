-- Roll2d6 Virtual Tabletop Project
--
-- Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>
--
-- This program is free software: you can redistribute it
-- and/or modify it under the terms of the GNU Affero
-- General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR A
-- PARTICULAR PURPOSE.  See the GNU Affero General Public
-- License for more details.
--
-- You should have received a copy of the GNU Affero General
-- Public License along with this program. If not, see
-- <https://www.gnu.org/licenses/>.

module Fate.CharacterSheet.Decode exposing
    ( decodeCharacterSheet
    , decodeAspect
    )

import Array exposing (Array)
import Fate.CharacterSheet.Types exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeCharacterSheet : Decoder CharacterSheet
decodeCharacterSheet =
    Decode.succeed CharacterSheet
        |> required "name" string
        |> required "description" string
        |> required "aspects" (array decodeAspect)
        |> required "skills" (array decodeSkill)
        |> required "refresh" int
        |> required "fatePoints" int
        |> required "stunts" (array decodeStunt)
        |> required "stress" (array decodeStressTrack)
        |> required "consequences" (array decodeConsequence)
        |> required "conditions" (array decodeCondition)
        |> required "notes" string



-- decodeType : String -> Decoder (String)


decodeType expectedType =
    string
        |> andThen
            (\actualType ->
                if actualType == expectedType then
                    succeed actualType

                else
                    fail
                        ("Expected type "
                            ++ expectedType
                            ++ ", but got type "
                            ++ actualType
                            ++ "."
                        )
            )



-- decodeConstructor : String -> Decoder String


decodeConstructor expectedConstructor =
    string
        |> andThen
            (\actualConstructor ->
                if actualConstructor == expectedConstructor then
                    succeed actualConstructor

                else
                    fail
                        ("Expected constructor "
                            ++ expectedConstructor
                            ++ ", but got constructor "
                            ++ actualConstructor
                            ++ "."
                        )
            )


decodeAspect : Decoder Aspect
decodeAspect =
    Decode.succeed Aspect
        |> required "title" string
        |> required "invokes" int


decodeSkill : Decoder Skill
decodeSkill =
    Decode.succeed Skill
        |> required "rating" (int |> andThen decodeSkillRating)
        |> required "name" string


decodeStunt : Decoder Stunt
decodeStunt =
    Decode.succeed Stunt
        |> required "title" string
        |> required "description" string


decodeConsequence : Decoder Consequence
decodeConsequence =
    Decode.succeed Consequence
        |> required "severity" (int |> andThen decodeSeverity)
        |> required "title" string
        |> required "invokes" int


decodeCondition : Decoder Condition
decodeCondition =
    Decode.succeed Condition
        |> required "title" string
        |> required "stressBoxes" (array decodeStressBox)


decodeStressBox : Decoder StressBox
decodeStressBox =
    Decode.succeed StressBox
        |> required "value" int
        |> required "marked" bool


decodeStressTrack : Decoder StressTrack
decodeStressTrack =
    Decode.succeed StressTrack
        |> required "title" string
        |> required "stressBoxes" (array decodeStressBox)


decodeSeverity : Int -> Decoder Severity
decodeSeverity int_ =
    -- Case expressions on negative integers are broken in compiler
    -- 0.19
    --
    -- case int_ of
    --     (-2) ->
    --         succeed Mild

    --     (-4) ->
    --         succeed Moderate

    --     (-6) ->
    --         succeed Severe

    --     (-8) ->
    --         succeed Extreme

    --     _ ->
    --         fail "Not a valid Severity"

    if int_ == -2
    then
        succeed Mild
    else
        if int_ == -4
        then
            succeed Moderate
        else
            if int_ == -6
            then
                succeed Severe
            else
                if int_ == -8
                then
                    succeed Extreme
                else
                    fail "Not a valid Severity"


decodeSkillRating : Int -> Decoder SkillRating
decodeSkillRating int_ =
    case int_ of
        8 ->
            succeed Legendary

        7 ->
            succeed Epic

        6 ->
            succeed Fantastic

        5 ->
            succeed Superb

        4 ->
            succeed Great

        3 ->
            succeed Good

        2 ->
            succeed Fair

        1 ->
            succeed Average

        -- 0    -> succeed Mediocre

        -- (-1) ->
        --     succeed Poor

        -- (-2) ->
        --     succeed Terrible

        -- _ ->
        --     fail "Not a valid SkillRating"

        other ->
            if other == -1
            then
                succeed Poor
            else
                if other == -2
                then
                    succeed Terrible
                else
                    fail "Not a valid SkillRating"
