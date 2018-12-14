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

module WorldOfDungeons.CharacterSheet.Update exposing (..)

import WorldOfDungeons.CharacterSheet.Types exposing (..)
import Array exposing (Array)
import Util exposing (removeIndexFromArray)

update : Msg -> CharacterSheet -> (CharacterSheet, Cmd Msg)
update msg model =
    case msg of
        UpdateName name ->
            ({ model | name = name }, Cmd.none)

        UpdateClass class ->
            ({ model | class = class }, Cmd.none)

        UpdateLevel level ->
            ({ model | level = level }, Cmd.none)

        UpdateStr str ->
            ({ model | str = str }, Cmd.none)
                
        UpdateDex dex ->
            ({ model | dex = dex }, Cmd.none)

        UpdateCon con ->
            ({ model | con = con }, Cmd.none)

        UpdateInt int ->
            ({ model | int = int }, Cmd.none)

        UpdateWis wis ->
            ({ model | wis = wis }, Cmd.none)

        UpdateCha cha ->
            ({ model | cha = cha }, Cmd.none)

        UpdateSkills skills ->
            ({ model | skills = skills }, Cmd.none)

        UpdateAbility index ability ->
            ( { model
                | abilities =
                    Array.set index ability model.abilities
              }
            , Cmd.none
            )

        AddNewAbility ability ->
            ( { model
                | abilities =
                    Array.push ability model.abilities
              }
            , Cmd.none
            )

        RemoveAbility index ->
            ( { model
                | abilities =
                    removeIndexFromArray index model.abilities
              }
            , Cmd.none
            )

        UpdateWeapons weapons ->
            ({ model | weapons = weapons }, Cmd.none)

        UpdateEquipment equipment ->
            ({ model | equipment = equipment }, Cmd.none)

        UpdateArmor armor ->
            ({ model | armor = armor }, Cmd.none)

        UpdateShield shield ->
            ({ model | shield = shield }, Cmd.none)

        UpdateBonusArmor bonusArmor ->
            ({ model | bonusArmor = bonusArmor }, Cmd.none)

        UpdateHitDice hitDice ->
            ({ model | hitDice = hitDice }, Cmd.none)

        UpdateHitPoints hitPoints ->
            ({ model | hitPoints = hitPoints }, Cmd.none)

        UpdateCoin coin ->
            ({ model | coin = coin }, Cmd.none)

        UpdateXp xp ->
            ({ model | xp = xp }, Cmd.none)

        UpdateNotes notes ->
            ({ model | notes = notes }, Cmd.none)
