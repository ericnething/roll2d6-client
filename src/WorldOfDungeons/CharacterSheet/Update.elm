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
