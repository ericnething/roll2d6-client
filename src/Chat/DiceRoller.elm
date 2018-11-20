module Chat.DiceRoller exposing (roll)

import Game.Types
    exposing
    ( DiceRollRequest(..)
    , DiceRoll(..)
    , DiceType(..)
    , DiceResult(..)
    , DFateFace(..)
    , Msg(..)
    )
import Random exposing (Generator)
import Maybe

roll : DiceRollRequest -> Cmd Msg
roll req =
    Random.generate DiceRollResult (genDiceResult req)

genDiceResult : DiceRollRequest -> Generator DiceRoll
genDiceResult (DiceRollRequest req) =
    let
        (low, high) =
            case req.type_ of
                DFate -> (-1, 1)
                D20 -> (1, 20)
                D6 -> (1, 6)
                DOther n -> (1, n)
        
        makeDiceRoll results =
            DiceRoll
             { type_ = req.type_
             , request = showDiceRollRequest (DiceRollRequest req)
             , results =
                 List.map (toDiceResult req.type_) results
             , modifier = req.modifier
             , total =
                 List.sum results + Maybe.withDefault 0 req.modifier
                     
             }
    in
        Random.map
            makeDiceRoll
            (Random.list req.size (Random.int low high))

toDiceResult : DiceType -> Int -> DiceResult
toDiceResult type_ n =
    case type_ of
        DFate ->
            DFateResult <|
                case compare n 0 of
                    LT -> DFateMinus
                    EQ -> DFateBlank
                    GT -> DFatePlus
        D20 ->
            D20Result n
        D6 ->
            D6Result n
        DOther d ->
            DOtherResult d n


showDiceRollRequest : DiceRollRequest -> String
showDiceRollRequest (DiceRollRequest { size, type_, modifier }) =
    let
        sizePart =
            if size > 1
            then
                String.fromInt size ++ "d"
            else
                "d"
        typePart =
            case type_ of
                DFate -> "f"
                D20 -> "20"
                D6 -> "6"
                DOther n -> String.fromInt n

        modifierPart =
            case modifier of
                Nothing -> ""
                Just n ->
                    if n >= 0
                    then
                        "+" ++ String.fromInt n
                    else
                        String.fromInt n
    in
        sizePart ++ typePart ++ modifierPart
