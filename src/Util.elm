module Util exposing (..)

import List.Extra exposing (takeWhile, dropWhile)

catMaybes : List (Maybe a) -> List a
catMaybes =
    let
        g ma acc =
            case ma of
                Just a  -> a :: acc
                Nothing -> acc
    in
        List.foldr g []

listDifference : List a -> List a -> List a
listDifference xs ys =
    let             
        removeFirst a bs =
            List.append
            (takeWhile (\b -> b /= a) bs)
            (List.drop 1 <| dropWhile (\b -> b /= a) bs)
    in
        List.foldl (\y acc -> removeFirst y acc) xs ys

stringToNatWithDefault : Int -> String -> Int
stringToNatWithDefault default value =
    (String.toInt value)
        |> Result.withDefault default
        |> Basics.max 0

stringToNatWithDefaultNonZero : Int -> String -> Int
stringToNatWithDefaultNonZero default value =
    stringToNatWithDefault default value
        |> Basics.max 1
