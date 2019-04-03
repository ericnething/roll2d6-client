{-
Roll2d6 Virtual Tabletop Project

Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program. If not, see
<https://www.gnu.org/licenses/>.
-}

module Util exposing (..)

import Array exposing (Array)
import List.Extra exposing (dropWhile, takeWhile)


catMaybes : List (Maybe a) -> List a
catMaybes =
    let
        g ma acc =
            case ma of
                Just a ->
                    a :: acc

                Nothing ->
                    acc
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
    String.toInt value
        |> Maybe.withDefault default
        |> Basics.max 0


stringToNatWithDefaultNonZero : Int -> String -> Int
stringToNatWithDefaultNonZero default value =
    stringToNatWithDefault default value
        |> Basics.max 1


removeIndexFromArray : Int -> Array a -> Array a
removeIndexFromArray index array =
    array
        |> Array.toIndexedList
        |> List.filter (\( idx, _ ) -> idx /= index)
        |> List.map Tuple.second
        |> Array.fromList


findArrayIndexOf : a -> Array a -> Maybe Int
findArrayIndexOf x xs =
    Tuple.second <|
    Array.foldl
    (\a (index, acc) ->
         if a == x
         then
             (index, Just index)
         else
             (index + 1, acc)
    )
    (0, Nothing)
    xs

swapArray : Int -> Int -> Array a -> Array a
swapArray indexA indexB array =
    let
        ma = Array.get indexA array
        mb = Array.get indexB array
    in
        case (ma, mb) of
            (Just a, Just b) ->
                array
                    |> Array.set indexA b
                    |> Array.set indexB a
            _ ->
                array

arrayAll : (a -> Bool) -> Array a -> Bool
arrayAll pred array =
    if Array.isEmpty array
    then
        False
    else
        Array.foldl
        (\a acc -> pred a && acc)
        True
        array

arrayAny : (a -> Bool) -> Array a -> Bool
arrayAny pred array =
    Array.foldl
    (\a acc -> pred a || acc)
    False
    array
