module Day4 exposing (isValid, isValidAdv, validKeys)

import Array exposing (..)



-- https://adventofcode.com/2019/day/4


isValid : Int -> Bool
isValid i =
    let
        l =
            String.fromInt i |> String.toList |> List.map (\c -> String.fromChar c) |> List.filterMap (\s -> String.toInt s)
    in
    lengthTest l
        && ascendingTest l
        && adjacentTest l


isValidAdv : Int -> Bool
isValidAdv =
    isValidBase adjacentTestAdv


isValidBase : (List Int -> Bool) -> Int -> Bool
isValidBase f i =
    let
        l =
            String.fromInt i |> String.toList |> List.map (\c -> String.fromChar c) |> List.filterMap (\s -> String.toInt s)
    in
    lengthTest l
        && ascendingTest l
        && f l



-- Code must be 6 digits long


lengthTest : List Int -> Bool
lengthTest l =
    List.length l == 6



-- Each digit must be the same or higher from left to right


ascendingTest : List Int -> Bool
ascendingTest l =
    case l of
        x :: y :: xs ->
            (x <= y) && ascendingTest (y :: xs)

        _ ->
            True



-- Each list must have at least one matching consecutive digits


adjacentTest : List Int -> Bool
adjacentTest l =
    l |> groupMatching |> List.filter (\x -> List.length x > 1) |> List.length |> (<) 0



-- Each list must not have three consecutive digits


adjacentTestAdv : List Int -> Bool
adjacentTestAdv l =
    l |> groupMatching |> List.filter (\x -> List.length x == 2) |> List.length |> (<) 0


groupMatching : List Int -> List (List Int)
groupMatching l =
    case l of
        x :: xs ->
            let
                group =
                    takeWhile (\y -> y == x) (x :: xs)
            in
            group :: groupMatching (List.drop (List.length group) l)

        _ ->
            []


takeWhile : (Int -> Bool) -> List Int -> List Int
takeWhile f l =
    case l of
        x :: xs ->
            if f x then
                x :: takeWhile f xs

            else
                []

        _ ->
            []



-- Find set of valid keys in range


validKeys : (Int -> Bool) -> ( Int, Int ) -> List Int
validKeys f ( lower, upper ) =
    List.range lower upper |> List.filter f
