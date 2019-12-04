module Day4 exposing (isValid, isValidPart2, validKeys)

import Array exposing (..)



-- https://adventofcode.com/2019/day/4


isValid : Int -> Bool
isValid =
    isValidBase (\l -> l |> groupFilter (\x -> List.length x > 1))


isValidPart2 : Int -> Bool
isValidPart2 =
    isValidBase (\l -> l |> groupFilter (\x -> List.length x == 2))


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


groupFilter : (List Int -> Bool) -> List Int -> Bool
groupFilter f =
    \l -> l |> groupMatching |> List.filter f |> List.length |> (<) 0


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


takeWhile : (a -> Bool) -> List a -> List a
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
