module Day7 exposing (..)

import Array exposing (..)
import Day5 exposing (..)
import Tuple exposing (first, second)



-- https://adventofcode.com/2019/day/7


process : List ( Prog, Int ) -> Value -> Maybe Value
process stages input =
    let
        getOutput i phase prg =
            run (phase :: i :: []) [] 0 prg |> second
    in
    case stages of
        ( stage, phase ) :: [] ->
            getOutput input phase stage |> List.head

        ( stage, phase ) :: rest ->
            getOutput input phase stage |> List.head |> Maybe.andThen (process rest)

        [] ->
            Nothing


permutations : List a -> List (List a)
permutations l =
    case l of
        x :: [] ->
            [ [ x ] ]

        [] ->
            [ [] ]

        xs ->
            List.concatMap (\x -> xs |> List.filter ((/=) x) |> permutations |> List.map ((::) x)) xs


maxPower : List Int -> Int -> Prog -> Maybe ( List Int, Int )
maxPower phaseSeq initialVal prog =
    permutations phaseSeq
        |> List.map (\ps -> ( ps, process (List.map (\p -> ( prog, p )) ps) initialVal ))
        |> List.map (\x -> ( first x, Maybe.withDefault 0 (second x) ))
        |> List.sortBy second
        |> List.reverse
        |> List.head
