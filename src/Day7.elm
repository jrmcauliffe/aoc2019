module Day7 exposing (..)

import Day5 exposing (..)
import Tuple exposing (first, second)

-- https://adventofcode.com/2019/day/7

process : List VM -> Value -> Maybe Value
process stages i  =
   case stages of
        stage :: [] -> { stage | input = (i :: stage.input)} |> run |> .output |> List.head
        stage :: rest -> { stage | input = (i :: stage.input)} |> run |> .output |> List.head |> Maybe.andThen (process rest)
        [] -> Nothing


permutations : List a -> List (List a)
permutations l =
    case l of
        x :: [] ->
            [ [ x ] ]

        [] ->
            [ [] ]

        xs ->
            List.concatMap (\x -> xs |> List.filter ((/=) x) |> permutations |> List.map ((::) x)) xs


maxPower : List Int -> Int -> Memory -> Maybe ( List Int, Int )
maxPower phaseSeq initialVal memory =
    permutations phaseSeq
        |> List.map (\ps -> ( ps, process (List.map (\p -> (VM 0 [p, initialVal] [] memory)) ps) initialVal ))
        |> List.map (\x -> ( first x, Maybe.withDefault 0 (second x) ))
        |> List.sortBy second
        |> List.reverse
        |> List.head
