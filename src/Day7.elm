module Day7 exposing (..)

import Array exposing (..)
import Day5 exposing (..)
import Tuple exposing (second)



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
