module Day7 exposing (..)

import Array exposing (..)
import Tuple exposing (second)
import Day5 exposing (..)
-- https://adventofcode.com/2019/day/7

process: List (Int, Prog) -> Value -> Maybe Value
process stages input = 
  let getOutput i phase prg = run (i :: phase :: []) [] 0 prg |> second
  in 
  case stages of
    (phase, stage) :: [] -> getOutput input phase stage |> List.head
    (phase, stage) :: rest -> getOutput input phase stage |> List.head |> Maybe.andThen (process stages)
    [] -> Just input
 

