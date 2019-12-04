module Day2 exposing (findOutput, getVal, newProg, parse, run)

import Array exposing (..)



-- https://adventofcode.com/2019/day/2


type alias Program =
    Array Int


type alias Address =
    Int


type alias Value =
    Int



-- Parse input string into list of ints


parse : String -> Program
parse s =
    String.split "," s
        |> List.filterMap (\x -> String.toInt x)
        |> Array.fromList


getVal : Address -> Program -> Value
getVal i program =
    Maybe.withDefault -1 <| Array.get i program


setVal : Address -> Value -> Program -> Program
setVal i v program =
    program |> Array.set i v


indirectGetVal : Address -> Program -> Value
indirectGetVal i program =
    getVal (getVal i program) program


indirectSetVal : Address -> Value -> Program -> Program
indirectSetVal i v program =
    setVal (getVal i program) v program



-- Recursive program instruction decode/execute


run : Address -> Program -> Program
run pc program =
    case getVal pc program of
        1 ->
            indirectSetVal (pc + 3) (indirectGetVal (pc + 1) program + indirectGetVal (pc + 2) program) program
                |> run (pc + 4)

        2 ->
            indirectSetVal (pc + 3) (indirectGetVal (pc + 1) program * indirectGetVal (pc + 2) program) program
                |> run (pc + 4)

        _ ->
            program



-- Create a new program with a header and an existing program


newProg : Value -> Value -> Program -> Program
newProg n v program =
    Array.toList program
        |> List.drop 3
        |> List.append [ 1, n, v ]
        |> Array.fromList



-- Does a programs output match the value?


hasOutput : Value -> Program -> Bool
hasOutput v program =
    Just v == Array.get 0 program



-- Search space of N and V values for program that results in a value


findOutput : Int -> Int -> Value -> Program -> Maybe Program
findOutput lower upper target program =
    List.concatMap (\n -> List.map (\v -> newProg n v program) (List.range lower upper)) (List.range lower upper)
        |> List.map (\x -> run 0 x)
        |> List.filter (\p -> hasOutput target p)
        |> List.head
