module Day5 exposing (..)

import Array exposing (..)
-- https://adventofcode.com/2019/day/5


type alias Program =
    Array Int


type alias Address =
    Int


type alias Value =
    Int

type alias AddressDecoder = Address -> Program -> Address

directDecoder: AddressDecoder
directDecoder a _ = a

indirectDecoder: AddressDecoder 
indirectDecoder a p = getVal a p

addressModeDecoder m = if m == 1 then directDecoder else indirectDecoder

instructionDecoder: Value -> (Value, (AddressDecoder, AddressDecoder, AddressDecoder))
instructionDecoder inst =
  let i = String.fromInt inst |> String.padLeft 5 '0' |> String.toList |> List.filterMap (\x -> String.toInt (String.fromChar x)) in
  case i of
  a :: b :: c :: d :: e :: [] -> ((10 * d) + e, (addressModeDecoder c, addressModeDecoder b, addressModeDecoder a))
  _ -> (99, (directDecoder, directDecoder, directDecoder))    



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


run : Maybe Value -> Maybe Value -> Address -> Program -> (Program, Maybe Value)

run input output pc program =
    case instructionDecoder(getVal pc program) of
        (1, (d1, d2, d3)) ->
            indirectSetVal (pc + 3) (indirectGetVal (pc + 1) program + indirectGetVal (pc + 2) program) program
                |> run input output (pc + 4)

        (2, (d1, d2, d3))  ->
            indirectSetVal (pc + 3) (indirectGetVal (pc + 1) program * indirectGetVal (pc + 2) program) program
                |> run input output (pc + 4)
        (3, (d1, _, _)) -> 
            Maybe.withDefault program (Maybe.map (\i -> indirectSetVal (pc + 1) i program) input) |> run input output (pc + 2)  
        (4, (d1, _, _)) -> 
            run input (Just (indirectGetVal (pc + 1) program)) (pc + 2) program 
        _ ->
            (program, output)





