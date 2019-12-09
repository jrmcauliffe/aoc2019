module Day5 exposing (..)

import Array exposing (..)



-- https://adventofcode.com/2019/day/5


type alias Program =
    Array Int


type alias Address =
    Int


type alias Value =
    Int


type alias AddressDecoder =
    Address -> Program -> Address


directDecoder : AddressDecoder
directDecoder a _ =
    a


indirectDecoder : AddressDecoder
indirectDecoder a p =
    Maybe.withDefault -1 <| Array.get a p


addressModeDecoder m =
    if m == 1 then
        directDecoder

    else
        indirectDecoder


instructionDecoder : Value -> ( Value, ( AddressDecoder, AddressDecoder, AddressDecoder ) )
instructionDecoder inst =
    let
        i =
            String.fromInt inst |> String.padLeft 5 '0' |> String.toList |> List.filterMap (\x -> String.toInt (String.fromChar x))
    in
    case i of
        a :: b :: c :: d :: e :: [] ->
            ( (10 * d) + e, ( addressModeDecoder c, addressModeDecoder b, addressModeDecoder a ) )

        _ ->
            ( 99, ( directDecoder, directDecoder, directDecoder ) )



-- Parse input string into list of ints


parse : String -> Program
parse s =
    String.split "," s
        |> List.filterMap (\x -> String.toInt x)
        |> Array.fromList


getVal : Address -> AddressDecoder -> Program -> Value
getVal a d p =
    p |> Array.get (d a p) |> Maybe.withDefault -1


setVal : Address -> AddressDecoder -> Value -> Program -> Program
setVal a d v p =
    p |> Array.set (d a p) v



-- Recursive program instruction decode/execute


run : List Value -> List Value -> Address -> Program -> ( Program, List Value )
run input output pc program =
    case instructionDecoder (getVal pc directDecoder program) of
        -- Add
        ( 1, ( d1, d2, d3 ) ) ->
            setVal (pc + 3) d3 (getVal (pc + 1) d1 program + getVal (pc + 2) d2 program) program
                |> run input output (pc + 4)

        -- Mult
        ( 2, ( d1, d2, d3 ) ) ->
            setVal (pc + 3) d3 (getVal (pc + 1) d1 program * getVal (pc + 2) d2 program) program
                |> run input output (pc + 4)

        -- Input to Parameter 1
        ( 3, ( d1, _, _ ) ) ->
            Maybe.withDefault program (Maybe.map (\i -> setVal (pc + 1) d1 i program) (List.head input)) |> run input output (pc + 2)

        -- Output Parameter 1
        ( 4, ( d1, _, _ ) ) ->
            -- if non-zero output halt for debugging
            let
                o =
                    getVal (pc + 1) d1 program
            in
            if o > 0 then
                ( program, o :: output )

            else
                run input (o :: output) (pc + 2) program

        -- Jump-if-true
        ( 5, ( d1, d2, _ ) ) ->
            if getVal (pc + 1) d1 program /= 0 then
                run input output (getVal (pc + 2) d2 program) program

            else
                run input output (pc + 3) program

        -- Jump-if-false
        ( 6, ( d1, d2, _ ) ) ->
            if getVal (pc + 1) d1 program == 0 then
                run input output (getVal (pc + 2) d2 program) program

            else
                run input output (pc + 3) program

        -- Less than
        ( 7, ( d1, d2, d3 ) ) ->
            let
                val =
                    if getVal (pc + 1) d1 program < getVal (pc + 2) d2 program then
                        1

                    else
                        0
            in
            program |> setVal (pc + 3) d3 val |> run input output (pc + 4)

        -- Equal
        ( 8, ( d1, d2, d3 ) ) ->
            let
                val =
                    if getVal (pc + 1) d1 program == getVal (pc + 2) d2 program then
                        1

                    else
                        0
            in
            program |> setVal (pc + 3) d3 val |> run input output (pc + 4)

        _ ->
            ( program, output )
