module Day5 exposing (..)

import Array exposing (..)



-- https://adventofcode.com/2019/day/5


type alias Memory =
    Array Int


type alias VM =
    { pc : Int
    , input : List Value
    , output : List Value
    , memory : Memory
    }


type alias Address =
    Int


type alias Value =
    Int


type alias AddressDecoder =
    Address -> Memory -> Address


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


parse : String -> Memory
parse s =
    String.split "," s
        |> List.filterMap (\x -> String.toInt x)
        |> Array.fromList


getVal : Address -> AddressDecoder -> Memory -> Value
getVal a d p =
    p |> Array.get (d a p) |> Maybe.withDefault -1


setVal : Address -> AddressDecoder -> Value -> Memory -> Memory
setVal a d v p =
    p |> Array.set (d a p) v



-- Recursive program instruction decode/execute
--run : List Value -> List Value -> Address -> Prog -> ( (Prog, Value), List Value )
--run input output pc program =


run : VM -> VM
run vm =
    case instructionDecoder (getVal vm.pc directDecoder vm.memory) of
        -- Add
        ( 1, ( d1, d2, _ ) ) ->
            setVal (vm.pc + 3) indirectDecoder (getVal (vm.pc + 1) d1 vm.memory + getVal (vm.pc + 2) d2 vm.memory) vm.memory
                |> VM (vm.pc + 4) vm.input vm.output
                |> run

        -- Mult
        ( 2, ( d1, d2, _ ) ) ->
            setVal (vm.pc + 3) indirectDecoder (getVal (vm.pc + 1) d1 vm.memory * getVal (vm.pc + 2) d2 vm.memory) vm.memory
                |> VM (vm.pc + 4) vm.input vm.output
                |> run

        -- Input to Parameter 1 (Halt on empty input queue saving program counter)
        ( 3, ( _, _, _ ) ) ->
            List.head vm.input
                |> Maybe.map (\i -> setVal (vm.pc + 1) indirectDecoder i vm.memory |>
                              VM (vm.pc + 2) (Maybe.withDefault [] (List.tail vm.input)) vm.output |> run)
                |> Maybe.withDefault vm

        -- Output Parameter 1
        ( 4, ( d1, _, _ ) ) ->
            VM (vm.pc + 2) vm.input ((getVal (vm.pc + 1) d1 vm.memory) :: vm.output) vm.memory |> run

        -- Jump-if-true
        ( 5, ( d1, d2, _ ) ) ->
            if getVal (vm.pc + 1) d1 vm.memory /= 0 then
                VM (getVal (vm.pc + 2) d2 vm.memory) vm.input vm.output vm.memory |> run

            else
                VM (vm.pc + 3) vm.input vm.output vm.memory |> run

        -- Jump-if-false
        ( 6, ( d1, d2, _ ) ) ->
            if getVal (vm.pc + 1) d1 vm.memory == 0 then
                VM (getVal (vm.pc + 2) d2 vm.memory) vm.input vm.output vm.memory |> run

            else
                VM (vm.pc + 3) vm.input vm.output vm.memory |> run

        -- Less than
        ( 7, ( d1, d2, d3 ) ) ->
            let
                val =
                    if getVal (vm.pc + 1) d1 vm.memory < getVal (vm.pc + 2) d2 vm.memory then
                        1

                    else
                        0
            in
            vm.memory |> setVal (vm.pc + 3) d3 val |> VM (vm.pc + 4) vm.input vm.output |> run

        -- Equal
        ( 8, ( d1, d2, d3 ) ) ->
            let
                val =
                    if getVal (vm.pc + 1) d1 vm.memory == getVal (vm.pc + 2) d2 vm.memory then
                        1

                    else
                        0
            in
            vm.memory |> setVal (vm.pc + 3) d3 val |> VM (vm.pc + 4) vm.input vm.output |> run

        _ ->
            vm



-- Check for whether a given program and pc that the program has halted (or stalled waiting for input)


runComplete : VM -> Bool
runComplete vm =
    getVal vm.pc directDecoder vm.memory == 99
