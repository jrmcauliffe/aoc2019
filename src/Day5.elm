module Day5 exposing (..)

import Array exposing (..)



-- https://adventofcode.com/2019/day/5


type alias Memory =
    Array Int


type alias VM =
    { pc : Int
    , input : List Value
    , output : List Value
    , baseOffset: Int
    , memory : Memory
    }


type alias Address =
    Int


type alias Value =
    Int


type alias AddressDecoder =
    Address -> Int -> Memory -> Address


directDecoder : AddressDecoder
directDecoder address _ _ =
    address

relativeDecoder : AddressDecoder
relativeDecoder address base program = Array.get (address) program |> Maybe.map ((+) base) |> Maybe.withDefault -1
indirectDecoder : AddressDecoder
indirectDecoder address _ program = Array.get (address) program |> Maybe.withDefault -1

addressModeDecoder m =
    case m of
      1 -> directDecoder
      2 -> relativeDecoder
      _ -> indirectDecoder


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


getVal : Address -> AddressDecoder -> VM -> Value
getVal a d vm =
    vm.memory |> Array.get (d a vm.baseOffset vm.memory) |> Maybe.withDefault -1


setVal : Address -> AddressDecoder -> Value -> VM -> Memory
setVal a d v vm =
    vm.memory |> Array.set (d a vm.baseOffset vm.memory) v



-- Recursive program instruction decode/execute

run : VM -> VM
run vm =
    case instructionDecoder (getVal vm.pc directDecoder vm) of
        -- Add
        ( 1, ( d1, d2, d3 ) ) ->
            setVal (vm.pc + 3) d3 (getVal (vm.pc + 1) d1 vm + getVal (vm.pc + 2) d2 vm) vm
                |> VM (vm.pc + 4) vm.input vm.output vm.baseOffset
                |> run

        -- Mult
        ( 2, ( d1, d2, d3 ) ) ->
            setVal (vm.pc + 3) d3 (getVal (vm.pc + 1) d1 vm * getVal (vm.pc + 2) d2 vm) vm
                |> VM (vm.pc + 4) vm.input vm.output vm.baseOffset
                |> run

        -- Input to Parameter 1 (Halt on empty input queue saving program counter)
        ( 3, ( d1, _, _ ) ) ->
            List.head vm.input
                |> Maybe.map
                    (\i ->
                        setVal (vm.pc + 1) d1  i vm
                            |> VM (vm.pc + 2) (Maybe.withDefault [] (List.tail vm.input)) vm.output vm.baseOffset
                            |> run
                    )
                |> Maybe.withDefault vm

        -- Output Parameter 1
        ( 4, ( d1, _, _ ) ) ->
            VM (vm.pc + 2) vm.input ((getVal (vm.pc + 1) d1 vm) :: vm.output) vm.baseOffset vm.memory |> run

        -- Jump-if-true
        ( 5, ( d1, d2, _ ) ) ->
            if getVal (vm.pc + 1) d1 vm /= 0 then
                VM (getVal (vm.pc + 2) d2 vm) vm.input vm.output vm.baseOffset vm.memory |> run

            else
                VM (vm.pc + 3) vm.input vm.output vm.baseOffset vm.memory |> run

        -- Jump-if-false
        ( 6, ( d1, d2, _ ) ) ->
            if getVal (vm.pc + 1) d1 vm == 0 then
                VM (getVal (vm.pc + 2) d2 vm) vm.input vm.output vm.baseOffset vm.memory |> run

            else
                VM (vm.pc + 3) vm.input vm.output vm.baseOffset vm.memory |> run

        -- Less than
        ( 7, ( d1, d2, d3 ) ) ->
            let
                val =
                    if getVal (vm.pc + 1) d1 vm < getVal (vm.pc + 2) d2 vm then
                        1

                    else
                        0
            in
            vm |> setVal (vm.pc + 3) d3 val |> VM (vm.pc + 4) vm.input vm.output vm.baseOffset |> run

        -- Equal
        ( 8, ( d1, d2, d3 ) ) ->
            let
                val =
                    if getVal (vm.pc + 1) d1 vm == getVal (vm.pc + 2) d2 vm then
                        1

                    else
                        0
            in
            vm |> setVal (vm.pc + 3) d3 val |> VM (vm.pc + 4) vm.input vm.output vm.baseOffset |> run

        -- Set base offset
        ( 9, ( d, _, _)) ->
            {vm | baseOffset = getVal (vm.pc+1) d vm, pc = vm.pc+2 } |> run
        _ ->
            vm



-- Check for whether a given program and pc that the program has halted (or stalled waiting for input)


runComplete : VM -> Bool
runComplete vm =
    getVal vm.pc directDecoder vm == 99
