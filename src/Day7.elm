module Day7 exposing (..)

import Day5 exposing (..)
import Tuple exposing (first, second)



-- https://adventofcode.com/2019/day/7


process : List VM -> Value -> Maybe Value
process stages i =
    let
        runWithInput vm ii =
            { vm | input = ii :: vm.input } |> run
    in
    case stages of
        stage :: [] ->
            runWithInput stage i |> .output |> List.head

        stage :: rest ->
            let
                out =
                    runWithInput stage i
            in
            out |> .output |> List.head
                |> Maybe.andThen
                    (process
                        (if runComplete out then
                            rest

                         else
                            List.append rest [ out ]
                        )
                    )

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



-- maxPower : List Int -> Int -> Memory -> Maybe ( List Int, Int )


maxPower phaseSeq initialVal memory =
    permutations phaseSeq
        |> List.map (\ps -> ( ps, initialVal |> process (List.map (\phase -> VM 0 [ phase ] [] memory |> run) ps) ))
        |> List.map (\x -> ( first x, Maybe.withDefault 0 (second x) ))
        |> List.sortBy second
        |> List.reverse
        |> List.head
