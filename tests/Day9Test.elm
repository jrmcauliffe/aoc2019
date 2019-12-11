module Day9Test exposing (..)

import Day5 exposing (..)
import Day7 exposing (..)
import Expect exposing (equal)
import Test exposing (..)
import Array exposing (toList)

example1 =
    "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"


suite : Test
suite =
    describe "Day7 Tests"
        [ describe "Part 1"
            [ test "Example 1" <|
                \_ ->
                    let
                        p =
                            example1 |> parse
                    in
                    p |> VM 0 [] [] 0 |> .output |> equal (Array.toList p)
            ]
            ]