module Day7Test exposing (..)

import Day5 exposing (..)
import Day7 exposing (..)
import Expect exposing (equal)
import Test exposing (..)


example1 =
    "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"


example2 =
    "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"


example3 =
    "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"


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
                    process ([ 4, 3, 2, 1, 0 ] |> List.map (Tuple.pair p)) 0 |> equal (Just 43210)
            , test "Example 2" <|
                \_ ->
                    let
                        p =
                            example2 |> parse
                    in
                    process ([ 0, 1, 2, 3, 4 ] |> List.map (Tuple.pair p)) 0 |> equal (Just 54321)
            , test "Example 3" <|
                \_ ->
                    let
                        p =
                            example3 |> parse
                    in
                    process ([ 1, 0, 4, 3, 2 ] |> List.map (Tuple.pair p)) 0 |> equal (Just 65210)
            ]
        ]
