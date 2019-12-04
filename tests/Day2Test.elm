module Day2Test exposing (suite)

import Array exposing (..)
import Day2 exposing (..)
import Expect exposing (equal)
import Test exposing (..)


example1 =
    "1,9,10,3,2,3,11,0,99,30,40,50"


example2 =
    "1,0,0,0,99"


example3 =
    "2,3,0,3,99"


example4 =
    "2,4,4,5,99,0"


example5 =
    "1,1,1,4,99,5,6,0,99"


problem =
    "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,1,9,23,27,2,27,9,31,1,31,5,35,2,35,9,39,1,39,10,43,2,43,13,47,1,47,6,51,2,51,10,55,1,9,55,59,2,6,59,63,1,63,6,67,1,67,10,71,1,71,10,75,2,9,75,79,1,5,79,83,2,9,83,87,1,87,9,91,2,91,13,95,1,95,9,99,1,99,6,103,2,103,6,107,1,107,5,111,1,13,111,115,2,115,6,119,1,119,5,123,1,2,123,127,1,6,127,0,99,2,14,0,0"


suite : Test
suite =
    describe "Day2 Tests"
        [ describe "Part1 Examples"
            [ test "First Example" <|
                \_ -> parse example1 |> run 0 |> equal (Array.fromList [ 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 ])
            , test "Second Example" <|
                \_ -> parse example2 |> run 0 |> equal (Array.fromList [ 2, 0, 0, 0, 99 ])
            , test "Third Example" <|
                \_ -> parse example3 |> run 0 |> equal (Array.fromList [ 2, 3, 0, 6, 99 ])
            , test "Forth Example" <|
                \_ -> parse example4 |> run 0 |> equal (Array.fromList [ 2, 4, 4, 5, 99, 9801 ])
            , test "Fifth Example" <|
                \_ -> parse example5 |> run 0 |> equal (Array.fromList [ 30, 1, 1, 4, 2, 5, 6, 0, 99 ])
            , test "Problem" <|
                \_ -> parse problem |> Array.set 1 12 |> Array.set 2 2 |> run 0 |> Array.get 0 |> equal (Just 9581917)
            ]
        , describe "Part2 Examples"
            [ test "First Example" <|
                \_ ->
                    let
                        correctProg =
                            parse problem |> findOutput 0 99 19690720
                    in
                    Maybe.map2 (\n v -> (100 * n) + v) (correctProg |> Maybe.map (\p -> getVal 1 p)) (correctProg |> Maybe.map (\p -> getVal 2 p))
                        |> equal (Just 2505)
            ]
        ]
