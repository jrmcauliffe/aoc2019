module Day5Test exposing (suite)

import Array exposing (..)
import Day5 exposing (..)
import Expect exposing (equal)
import Test exposing (..)
import Tuple exposing (first, second)


example0 =
    "3,0,4,0,99"


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


example6 =
    "1101,100,-1,4,0"


day2problem =
    "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,1,9,23,27,2,27,9,31,1,31,5,35,2,35,9,39,1,39,10,43,2,43,13,47,1,47,6,51,2,51,10,55,1,9,55,59,2,6,59,63,1,63,6,67,1,67,10,71,1,71,10,75,2,9,75,79,1,5,79,83,2,9,83,87,1,87,9,91,2,91,13,95,1,95,9,99,1,99,6,103,2,103,6,107,1,107,5,111,1,13,111,115,2,115,6,119,1,119,5,123,1,2,123,127,1,6,127,0,99,2,14,0,0"


problem =
    "3,225,6,6,1100,1,238,225,104,0,1102,91,92,225,1102,85,13,225,1,47,17,224,101,-176,224,224,4,224,1002,223,8,223,1001,224,7,224,1,223,224,223,1102,79,43,225,1102,91,79,225,1101,94,61,225,1002,99,42,224,1001,224,-1890,224,4,224,1002,223,8,223,1001,224,6,224,1,224,223,223,102,77,52,224,1001,224,-4697,224,4,224,102,8,223,223,1001,224,7,224,1,224,223,223,1101,45,47,225,1001,43,93,224,1001,224,-172,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,1102,53,88,225,1101,64,75,225,2,14,129,224,101,-5888,224,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,101,60,126,224,101,-148,224,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1102,82,56,224,1001,224,-4592,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1101,22,82,224,1001,224,-104,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,226,677,224,102,2,223,223,1005,224,329,1001,223,1,223,1007,226,226,224,1002,223,2,223,1006,224,344,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,107,226,677,224,102,2,223,223,1006,224,374,101,1,223,223,8,677,677,224,102,2,223,223,1006,224,389,1001,223,1,223,1008,226,677,224,1002,223,2,223,1006,224,404,101,1,223,223,7,677,677,224,1002,223,2,223,1005,224,419,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,434,101,1,223,223,1108,226,226,224,102,2,223,223,1005,224,449,1001,223,1,223,107,226,226,224,102,2,223,223,1005,224,464,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,479,101,1,223,223,1007,226,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,524,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,539,101,1,223,223,108,226,677,224,1002,223,2,223,1005,224,554,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,1107,677,677,224,102,2,223,223,1005,224,584,1001,223,1,223,7,677,226,224,102,2,223,223,1005,224,599,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,614,1001,223,1,223,7,226,677,224,1002,223,2,223,1006,224,629,101,1,223,223,1107,677,226,224,1002,223,2,223,1005,224,644,1001,223,1,223,1107,226,677,224,102,2,223,223,1006,224,659,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,674,101,1,223,223,4,223,99,226"


suite : Test
suite =
    describe "Day5 Tests"
        [ describe "Day2 Examples"
            [ test "First Example" <|
                \_ -> parse example1 |> run [] [] 0 |> first |> equal (Array.fromList [ 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 ])
            , test "Second Example" <|
                \_ -> parse example2 |> run [] [] 0 |> first |> equal (Array.fromList [ 2, 0, 0, 0, 99 ])
            , test "Third Example" <|
                \_ -> parse example3 |> run [] [] 0 |> first |> equal (Array.fromList [ 2, 3, 0, 6, 99 ])
            , test "Forth Example" <|
                \_ -> parse example4 |> run [] [] 0 |> first |> equal (Array.fromList [ 2, 4, 4, 5, 99, 9801 ])
            , test "Fifth Example" <|
                \_ -> parse example5 |> run [] [] 0 |> first |> equal (Array.fromList [ 30, 1, 1, 4, 2, 5, 6, 0, 99 ])
            ]
        , describe "Part1 Examples"
            [ test "First Example" <|
                \_ -> parse example0 |> run [62] [] 0 |> second |> equal [ 62 ]
            , test "Sixth Example" <|
                \_ -> parse example6 |> run [] [] 0 |> first |> equal (Array.fromList [ 1101, 100, -1, 4, 99 ])
            , test "Day 2 problem" <|
                \_ -> parse day2problem |> Array.set 1 12 |> Array.set 2 2 |> run [] [] 0 |> first |> Array.get 0 |> equal (Just 9581917)
            , skip <|
                test "Problem part 1" <|
                    \_ ->
                        let
                            program =
                                parse problem
                        in
                        run [1] [] 0 program |> second |> List.head |> equal Nothing
            , skip <|
                test "Problem part 2" <|
                    \_ ->
                        let
                            program =
                                parse problem
                        in
                        run [5] [] 0 program |> second |> List.head |> equal Nothing
            ]
        ]
