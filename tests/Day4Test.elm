module Day4Test exposing (suite)

import Day4 exposing (..)
import Expect exposing (equal)
import Test exposing (..)


problem =
    ( 172851, 675869 )


suite : Test
suite =
    describe "Day4 Tests"
        [ describe "isValid Test"
            [ test "Example 1" <|
                \_ -> 111111 |> isValid |> equal True
            , test "Example 2" <|
                \_ -> 223450 |> isValid |> equal False
            , test "Example 3" <|
                \_ -> 123789 |> isValid |> equal False
            , test "Too short" <|
                \_ -> 12345 |> isValid |> equal False
            , test "Too long" <|
                \_ -> 1234567 |> isValid |> equal False
            ]
        , describe "isValidAdv Test"
            [ test "Example 1" <|
                \_ -> 112233 |> isValidPart2 |> equal True
            , test "Example 2" <|
                \_ -> 123444 |> isValidPart2 |> equal False
            , test "Example 3" <|
                \_ -> 111122 |> isValidPart2 |> equal True
            ]
        , describe "validRange Test"
            [ test "Problem Part 1" <|
                \_ -> problem |> validKeys isValid |> List.length |> equal 1660
            , test "Problem Part 2" <|
                \_ -> problem |> validKeys isValidPart2 |> List.length |> equal 1135
            ]
        ]
