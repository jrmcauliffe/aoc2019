module Day1Test exposing (suite)

import Day1 exposing (..)
import Expect exposing (equal)
import Test exposing (..)


example1 =
    "12"


example2 =
    "14"


example3 =
    "1969"


example4 =
    "100756"


problem =
    "74767,108567,135114,103725,55085,144135,88766,94314,109095,114013,91594,97858,122165,80803,94873,98280,116305,66960,85105,97510,51829,50460,86361,71217,77310,68460,60591,109303,66381,139184,93497,116217,93193,92289,104371,74040,124924,125877,144950,139877,104798,148258,98386,145120,75609,80208,68458,138641,147555,81179,70443,108683,148921,64459,127861,83336,50123,102155,118397,139916,115265,112932,142676,106577,87480,122386,51573,61156,140013,87671,122005,82909,141790,61341,123625,91724,69630,112495,145851,79977,107629,130937,127680,56887,73639,68652,143813,50498,102140,55277,86773,53889,148907,94901,53640,129436,105184,71527,100433,56709"


suite : Test
suite =
    describe "Day1 Tests"
        [ describe "Part1 Examples"
            [ test "First Example" <|
                \_ -> sumFuel example1 |> equal 2
            , test "Second Example" <|
                \_ -> sumFuel example2 |> equal 2
            , test "Question Part 1" <|
                \_ ->
                    sumFuel problem |> equal 3282935
            ]
        , describe "Part2 Examples"
            [ test "Second Example" <|
                \_ -> sumFuelCumulative example2 |> equal 2
            , test "Third Example" <|
                \_ -> sumFuelCumulative example3 |> equal 966
            , test "Forth Example" <|
                \_ -> sumFuelCumulative example4 |> equal 50346
            , test "Question Part 1" <|
                \_ ->
                    sumFuelCumulative problem |> equal 4921542
            ]
        ]
