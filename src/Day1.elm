module Day1 exposing (sumFuel, sumFuelCumulative)

-- https://adventofcode.com/2019/day/1
-- Parse input string into list of ints


parse : String -> List Int
parse s =
    String.split "," s
        |> List.filterMap (\x -> String.toInt x)



-- Basic fuel consumption calculation


calc : Int -> Int
calc v =
    (v // 3) - 2



-- Calculate sum fuel for list of modules


sumFuel : String -> Int
sumFuel s =
    parse s
        |> List.map calc
        |> List.sum



-- Recursively count fuel requierd for payload + fuel


recalc : List Int -> Int -> List Int
recalc x v =
    let
        t =
            calc v
    in
    if t <= 0 then
        v :: x

    else
        recalc (v :: x) t



-- Calculate sum of fuel required including fuel weight


sumFuelCumulative : String -> Int
sumFuelCumulative s =
    parse s
        |> List.concatMap (\x -> Maybe.withDefault [] (List.tail (List.reverse (recalc [] x))))
        |> List.sum
