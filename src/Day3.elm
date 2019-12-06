module Day3 exposing (Direction(..), Distance, Line, Point, closestIntersection, detectCrossing, parse, parseList, shortestPath, toLine, toLines)

import Tuple exposing (first, second)


type Direction
    = Left
    | Right
    | Up
    | Down


type alias Distance =
    Int


type alias Point =
    ( Int, Int )


type alias Line =
    ( Point, Point )


parseList : String -> List ( Direction, Distance )
parseList s =
    String.split "," s
        |> List.filterMap parse



-- Convert a single direction string to a typed representation


parse : String -> Maybe ( Direction, Distance )
parse s =
    case String.toList s of
        d :: num ->
            case d of
                'U' ->
                    Maybe.map (\n -> Tuple.pair Up n) (String.toInt <| String.fromList num)

                'D' ->
                    Maybe.map (\n -> Tuple.pair Down n) (String.toInt <| String.fromList num)

                'L' ->
                    Maybe.map (\n -> Tuple.pair Left n) (String.toInt <| String.fromList num)

                'R' ->
                    Maybe.map (\n -> Tuple.pair Right n) (String.toInt <| String.fromList num)

                _ ->
                    Nothing

        _ ->
            Nothing



-- Convert starting point and direction/distance into a line


toLine : Point -> ( Direction, Distance ) -> Line
toLine point ( direction, distance ) =
    case direction of
        Up ->
            ( point, ( first point, second point + distance ) )

        Down ->
            ( point, ( first point, second point - distance ) )

        Left ->
            ( point, ( first point - distance, second point ) )

        Right ->
            ( point, ( first point + distance, second point ) )



-- For a starting point and list of directions/distances, create a set of lines representing this


toLines : Point -> List ( Direction, Distance ) -> List Line
toLines point l =
    case l of
        x :: xs ->
            let
                new =
                    toLine point x
            in
            new :: toLines (second new) xs

        [] ->
            []



-- Dectect if two lines cross and return crossing point if the do


detectCrossing : Line -> Line -> Maybe Point
detectCrossing ( ( x1, y1 ), ( x2, y2 ) ) ( ( x3, y3 ), ( x4, y4 ) ) =
    if between x3 x4 x1 && between x3 x4 x2 && between y1 y2 y3 && between y1 y2 y4 then
        Just ( x1, y3 )

    else if between y3 y4 y1 && between y3 y4 y2 && between x1 x2 x3 && between x1 x2 x4 then
        Just ( x3, y1 )

    else
        Nothing



-- Is an integer between two other integers?


between : Int -> Int -> Int -> Bool
between a b m =
    ((m > a) && (m < b)) || ((m > b) && (m < a))



-- Find the closest intersection between two paths


closestIntersection : ( String, String ) -> Maybe Int
closestIntersection t =
    let
        a =
            first t |> parseList |> toLines ( 0, 0 )

        b =
            second t |> parseList |> toLines ( 0, 0 )
    in
    List.concatMap (\x -> List.filterMap (\y -> detectCrossing x y) a) b
        |> List.map (\x -> abs (first x) + abs (second x))
        |> List.sort
        |> List.head


length : Line -> Int
length ( ( x1, y1 ), ( x2, y2 ) ) =
    abs (x1 - x2) + abs (y1 - y2)


aggLength : List Line -> List ( Line, Int )
aggLength l =
    l |> List.map (\x -> ( x, length x ))


cumSum : number -> List number -> List number
cumSum i l =
    case l of
        x :: xs ->
            (i + x) :: cumSum (i + x) xs

        _ ->
            []


shortestPath : ( String, String ) -> Maybe Int
shortestPath t =
    let
        a =
            first t |> parseList |> toLines ( 0, 0 )

        agga =
            aggLength

        b =
            second t |> parseList |> toLines ( 0, 0 )
    in
    Nothing
