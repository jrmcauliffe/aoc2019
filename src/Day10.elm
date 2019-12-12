module Day10 exposing (bestLocation, parse)

import Array exposing (..)



-- https://adventofcode.com/2019/day/10


type alias Asteroid =
    { x : Int
    , y : Int
    }


type alias Vector =
    { x : Int, y : Int }



-- Parse input string into list of Asteroids


parse : String -> List Asteroid
parse s =
    let
        asteroid a =
            case a of
                ( ( x, y ), '#' ) ->
                    Just (Asteroid x y)

                _ ->
                    Nothing
    in
    s
        |> String.filter ((/=) ' ')
        |> String.split "\n"
        |> List.indexedMap Tuple.pair
        |> List.concatMap (\y -> Tuple.second y |> String.toList |> List.indexedMap Tuple.pair |> List.map (\x -> ( ( Tuple.first x, Tuple.first y ), Tuple.second x )))
        |> List.filterMap asteroid



-- Is the view of b from a occluded by c?


occludes : Asteroid -> Asteroid -> Asteroid -> Bool
occludes a b c =
    let
        vector : Asteroid -> Asteroid -> Vector
        vector aa bb =
            { x = bb.x - aa.x, y = bb.y - aa.y }

        vb =
            vector a b

        vc =
            vector a c
    in
    ((vb.x * vc.y) == (vc.x * vb.y)) && (abs vc.x <= abs vb.x) && (vb.x * vc.x >= 0) && (abs vc.y <= abs vb.y) && (vb.y * vc.y >= 0)


bestLocation : List Asteroid -> Maybe ( Asteroid, Int )
bestLocation asteroids =
    let
        visibleFrom : Asteroid -> List Asteroid -> Int
        visibleFrom a rest =
            rest |> List.map (\b -> rest |> List.filter ((/=) b) |> List.map (occludes a b)) |> List.filter (\q -> not (List.member True q)) |> List.length
    in
    asteroids |> List.map (\a -> asteroids |> List.filter ((/=) a) |> visibleFrom a |> Tuple.pair a) |> List.sortBy Tuple.second |> List.reverse |> List.head
