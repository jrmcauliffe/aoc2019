module Day10 exposing (bestLocation, murderOrder, parse)

import List.Extra



-- https://adventofcode.com/2019/day/10


type alias Asteroid =
    { x : Int
    , y : Int
    }


type alias Vector =
    { x : Int, y : Int }


type alias PolarType =
    { a : Float, d : Float }



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


vector : Asteroid -> Asteroid -> Vector
vector aa bb =
    { x = bb.x - aa.x, y = bb.y - aa.y }


occludes : Asteroid -> Asteroid -> Asteroid -> Bool
occludes a b c =
    let
        vb =
            vector a b

        vc =
            vector a c
    in
    ((vb.x * vc.y) == (vc.x * vb.y)) && (abs vc.x <= abs vb.x) && (vb.x * vc.x >= 0) && (abs vc.y <= abs vb.y) && (vb.y * vc.y >= 0)



-- Find the asteroid that can see the highest number of other asteroids


bestLocation : List Asteroid -> Maybe ( Asteroid, Int )
bestLocation asteroids =
    let
        visibleFrom : Asteroid -> List Asteroid -> Int
        visibleFrom a rest =
            rest |> List.map (\b -> rest |> List.filter ((/=) b) |> List.map (occludes a b)) |> List.filter (\q -> not (List.member True q)) |> List.length
    in
    asteroids |> List.map (\a -> asteroids |> List.filter ((/=) a) |> visibleFrom a |> Tuple.pair a) |> List.sortBy Tuple.second |> List.reverse |> List.head


ang : Vector -> Float
ang v =
    0 - (atan2 (toFloat v.x) (toFloat v.y) - pi)


dist : Vector -> Float
dist v =
    let
        x =
            toFloat v.x

        y =
            toFloat v.y
    in
    sqrt (x * x + y * y)


zpolar : Vector -> PolarType
zpolar v =
    PolarType (ang v) (dist v)



-- Sort by angle then by distance


ascendingPolar : ( Asteroid, PolarType ) -> ( Asteroid, PolarType ) -> Order
ascendingPolar a b =
    case ( a, b ) of
        ( ( _, p1 ), ( _, p2 ) ) ->
            case compare p1.a p2.a of
                LT ->
                    LT

                GT ->
                    GT

                EQ ->
                    compare p1.d p2.d



-- Find out the order that the asteroids will be shot from a given base asteroid


murderOrder : List Asteroid -> Asteroid -> List Asteroid
murderOrder asteroids base =
    let
        sendToBack : Asteroid -> List Asteroid -> List Asteroid
        sendToBack b rest =
            List.append (List.Extra.dropWhile (\c -> occludes base c b) rest) (List.Extra.takeWhile (\c -> occludes base c b) rest)

        startShooting : List Asteroid -> List Asteroid
        startShooting targets =
            case targets of
                target :: rest ->
                    target :: startShooting (sendToBack target rest)

                [] ->
                    []
    in
    asteroids
        |> List.filter ((/=) base)
        |> List.map (\a -> ( a, vector base a |> zpolar ))
        |> List.sortWith ascendingPolar
        |> List.map Tuple.first
        |> startShooting



--
