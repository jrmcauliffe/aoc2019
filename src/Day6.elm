module Day6 exposing (..)

import Dict exposing (..)
import Tuple exposing (first, pair, second)



-- https://adventofcode.com/2019/day/6


type alias Orbit =
    String


type OrbitMap
    = Node ( Orbit, List OrbitMap )



-- Parse an individual orbit string into an Orbit


parseOrbit : String -> Maybe ( Orbit, Orbit )
parseOrbit o =
    case String.split ")" o of
        a :: b :: [] ->
            Just ( a, b )

        _ ->
            Nothing


gatherChildren : Orbit -> List ( Orbit, Orbit ) -> OrbitMap
gatherChildren p allOrbits =
    let
        children =
            allOrbits |> List.filter (\( o, _ ) -> o == p) |> List.map second
    in
    children |> List.map (\c -> gatherChildren c allOrbits) |> pair p |> Node



-- Parse input string of orbits into OrbitMap


parse : String -> OrbitMap
parse s =
    let
        orbitList =
            s |> String.split "," |> List.filterMap parseOrbit
    in
    gatherChildren "COM" orbitList



-- The number of direct and indirect orbits


orbitCount : OrbitMap -> Int
orbitCount orbitMap =
    let
        agg depth o =
            case o of
                Node ( _, m ) ->
                    List.foldl (\oo count -> agg (depth + 1) oo + count + depth + 1) 0 m
    in
    agg 0 orbitMap
