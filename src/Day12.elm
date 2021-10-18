module Day12 exposing (..)

import Array exposing (Array)


type alias Body =
    { x : Int, y : Int, z : Int, vx : Int, vy : Int, vz : Int }


parseLine : String -> Maybe Body
parseLine s =
    let
        xyz : Array Int
        xyz =
            s
                |> String.split ","
                |> List.filterMap (\ss -> ss |> String.toList |> List.filter Char.isDigit |> String.fromList |> String.toInt)
                |> Array.fromList
    in
    Maybe.map3 (\x y z -> Body x y z 0 0 0) (Array.get 0 xyz) (Array.get 1 xyz) (Array.get 2 xyz)


parse : String -> List Body
parse s =
    s |> String.split "\n" |> List.filterMap parseLine


applyGravity : ( Body, Body ) -> ( Body, Body )
applyGravity p =
    let
        modx : ( Body, Body ) -> ( Body, Body )
        modx ( b1, b2 ) =
            if b1.x > b2.x then
                ( Body b1.x b1.y b1.z (b1.vx + 1) b1.vy b1.vz, Body b2.x b2.y b2.z (b2.vx - 1) b2.vy b2.vz )

            else if b1.x < b2.x then
                ( Body b1.x b1.y b1.z (b1.vx - 1) b1.vy b1.vz, Body b2.x b2.y b2.z (b2.vx + 1) b2.vy b2.vz )

            else
                ( b1, b2 )

        mody : ( Body, Body ) -> ( Body, Body )
        mody ( b1, b2 ) =
            if b1.y > b2.y then
                ( Body b1.x b1.y b1.z b1.vx (b1.vy + 1) b1.vz, Body b2.x b2.y b2.z b2.vx (b2.vy - 1) b2.vz )

            else if b1.y < b2.y then
                ( Body b1.x b1.y b1.z b1.vx (b1.vy - 1) b1.vz, Body b2.x b2.y b2.z b2.vx (b2.vy + 1) b2.vz )

            else
                ( b1, b2 )

        modz : ( Body, Body ) -> ( Body, Body )
        modz ( b1, b2 ) =
            if b1.z > b2.z then
                ( Body b1.x b1.y b1.z b1.vx b1.vy (b1.vz + 1), Body b2.x b2.y b2.z b2.vx b2.vy (b2.vz - 1) )

            else if b1.z < b2.z then
                ( Body b1.x b1.y b1.z b1.vx b1.vy (b1.vz - 1), Body b2.x b2.y b2.z b2.vx b2.vy (b2.vz + 1) )

            else
                ( b1, b2 )
    in
    case p of
        ( a, b ) ->
            ( a, b ) |> modx |> mody |> modz


pairs : List a -> List ( a, a )
pairs l =
    case l of
        [] ->
            []

        x :: [] ->
            []

        x :: xs ->
            xs |> List.map (\y -> ( x, y )) |> List.append (pairs xs)


applyVelocity : Body -> Body
applyVelocity b =
    Body (b.x + b.vx) (b.y + b.vy) (b.z + b.vz) b.vx b.vy b.vz
