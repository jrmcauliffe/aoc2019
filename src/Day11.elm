module Day11 exposing (..)

import Day5 exposing (Memory, VM, run, runComplete)
import Dict exposing (Dict)


type alias Panel =
    ( Int, Int )


type Colour
    = Black
    | White
    | Unpainted


type Rotation
    = CW
    | CCW
    | Unknown


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Ship =
    Dict Panel Colour


type alias Robot =
    { vm : VM, loc : Panel, dir : Direction }


encColour : Colour -> Int
encColour c =
    case c of
        Black ->
            0

        White ->
            1

        Unpainted ->
            -1


decColour : Int -> Colour
decColour c =
    case c of
        0 ->
            Black

        1 ->
            White

        _ ->
            Unpainted


decRotation : Int -> Rotation
decRotation r =
    case r of
        0 ->
            CCW

        1 ->
            CW

        _ ->
            Unknown


readPanel : Ship -> Panel -> Colour
readPanel ship panel =
    ship |> Dict.get panel |> Maybe.withDefault Black


writePanel : Ship -> Panel -> Colour -> Ship
writePanel ship panel colour =
    ship |> Dict.insert panel colour


inputData : VM -> Colour -> VM
inputData vm i =
    { vm | input = encColour i :: vm.input }


outputData : VM -> ( Maybe Int, VM )
outputData vm =
    ( vm.output |> List.head, { vm | output = vm.output |> List.drop 1 } )


move : Direction -> Panel -> Panel
move d p =
    case d of
        Up ->
            p |> Tuple.mapSecond (\y -> y + 1)

        Down ->
            p |> Tuple.mapSecond (\y -> y - 1)

        Right ->
            p |> Tuple.mapFirst (\x -> x + 1)

        Left ->
            p |> Tuple.mapFirst (\x -> x - 1)


rotate : Rotation -> Direction -> Direction
rotate r d =
    case ( r, d ) of
        ( CW, Up ) ->
            Right

        ( CCW, Up ) ->
            Left

        ( CW, Down ) ->
            Left

        ( CCW, Down ) ->
            Right

        ( CW, Left ) ->
            Up

        ( CCW, Left ) ->
            Down

        ( CW, Right ) ->
            Down

        ( CCW, Right ) ->
            Up

        ( Unknown, dir ) ->
            dir


robotStep : ( Robot, Ship ) -> ( Robot, Ship )
robotStep ( r, s ) =
    let
        -- input colour
        vmIn =
            r.loc |> readPanel s |> inputData r.vm |> run

        -- read output
        vmColour : ( Maybe Colour, VM )
        vmColour =
            vmIn |> outputData |> Tuple.mapFirst (\x -> Maybe.map decColour x)

        vmDirection : ( Maybe Rotation, VM )
        vmDirection =
            vmColour |> Tuple.second |> outputData |> Tuple.mapFirst (\x -> Maybe.map decRotation x)

        -- paint ship
        paintedShip : Maybe Ship
        paintedShip =
            vmColour |> Tuple.first |> Maybe.map (writePanel s r.loc)

        -- rotate robot
        rotatedRobot : Maybe Robot
        rotatedRobot = vmDirection |> Tuple.first |> Maybe.map (\rot -> {r | dir = rotate rot r.dir, vm = vmDirection |> Tuple.second})

        -- move robot
        movedRobot : Maybe Robot
        movedRobot = rotatedRobot |> Maybe.map (\rr -> { rr | loc = move rr.dir rr.loc})
    in
    Maybe.map2 (\nr ns -> ( nr, ns )) movedRobot paintedShip |> Maybe.withDefault ( r, s )
    
robotLoop : ( Robot, Ship ) -> ( Robot, Ship )
robotLoop next = 
    if next |> Tuple.first |> .vm |> runComplete then
        next
    else
        robotLoop next


panelsPainted : Memory -> Int
panelsPainted prog =
    let
        initialVm : VM
        initialVm =
            VM 0 [] [] 0 prog

        robot : Robot
        robot =
            Robot initialVm ( 0, 0 ) Up

        ship : Ship
        ship =
            Dict.singleton ( 0, 0 ) Black
    in
    ( robot, ship ) |> robotLoop |> Tuple.second |> Dict.size
