module Day8 exposing (decode, parse, part1, render)

import Tuple exposing (first, pair, second)



-- https://adventofcode.com/2019/day/8


type alias Line =
    List Char


type alias Layer =
    List Line


type alias Image =
    List Layer


parse : Int -> Int -> String -> Image
parse w h s =
    case String.toList s of
        [] ->
            []

        remaining ->
            let
                l =
                    remaining |> List.take (w * h) |> parseLayer w |> List.singleton
            in
            remaining |> List.drop (w * h) |> String.fromList |> parse w h |> List.append l


parseLayer : Int -> List Char -> Layer
parseLayer w s =
    case s of
        [] ->
            []

        remaining ->
            let
                l =
                    remaining |> List.take w |> List.singleton
            in
            remaining |> List.drop w |> parseLayer w |> List.append l



-- Solution to Part 1


part1 : Image -> Maybe Int
part1 i =
    let
        charCount : Char -> Layer -> Int
        charCount c layer =
            layer |> List.concat |> List.filter ((==) c) |> List.length

        fewestZeros =
            i
                |> List.map (\layer -> layer |> charCount '0' |> pair layer)
                |> List.sortBy second
                |> List.head
                |> Maybe.map first

        onesCount =
            fewestZeros |> Maybe.map (charCount '1')

        twosCount =
            fewestZeros |> Maybe.map (charCount '2')
    in
    Maybe.map2 (*) onesCount twosCount



-- Get correct layer values using transparency '2'


superImpose : Layer -> Layer -> Layer
superImpose upper lower =
    let
        len =
            upper |> List.head |> Maybe.map List.length |> Maybe.withDefault 0
    in
    List.map2
        (\u l ->
            if u == '2' then
                l

            else
                u
        )
        (List.concat upper)
        (List.concat lower)
        |> parseLayer len



-- Apply superpositon progressively from lowest to hightest layer


decode : Image -> Layer
decode i =
    let
        applyOver : Layer -> List Layer -> Layer
        applyOver l rest =
            case rest of
                x :: xs ->
                    applyOver (superImpose x l) xs

                [] ->
                    l
    in
    case List.reverse i of
        x :: xs ->
            applyOver x xs

        [] ->
            [ [] ]



-- Render the image


render : Layer -> String
render l =
    l
        |> List.map
            (\r ->
                r
                    |> List.map
                        (\c ->
                            if c == '1' then
                                '█'

                            else
                                ' '
                        )
                    |> String.fromList
            )
        |> String.join (String.fromChar '\n')
