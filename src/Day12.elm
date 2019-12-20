module Day12 exposing (..)

part1: List Int -> List Int
part1 input =
  let
    basePattern = [0,1,0,-1]
    output = List.map2 Tuple.pair input basePattern
             |> List.map (\x -> (Tuple.first x) * (Tuple.second x))
             |> List.filterMap lsd
    rest = List.drop (List.length output) input
  in
  case rest of
    [] -> output
    _ -> List.append output (part1 rest)
   
lsd: Int -> Maybe Int
lsd input = input |> String.fromInt |> String.right 1 |> String.toInt

repeat: Int -> List Int -> List Int
repeat n l = l |> List.concatMap (\x -> List.repeat n x)
