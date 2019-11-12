module Util.ArrayUtil exposing (..)

import List exposing (..)
import Random


shuffleList : List t -> Random.Generator (List t)
shuffleList aList =
    Random.int 0 10000
        |> Random.list (List.length aList)
        |> Random.map (\l -> List.map2 (\a b -> ( a, b )) l aList)
        |> Random.map (sortBy Tuple.first)
        |> Random.map (List.map (\a -> Tuple.second a))
