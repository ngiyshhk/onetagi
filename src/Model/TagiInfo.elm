module Model.TagiInfo exposing (..)

import Bool.Extra
import Dict
import Dict.Extra exposing (..)
import List exposing (..)
import Model.Card exposing (..)
import Model.CardColor exposing (..)


type alias TagiInfo =
    { text : String, f : List Card -> String }


infos : List TagiInfo
infos =
    [ { text = "真ん中の合計数"
      , f =
            \list ->
                map2 (\a b -> ( a, b )) (List.range 0 10) list
                    |> filter (\t -> member (Tuple.first t) [ 1, 2, 3 ])
                    |> List.map Tuple.second
                    |> List.map .num
                    |> sum
                    |> String.fromInt
      }
    , { text = "左の合計数"
      , f =
            \list ->
                map2 (\a b -> ( a, b )) (List.range 0 10) list
                    |> filter (\t -> member (Tuple.first t) [ 0, 1, 2 ])
                    |> List.map Tuple.second
                    |> List.map .num
                    |> sum
                    |> String.fromInt
      }
    , { text = "右の合計数"
      , f =
            \list ->
                map2 (\a b -> ( a, b )) (List.range 0 10) list
                    |> filter (\t -> member (Tuple.first t) [ 2, 3, 4 ])
                    |> List.map Tuple.second
                    |> List.map .num
                    |> sum
                    |> String.fromInt
      }
    , { text = "赤の合計数"
      , f =
            \list ->
                list
                    |> filter (\c -> c.color == Red)
                    |> List.map .num
                    |> sum
                    |> String.fromInt
      }
    , { text = "青の合計数"
      , f =
            \list ->
                list
                    |> filter (\c -> c.color == Blue)
                    |> List.map .num
                    |> sum
                    |> String.fromInt
      }
    , { text = "タイルすべての合計数"
      , f =
            \list ->
                list
                    |> List.map .num
                    |> sum
                    |> String.fromInt
      }
    , { text = "0はどこ"
      , f = discoveryNum 0
      }
    , { text = "3はどこ"
      , f = discoveryNum 3
      }
    , { text = "5はどこ"
      , f = discoveryNum 5
      }
    , { text = "7はどこ"
      , f = discoveryNum 7
      }
    , { text = "9はどこ"
      , f = discoveryNum 9
      }
    , { text = "奇数は何枚"
      , f =
            \list ->
                list
                    |> filter (\c -> modBy 2 c.num == 1)
                    |> length
                    |> String.fromInt
      }
    , { text = "偶数は何枚"
      , f =
            \list ->
                list
                    |> filter (\c -> modBy 2 c.num == 0)
                    |> length
                    |> String.fromInt
      }
    , { text = "青は何枚"
      , f =
            \list ->
                list
                    |> filter (\c -> c.color == Blue)
                    |> length
                    |> String.fromInt
      }
    , { text = "同じ数字タイルのペアは何組"
      , f =
            \list ->
                list
                    |> groupBy .num
                    |> Dict.toList
                    |> filter (\a -> 2 == length (Tuple.second a))
                    |> length
                    |> String.fromInt
      }
    , { text = "中央の数字タイルは5以上？"
      , f =
            \list ->
                list
                    |> drop 2
                    |> head
                    |> Maybe.map .num
                    |> Maybe.map (\a -> a >= 5)
                    |> Maybe.withDefault False
                    |> Bool.Extra.toString
      }
    , { text = "最大から最小を引いた数"
      , f =
            \list ->
                let
                    nums =
                        map .num list

                    maxNumM =
                        maximum nums |> Maybe.withDefault 0

                    minNumM =
                        minimum nums |> Maybe.withDefault 0
                in
                String.fromInt (maxNumM - minNumM)
      }
    ]


discoveryNum n list =
    list
        |> map2 (\a b -> ( a, b )) (List.range 1 10)
        |> filter (\t -> n == (Tuple.second t).num)
        |> List.map Tuple.first
        |> List.sort
        |> List.map String.fromInt
        |> String.join ", "
