module Model.MainModel exposing (..)

import Basics exposing (..)
import Bool.Extra
import Dict
import Dict.Extra exposing (..)
import List exposing (..)
import List.FlatMap exposing (..)
import Maybe
import Random
import Util.ArrayUtil exposing (..)


type Msg
    = Start
    | DeliveryCard ( List (List Card), List TagiInfo )
    | Info
    | Answer


type alias Card =
    { color : CardColor, num : Int }


type CardColor
    = Red
    | Blue
    | Green


type alias Model =
    { cards : List (List Card), restInfos : List TagiInfo, displayNum : Int, playerNum : Int }


type alias TagiInfo =
    { text : String, f : List Card -> String }


cardToTuple : Card -> ( Int, Int )
cardToTuple card =
    case card.color of
        Red ->
            ( card.num, 0 )

        Blue ->
            ( card.num, 1 )

        Green ->
            ( card.num, 2 )


tupleToCard : ( Int, Int ) -> Card
tupleToCard tuple =
    case tuple of
        ( n, 0 ) ->
            { color = Red, num = n }

        ( n, 1 ) ->
            { color = Blue, num = n }

        ( n, _ ) ->
            { color = Green, num = n }


sortCards : List Card -> List Card
sortCards cards =
    cards |> map cardToTuple |> sort |> map tupleToCard


sortCardsList : List (List Card) -> List (List Card)
sortCardsList cardListList =
    cardListList |> map sortCards


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


toString a =
    case a of
        Blue ->
            "bg-primary"

        Green ->
            "bg-success"

        Red ->
            "bg-danger"


makeCard num =
    case num of
        5 ->
            repeat 2 Green
                |> List.map (\a -> { color = a, num = num })

        _ ->
            [ Red, Blue ]
                |> List.map (\a -> { color = a, num = num })


allcards =
    List.range 0 9
        |> flatMap makeCard


partCard n cards =
    case n of
        4 ->
            [ take 4 cards
            , take 4 <| drop 4 cards
            , take 4 <| drop 8 cards
            , take 4 <| drop 12 cards
            , take 4 <| drop 16 cards
            ]

        _ ->
            [ take 5 cards
            , take 5 <| drop 5 cards
            , take 5 <| drop 10 cards
            , take 5 <| drop 15 cards
            ]


delivCard : Int -> Random.Generator (List (List Card))
delivCard n =
    shuffleList allcards
        |> Random.map (partCard n)
