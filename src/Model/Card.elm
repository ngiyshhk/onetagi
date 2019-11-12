module Model.Card exposing (..)

import List exposing (..)
import List.FlatMap exposing (..)
import Model.CardColor exposing (..)
import Random
import Util.ArrayUtil exposing (..)


type alias Card =
    { color : CardColor, num : Int }


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


makeCard : Int -> List Card
makeCard num =
    case num of
        5 ->
            repeat 2 Green
                |> List.map (\a -> { color = a, num = num })

        _ ->
            [ Red, Blue ]
                |> List.map (\a -> { color = a, num = num })


allcards : List Card
allcards =
    List.range 0 9
        |> flatMap makeCard


partCard : Int -> List Card -> List (List Card)
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
