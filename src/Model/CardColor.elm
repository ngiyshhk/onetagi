module Model.CardColor exposing (..)


type CardColor
    = Red
    | Blue
    | Green


toString : CardColor -> String
toString a =
    case a of
        Blue ->
            "bg-primary"

        Green ->
            "bg-success"

        Red ->
            "bg-danger"
