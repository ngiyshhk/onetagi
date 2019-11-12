module Model.Msg exposing (Msg(..))

import Model.Card exposing (..)
import Model.TagiInfo exposing (..)


type Msg
    = Start
    | DeliveryCard ( List (List Card), List TagiInfo )
    | Info
    | Answer
