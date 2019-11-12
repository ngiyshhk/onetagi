module Model.MainModel exposing (..)

import Basics exposing (..)
import Bool.Extra
import Dict
import Dict.Extra exposing (..)
import List exposing (..)
import List.FlatMap exposing (..)
import Maybe
import Model.Card exposing (..)
import Model.TagiInfo exposing (..)
import Random
import Util.ArrayUtil exposing (..)


type alias Model =
    { cards : List (List Card), restInfos : List TagiInfo, displayNum : Int, playerNum : Int }
