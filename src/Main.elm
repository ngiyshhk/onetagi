module Main exposing (..)

import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import List.FlatMap exposing (..)
import Model.MainModel exposing (..)
import Random
import Tuple
import Util.ArrayUtil exposing (..)


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cards = [ [] ], restInfos = [], displayNum = 1, playerNum = 3 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | displayNum = 1 }, Random.generate DeliveryCard <| Random.map2 (\a b -> ( a, b )) (delivCard model.playerNum) (shuffleList infos) )

        DeliveryCard ( cards, newInfos ) ->
            ( { model | cards = sortCardsList cards, restInfos = newInfos }, Cmd.none )

        Info ->
            ( { model | restInfos = tail model.restInfos |> Maybe.withDefault [] }, Cmd.none )

        Answer ->
            ( { model | displayNum = 5 }, Cmd.none )


viewCard : Card -> Html Msg
viewCard card =
    Card.config [ Card.outlinePrimary, Card.align Text.alignXlCenter ]
        |> Card.headerH4 [ class <| toString card.color ] [ text <| String.fromInt card.num ]
        |> Card.view


viewInfo : Model -> List (Html Msg)
viewInfo model =
    case model.restInfos of
        tagInfo :: rest ->
            [ Grid.row [ Row.attrs [ Spacing.mt3 ] ]
                [ Grid.col []
                    [ Card.config [ Card.outlinePrimary ]
                        |> Card.headerH4 [] [ text tagInfo.text ]
                        |> Card.block []
                            [ Block.text []
                                [ text ("P1: " ++ (tagInfo.f <| Maybe.withDefault [] <| head <| drop 1 model.cards)) ]
                            , Block.text []
                                [ text ("P2: " ++ (tagInfo.f <| Maybe.withDefault [] <| head <| drop 2 model.cards)) ]
                            ]
                        |> Card.view
                    ]
                ]
            ]

        _ ->
            []


mainContent : Model -> Html Msg
mainContent model =
    List.map
        (\r -> r |> List.map (\c -> Grid.col [] [ viewCard c ]) |> Grid.row [ Row.attrs [ Spacing.mt3 ] ])
        (take model.displayNum model.cards)
        |> List.append
            (viewInfo model)
        |> List.append
            [ h1 [] [ text "ワンタギ" ]
            , Grid.row []
                [ Grid.col []
                    [ Card.config [ Card.outlinePrimary ]
                        |> Card.headerH4 [] [ text "説明" ]
                        |> Card.block []
                            [ Block.text [] [ text "Startで開始。Infoで次の情報。Answerで全員の情報出力。player3人想定" ]
                            , Block.custom <|
                                Button.button [ Button.primary, Button.attrs [ onClick Start, Spacing.ml1 ] ] [ text "Start" ]
                            , Block.custom <|
                                Button.button [ Button.info, Button.attrs [ onClick Info, Spacing.ml1 ] ] [ text "Info" ]
                            , Block.custom <|
                                Button.button [ Button.success, Button.attrs [ onClick Answer, Spacing.ml1 ] ] [ text "Answer" ]
                            ]
                        |> Card.view
                    ]
                ]
            ]
        |> Grid.container []


view model =
    div []
        [ CDN.stylesheet
        , mainContent model
        ]
