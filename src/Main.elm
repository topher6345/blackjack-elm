--BlackJack


module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Array
import Random
import Random.Array
import Card exposing (..)
import Score exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { round : Int
    , deck : List String
    , playerHand : List String
    , playerScore : Score
    , dealerHand : List String
    , dealerScore : Score
    , flash : String
    }


init : ( Model, Cmd Msg )
init =
    let
        initScore =
            Score 0 0

        model =
            { round = 0
            , deck = []
            , playerHand = []
            , playerScore = Score 0 0
            , dealerHand = []
            , dealerScore = Score 0 0
            , flash = "Welcome To BlackJack!"
            }
    in
        ( model, shuffleDeck )



-- UPDATE


type Msg
    = NewGame
    | Hit
    | ShuffleDeck (Array.Array String)


shuffleDeck : Cmd Msg
shuffleDeck =
    Random.generate ShuffleDeck
        (Random.Array.shuffle (Array.fromList Card.initDeck))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, shuffleDeck )

        Hit ->
            let
                cards =
                    Array.fromList model.deck

                playerHand =
                    model.playerHand ++ (Array.toList <| Array.slice 0 1 cards)

                newDeck =
                    Array.toList <| Array.slice 2 -1 cards
            in
                ( { model
                    | playerHand = playerHand
                    , deck = newDeck
                    , playerScore = scoreHand playerHand
                  }
                , Cmd.none
                )

        ShuffleDeck cards ->
            let
                playerHand =
                    Array.toList <| Array.slice 0 2 cards

                dealerHand =
                    Array.toList <| Array.slice 3 5 cards

                newDeck =
                    Array.toList <| Array.slice 5 -1 cards

                newRound =
                    model.round + 1
            in
                ( { model
                    | deck = newDeck
                    , round = newRound
                    , dealerHand = dealerHand
                    , playerHand = playerHand
                    , playerScore = scoreHand playerHand
                    , dealerScore = scoreHand dealerHand
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "BlackJack" ]
        , div [] [ text (toString model.flash) ]
        , div [] [ text (toString model.round) ]
        , h2 [] [ text "Player" ]
        , div [] [ text (toString model.playerHand) ]
        , div [] [ text (toString model.playerScore) ]
        , h2 [] [ text "Dealer" ]
        , div [] [ text (toString model.dealerHand) ]
        , div [] [ text (toString model.dealerScore) ]
        , button [ onClick NewGame ] [ text "New Game" ]
        , button [ onClick Hit ] [ text "Hit" ]
        , button [ onClick NewGame ] [ text "Stand" ]
        , button [ onClick NewGame ] [ text "Surrender" ]
        ]
