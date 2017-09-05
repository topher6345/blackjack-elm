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
    , playerState : ScoreState
    , dealerHand : List String
    , dealerScore : Score
    , dealerState : ScoreState
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
            , playerState = makeState (Score 0 0)
            , dealerHand = []
            , dealerScore = Score 0 0
            , dealerState = makeState (Score 0 0)
            , flash = "Welcome To BlackJack!"
            }
    in
        ( model, shuffleDeck )



-- UPDATE


type Msg
    = NewGame
    | Hit
    | Stand
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

        Stand ->
            let
                cards =
                    Array.fromList model.deck

                dealerHand =
                    if model.dealerScore.soft < 18 then
                        model.dealerHand ++ (Array.toList <| Array.slice 0 1 cards)
                    else
                        model.dealerHand

                newDeck =
                    Array.toList <| Array.slice 2 -1 cards

                score =
                    scoreHand dealerHand

                dealerState =
                    makeState score

                flash =
                    case dealerState of
                        Blackjack ->
                            "Dealer Wins!"

                        Bust ->
                            "You Win!"

                        Under ->
                            if score.soft > model.playerScore.hard then
                                "Dealer Wins!"
                            else
                                model.flash
            in
                ( { model
                    | dealerHand = dealerHand
                    , deck = newDeck
                    , dealerScore = score
                    , dealerState = dealerState
                    , flash = flash
                  }
                , Cmd.none
                )

        Hit ->
            let
                cards =
                    Array.fromList model.deck

                playerHand =
                    model.playerHand ++ (Array.toList <| Array.slice 0 1 cards)

                newDeck =
                    Array.toList <| Array.slice 2 -1 cards

                score =
                    scoreHand playerHand

                flash =
                    case makeState <| scoreHand playerHand of
                        Blackjack ->
                            "You Win!"

                        Under ->
                            model.flash

                        Bust ->
                            "Bust!"
            in
                ( { model
                    | playerHand = playerHand
                    , deck = newDeck
                    , playerScore = score
                    , flash = flash
                    , playerState = makeState score
                  }
                , Cmd.none
                )

        ShuffleDeck cards ->
            let
                sliceToList x y =
                    Array.toList <| Array.slice x y cards

                playerHand =
                    sliceToList 0 2

                dealerHand =
                    sliceToList 3 5

                newDeck =
                    sliceToList 5 -1

                newRound =
                    model.round + 1

                flash =
                    case makeState <| scoreHand playerHand of
                        Blackjack ->
                            "You Win!"

                        Under ->
                            "Welcome To BlackJack!"

                        Bust ->
                            "Bust!"
            in
                ( { model
                    | deck = newDeck
                    , round = newRound
                    , dealerHand = dealerHand
                    , playerHand = playerHand
                    , playerScore = scoreHand playerHand
                    , playerState = makeState <| scoreHand playerHand
                    , dealerScore = scoreHand dealerHand
                    , dealerState = makeState <| scoreHand dealerHand
                    , flash = flash
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
        , div [] [ text (toString model.playerState) ]
        , h2 [] [ text "Dealer" ]
        , div [] [ text (toString <| List.tail model.dealerHand) ]
        , div [] [ text (toString model.dealerHand) ]
        , div [] [ text (toString model.dealerScore) ]
        , div [] [ text (toString model.dealerState) ]
        , button [ onClick NewGame ] [ text "New Game" ]
        , button [ onClick Hit ] [ text "Hit" ]
        , button [ onClick Stand ] [ text "Stand" ]
        , button [ onClick NewGame ] [ text "Surrender" ]
        ]
