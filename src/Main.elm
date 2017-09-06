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


type Msg
    = NewGame
    | Hit
    | Stand
    | ShuffleDeck (Array.Array String)


shuffleDeck : Cmd Msg
shuffleDeck =
    Random.generate ShuffleDeck
        (Random.Array.shuffle (Array.fromList Card.initDeck))


maybeDealerWin :
    { b | flash : String, playerScore : { a | hard : comparable } }
    -> { c | soft : comparable }
    -> String
maybeDealerWin model score =
    if score.soft > model.playerScore.hard then
        "Dealer Wins!"
    else
        model.flash


standFlash :
    { b | flash : String, playerScore : { a | hard : comparable } }
    -> { c | soft : comparable }
    -> ScoreState
    -> String
standFlash model score dealerState =
    case dealerState of
        Blackjack ->
            "Dealer Wins!"

        Bust ->
            "You Win!"

        Under ->
            maybeDealerWin model score


dealNCards : List a -> List a -> Int -> ( List a, List a )
dealNCards to from n =
    let
        c1 =
            List.take n from

        c2 =
            List.drop n from
    in
        ( to ++ c1, c2 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, shuffleDeck )

        Stand ->
            let
                ( dealerHand, newDeck ) =
                    if model.dealerScore.soft < 18 then
                        dealNCards model.dealerHand model.deck 1
                    else
                        ( model.dealerHand, model.deck )

                dealerScore =
                    makeScoreFromHand dealerHand

                dealerState =
                    makeState dealerScore

                flash =
                    standFlash model dealerScore dealerState
            in
                ( { model
                    | dealerHand = dealerHand
                    , deck = newDeck
                    , dealerScore = dealerScore
                    , dealerState = dealerState
                    , flash = flash
                  }
                , Cmd.none
                )

        Hit ->
            let
                ( playerHand, newDeck ) =
                    dealNCards model.dealerHand model.deck 1

                score =
                    makeScoreFromHand playerHand

                flash =
                    case makeState score of
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
                d1 =
                    Array.toList cards

                ( playerHand, d2 ) =
                    dealNCards [] d1 2

                ( dealerHand, d3 ) =
                    dealNCards [] d2 2

                newRound =
                    model.round + 1

                flash =
                    case makeState <| makeScoreFromHand playerHand of
                        Blackjack ->
                            "You Win!"

                        Under ->
                            "Welcome To BlackJack!"

                        Bust ->
                            "Bust!"
            in
                ( { model
                    | deck = d3
                    , round = newRound
                    , dealerHand = dealerHand
                    , playerHand = playerHand
                    , playerScore = makeScoreFromHand playerHand
                    , playerState = makeState <| makeScoreFromHand playerHand
                    , dealerScore = makeScoreFromHand dealerHand
                    , dealerState = makeState <| makeScoreFromHand dealerHand
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
