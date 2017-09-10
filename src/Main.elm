--BlackJack


module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Array
import Random
import Random.Array
import Card
import Score exposing (Score, ScoreState)
import BasicStrategy


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
    , playerCanHit : Bool
    , dealerHand : List String
    , dealerScore : Score
    , dealerState : ScoreState
    , deckVisible : Bool
    , dealerHandVisible : Bool
    , flash : String
    , history : List Game
    }


type alias Game =
    { round : Int
    , winner : String
    , playerHand : List String
    , playerScore : Score
    , dealerHand : List String
    , dealerScore : Score
    }


gameFromModel : Model -> Game
gameFromModel model =
    { round = model.round
    , winner = model.flash
    , playerHand = model.playerHand
    , playerScore = model.playerScore
    , dealerHand = model.dealerHand
    , dealerScore = model.dealerScore
    }


type History
    = List Game


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
            , playerState = Score.makeState (Score 0 0)
            , playerCanHit = True
            , dealerHand = []
            , dealerScore = Score 0 0
            , dealerState = Score.makeState (Score 0 0)
            , deckVisible = False
            , dealerHandVisible = False
            , flash = "Welcome To BlackJack!"
            , history = []
            }
    in
        ( model, shuffleDeck )


type Msg
    = NewGame
    | Hit
    | Stand
    | ToggleShowDeck
    | ToggleShowDealerHand
    | Surrender
    | ShuffleDeck (Array.Array String)


shuffleDeck : Cmd Msg
shuffleDeck =
    Random.generate ShuffleDeck
        (Random.Array.shuffle (Array.fromList Card.initDeck))


maybeDealerWin playerScore dealerScore =
    if playerScore.hard < 22 then
        if dealerScore.soft > playerScore.hard then
            "Dealer Wins!"
        else
            "You Win!"
    else if dealerScore.soft > playerScore.soft then
        "Dealer Wins!"
    else if dealerScore.soft == playerScore.soft then
        "Its a tie!"
    else
        "You Win!"


standFlash playerScore dealerScore dealerState =
    case dealerState of
        Score.Blackjack ->
            "Dealer Wins!"

        Score.Bust ->
            "You Win!"

        Score.Under ->
            maybeDealerWin playerScore dealerScore


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
        ToggleShowDeck ->
            let
                current =
                    model.deckVisible
            in
                ( { model
                    | deckVisible = not current
                  }
                , Cmd.none
                )

        Surrender ->
            ( { model
                | flash = "You lose!"
                , playerCanHit = False
                , dealerHandVisible = True
              }
            , Cmd.none
            )

        ToggleShowDealerHand ->
            let
                current =
                    model.dealerHandVisible
            in
                ( { model
                    | dealerHandVisible = not current
                  }
                , Cmd.none
                )

        NewGame ->
            let
                game =
                    gameFromModel model

                games =
                    model.history
            in
                ( { model
                    | history = game :: games
                    , playerCanHit = True
                  }
                , shuffleDeck
                )

        Stand ->
            let
                ( dealerHand, newDeck ) =
                    if model.dealerScore.soft < 18 then
                        dealNCards model.dealerHand model.deck (Score.dealerStandUnder (model.dealerHand ++ model.deck) - 2)
                    else
                        ( model.dealerHand, model.deck )

                dealerScore =
                    Score.makeScoreFromHand dealerHand

                dealerState =
                    Score.makeState dealerScore

                flash =
                    standFlash model.playerScore dealerScore dealerState
            in
                ( { model
                    | dealerHand = dealerHand
                    , deck = newDeck
                    , dealerScore = dealerScore
                    , dealerState = dealerState
                    , playerCanHit = False
                    , dealerHandVisible = True
                    , flash = flash
                  }
                , Cmd.none
                )

        Hit ->
            let
                ( playerHand, newDeck ) =
                    dealNCards model.dealerHand model.deck 1

                score =
                    Score.makeScoreFromHand playerHand

                flash =
                    case Score.makeState score of
                        Score.Blackjack ->
                            "You Win!"

                        Score.Under ->
                            model.flash

                        Score.Bust ->
                            "Bust!"
            in
                ( { model
                    | playerHand = playerHand
                    , deck = newDeck
                    , playerScore = score
                    , flash = flash
                    , playerState = Score.makeState score
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
                    case Score.makeState <| Score.makeScoreFromHand playerHand of
                        Score.Blackjack ->
                            "Blackjack on deal! - You Win!"

                        Score.Under ->
                            "Welcome To BlackJack!"

                        Score.Bust ->
                            "Bust on Deal, this should never happen!"
            in
                ( { model
                    | deck = d3
                    , round = newRound
                    , dealerHand = dealerHand
                    , playerHand = playerHand
                    , playerScore = Score.makeScoreFromHand playerHand
                    , playerState = Score.makeState <| Score.makeScoreFromHand playerHand
                    , dealerScore = Score.makeScoreFromHand dealerHand
                    , dealerState = Score.makeState <| Score.makeScoreFromHand dealerHand
                    , flash = flash
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


showDeck : Model -> List String
showDeck model =
    if model.deckVisible then
        model.deck
    else
        []


showDealerHand : Model -> List String
showDealerHand model =
    if model.dealerHandVisible then
        model.dealerHand
    else
        []


showDealerScore : Model -> String
showDealerScore model =
    if model.dealerHandVisible then
        toString model.dealerScore
    else
        ""


view : Model -> Html Msg
view model =
    div [ attribute "style" "display: flex;" ]
        [ div [ attribute "style" "flex-grow:1;max-width: 33%; " ]
            [ h1 [] [ text "🂠BlackJack🂠" ]
            , h1 [] [ text "♠️♣️♥️♦️" ]
            , div [] [ text (toString model.flash) ]
            , div [] [ button [ onClick NewGame ] [ text "New Game" ] ]
            , div []
                [ if model.playerCanHit then
                    button [ onClick Hit ] [ text "Hit" ]
                  else
                    div [] []
                ]
            , button [ onClick Stand ] [ text "Stand" ]
            , button [ onClick Surrender ] [ text "Surrender" ]
            , div [] [ text ("Round: " ++ (toString model.round)) ]
            , h2 [] [ text "Player" ]
            , div [ attribute "style" "font-size: 102px;" ] <| List.map (\x -> text x) (List.map Card.cardStringToGlyph model.playerHand)
            , div [] [ text (toString model.playerScore) ]
            , h2 [] [ text "Dealer" ]
            , div [ attribute "style" "font-size: 102px;" ] [ text ("🂠" ++ (Card.cardStringToGlyph <| Maybe.withDefault "" <| List.head <| Maybe.withDefault [] <| List.tail model.dealerHand)) ]
            , button [ onClick ToggleShowDealerHand ] [ text "🔎" ]
            , div [] [ text (toString <| showDealerHand model) ]
            , div [] [ text (toString <| showDealerScore model) ]
            , h2 [] [ text "Deck" ]
            , button [ onClick ToggleShowDeck ] [ text "🔎" ]
            , div []
                [ text (toString <| showDeck model) ]
              --, ol []
              --    [ text (toString model.history) ]
            ]
        , div [ attribute "style" " flex-grow:1 " ]
            [ BasicStrategy.legend ]
        , div [ attribute "style" " flex-grow:1 " ]
            [ BasicStrategy.basicStrategy model ]
        ]
