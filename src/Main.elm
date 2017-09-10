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
            , playerScore = Score.zero
            , playerState = Score.initState
            , playerCanHit = True
            , dealerHand = []
            , dealerScore = Score.zero
            , dealerState = Score.initState
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleShowDeck ->
            ( { model
                | deckVisible = not model.deckVisible
              }
            , Cmd.none
            )

        Surrender ->
            ( { model
                | flash = "You Surrendered!"
                , playerCanHit = False
                , dealerHandVisible = True
              }
            , Cmd.none
            )

        ToggleShowDealerHand ->
            ( { model
                | dealerHandVisible = not model.dealerHandVisible
              }
            , Cmd.none
            )

        NewGame ->
            ( { model
                | history = (gameFromModel model) :: model.history
                , playerCanHit = True
              }
            , shuffleDeck
            )

        Stand ->
            let
                ( dealerHand, newDeck ) =
                    Score.dealStand
                        model.dealerScore.soft
                        model.dealerHand
                        model.deck

                dealerScore =
                    Score.makeScoreFromHand dealerHand

                dealerState =
                    Score.makeState dealerScore
            in
                ( { model
                    | dealerHand = dealerHand
                    , deck = newDeck
                    , dealerScore = dealerScore
                    , dealerState = dealerState
                    , playerCanHit = False
                    , dealerHandVisible = True
                    , flash =
                        Score.standFlash model.playerScore dealerScore dealerState
                  }
                , Cmd.none
                )

        Hit ->
            let
                ( playerHand, newDeck ) =
                    Card.dealNCards model.dealerHand model.deck 1

                score =
                    Score.makeScoreFromHand playerHand
            in
                ( { model
                    | playerHand = playerHand
                    , deck = newDeck
                    , playerScore = score
                    , flash = Score.hitFlash score model.flash
                    , playerState = Score.makeState score
                  }
                , Cmd.none
                )

        ShuffleDeck cards ->
            let
                ( newDeck, dealerHand, playerHand ) =
                    Card.dealShuffled (Array.toList cards)
            in
                ( { model
                    | deck = newDeck
                    , round = model.round + 1
                    , dealerHand = dealerHand
                    , playerHand = playerHand
                    , playerScore = Score.makeScoreFromHand playerHand
                    , playerState = Score.makeStateFromHand playerHand
                    , dealerScore = Score.makeScoreFromHand dealerHand
                    , dealerState = Score.makeStateFromHand dealerHand
                    , flash = Score.shuffleDeckFlash playerHand dealerHand
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
    div [ attribute "style" "display: flex;" ]
        [ div [ attribute "style" "flex-grow:1;max-width: 33%; " ]
            [ h1 [] [ text "🂠BlackJack🂠" ]
            , h1 [] [ text "♠️♣️♥️♦️" ]
            , div [] [ text (toString model.flash) ]
            , div [] [ text ("Round: " ++ (toString model.round)) ]
            , div [] [ button [ onClick NewGame ] [ text "New Game" ] ]
            , div [ attribute "style" "margin-top: 20px" ]
                [ if model.playerCanHit then
                    button [ onClick Hit ] [ text "Hit" ]
                  else
                    button [ onClick Hit, attribute "disabled" "true" ] [ text "Hit" ]
                , button [ onClick Stand ] [ text "Stand" ]
                , button [ onClick Surrender ] [ text "Surrender" ]
                ]
            , h2 [] [ text "Player" ]
            , div [ attribute "style" "font-size: 102px;" ] <|
                List.map text <|
                    Card.playerCardStringText model.playerHand
            , div [] [ text (toString model.playerScore) ]
            , h2 [] [ text "Dealer" ]
            , button [ onClick ToggleShowDealerHand ] [ text "🔎" ]
            , div [ attribute "style" "font-size: 102px;" ] <|
                if model.dealerHandVisible then
                    List.map text <|
                        Card.playerCardStringText model.dealerHand
                else
                    [ text <|
                        Card.dealerCardStringText model.dealerHand
                    ]
            , div []
                [ text <|
                    toString <|
                        if model.dealerHandVisible then
                            toString model.dealerScore
                        else
                            ""
                ]
            , h2 [] [ text "Deck" ]
            , button [ onClick ToggleShowDeck ] [ text "🔎" ]
            , div []
                [ text <|
                    toString <|
                        if model.deckVisible then
                            model.deck
                        else
                            []
                ]
              --, ol []
              --    [ text (toString model.history) ]
            ]
        , div [ attribute "style" " flex-grow:1 " ]
            [ BasicStrategy.legend ]
        , div [ attribute "style" " flex-grow:1 " ]
            [ BasicStrategy.basicStrategy model ]
        ]
