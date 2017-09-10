--BlackJack


module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Array
import Random
import Random.Array
import Card
import Score exposing (Score)
import Flash exposing (ScoreState)
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
    { dealerHand : List String
    , dealerHandVisible : Bool
    , dealerScore : Score
    , dealerState : ScoreState
    , deck : List String
    , deckVisible : Bool
    , flash : String
    , history : List Game
    , playerCanHit : Bool
    , playerHand : List String
    , playerScore : Score
    , playerState : ScoreState
    , round : Int
    }


type alias Game =
    { dealerHand : List String
    , dealerHand : List String
    , dealerScore : Score
    , playerHand : List String
    , playerScore : Score
    , round : Int
    , winner : String
    }


gameFromModel : Model -> Game
gameFromModel model =
    { dealerHand = model.dealerHand
    , dealerScore = model.dealerScore
    , playerHand = model.playerHand
    , playerScore = model.playerScore
    , round = model.round
    , winner = model.flash
    }


type History
    = List Game


init : ( Model, Cmd Msg )
init =
    let
        initScore =
            Score 0 0

        model =
            { dealerHandVisible = False
            , dealerHand = []
            , dealerScore = Score.zero
            , dealerState = Flash.initState
            , deck = []
            , deckVisible = False
            , flash = "Welcome To BlackJack!"
            , history = []
            , playerCanHit = True
            , playerHand = []
            , playerScore = Score.zero
            , playerState = Flash.initState
            , round = 0
            }
    in
        ( model, shuffleDeck )


type Msg
    = Hit
    | NewGame
    | ShuffleDeck (Array.Array String)
    | Stand
    | Surrender
    | ToggleShowDealerHand
    | ToggleShowDeck


shuffleDeck : Cmd Msg
shuffleDeck =
    Random.generate ShuffleDeck <|
        Random.Array.shuffle <|
            Array.fromList Card.initDeck


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
                | dealerHandVisible = True
                , flash = "You Surrendered!"
                , playerCanHit = False
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
                    Flash.makeState dealerScore
            in
                ( { model
                    | dealerHand = dealerHand
                    , dealerHandVisible = True
                    , dealerScore = dealerScore
                    , dealerState = dealerState
                    , deck = newDeck
                    , flash = Flash.standFlash model.playerScore dealerScore dealerState
                    , playerCanHit = False
                  }
                , Cmd.none
                )

        Hit ->
            let
                ( playerHand, newDeck ) =
                    Card.dealNCards model.playerHand model.deck 1

                score =
                    Score.makeScoreFromHand playerHand
            in
                ( { model
                    | deck = newDeck
                    , flash = Flash.hitFlash score model.flash
                    , playerHand = playerHand
                    , playerScore = score
                    , playerState = Flash.makeState score
                  }
                , Cmd.none
                )

        ShuffleDeck cards ->
            let
                ( newDeck, dealerHand, playerHand ) =
                    Card.dealShuffled (Array.toList cards)
            in
                ( { model
                    | dealerHand = dealerHand
                    , dealerScore = Score.makeScoreFromHand dealerHand
                    , dealerState = Flash.makeStateFromHand dealerHand
                    , deck = newDeck
                    , flash = Flash.shuffleDeckFlash playerHand dealerHand
                    , playerHand = playerHand
                    , playerScore = Score.makeScoreFromHand playerHand
                    , playerState = Flash.makeStateFromHand playerHand
                    , round = model.round + 1
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
