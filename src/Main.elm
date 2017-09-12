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
import Flash exposing (ScoreState, PlayerState(Start, Continue, Win, Lose, Tie, Surrender))
import BasicStrategy
import DealerStand
import Statistics


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
    , flash : PlayerState
    , history : List Game
    , playerCanHit : Bool
    , playerCanNewGame : Bool
    , playerCanStand : Bool
    , playerCanSurrender : Bool
    , playerHand : List String
    , playerScore : Score
    , playerState : ScoreState
    , playerPocket : Int
    , round : Int
    , wager : Int
    }


type alias Game =
    { dealerHand : List String
    , dealerHand : List String
    , dealerScore : Score
    , playerHand : List String
    , playerScore : Score
    , round : Int
    , winner : PlayerState
    , pocket : Int
    }


gameFromModel : Model -> Game
gameFromModel model =
    { dealerHand = model.dealerHand
    , dealerScore = model.dealerScore
    , playerHand = model.playerHand
    , playerScore = model.playerScore
    , round = model.round
    , winner = model.flash
    , pocket = model.playerPocket
    }


type History
    = List Game


init : ( Model, Cmd Msg )
init =
    ( { dealerHandVisible = False
      , dealerHand = []
      , dealerScore = Score.zero
      , dealerState = Flash.initState
      , deck = []
      , deckVisible = False
      , flash = Start "Welcome to Blackjack - Click NewGame below to start!"
      , history = []
      , playerCanHit = False
      , playerCanNewGame = True
      , playerCanStand = False
      , playerCanSurrender = False
      , playerHand = []
      , playerScore = Score.zero
      , playerState = Flash.initState
      , playerPocket = 1000
      , round = 0
      , wager = 100
      }
    , Cmd.none
    )


type Msg
    = Hit
    | NewGame
    | ShuffleDeck (Array.Array String)
    | Stand
    | Surrender
    | UpdateBet String
    | ToggleShowDealerHand
    | ToggleShowDeck


shuffleDeck : Cmd Msg
shuffleDeck =
    Random.generate ShuffleDeck <|
        Random.Array.shuffle <|
            Array.fromList Card.initDeck


allowedWager : Int -> String -> Int
allowedWager pocket amount =
    let
        wager =
            Result.withDefault 0 <| String.toInt amount
    in
        if wager < pocket then
            wager
        else if pocket < 10 then
            0
        else if wager < 10 then
            10
        else
            pocket


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateBet amount ->
            ( { model
                | wager = allowedWager model.playerPocket amount
              }
            , Cmd.none
            )

        ToggleShowDeck ->
            ( { model
                | deckVisible = not model.deckVisible
              }
            , Cmd.none
            )

        Surrender ->
            ( { model
                | dealerHandVisible = True
                , flash = Flash.Surrender "You Surrendered!"
                , playerCanHit = False
                , playerCanSurrender = False
                , playerCanStand = False
                , playerCanNewGame = True
                , playerPocket = Flash.disburse (Flash.Surrender "You Surrendered!") model.playerPocket model.wager
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
                | dealerHandVisible = False
                , history = (gameFromModel model) :: model.history
                , playerCanHit = True
                , playerCanNewGame = False
                , playerCanSurrender = True
                , wager = allowedWager model.playerPocket (toString model.wager)
              }
            , shuffleDeck
            )

        Stand ->
            let
                ( dealerHand, newDeck ) =
                    DealerStand.dealStand
                        model.dealerScore.soft
                        model.dealerHand
                        model.deck

                dealerScore =
                    Score.fromHand dealerHand

                dealerState =
                    Flash.makeState dealerScore

                flash =
                    Flash.standFlash model.playerScore dealerScore dealerState
            in
                ( { model
                    | dealerHand = dealerHand
                    , dealerHandVisible = True
                    , dealerScore = dealerScore
                    , dealerState = dealerState
                    , deck = newDeck
                    , flash = Flash.standFlash model.playerScore dealerScore dealerState
                    , playerCanHit = False
                    , playerCanStand = False
                    , playerCanSurrender = False
                    , playerCanNewGame = True
                    , playerPocket = Flash.disburse flash model.playerPocket model.wager
                  }
                , Cmd.none
                )

        Hit ->
            let
                ( playerHand, newDeck ) =
                    Card.dealN model.playerHand model.deck 1

                score =
                    Score.fromHand playerHand

                flash =
                    Flash.hitFlash score (Flash.toString model.flash)
            in
                ( { model
                    | deck = newDeck
                    , flash = Flash.hitFlash score (Flash.toString model.flash)
                    , playerCanHit = Flash.playerCanHit score
                    , playerCanNewGame = not <| Flash.playerCanHit score
                    , playerCanStand = Flash.playerCanHit score
                    , playerCanSurrender = Flash.playerCanHit score
                    , playerHand = playerHand
                    , playerScore = score
                    , playerState = Flash.makeState score
                    , playerPocket = Flash.disburse flash model.playerPocket model.wager
                  }
                , Cmd.none
                )

        ShuffleDeck cards ->
            let
                ( newDeck, dealerHand, playerHand ) =
                    Card.dealShuffled (Array.toList cards)

                isBlackJack =
                    (Flash.isBlackjackHand playerHand) || (Flash.isBlackjackHand dealerHand)

                playerCanHit =
                    Flash.playerCanHit

                flash =
                    Flash.shuffleDeckFlash playerHand dealerHand
            in
                ( { model
                    | dealerHand = dealerHand
                    , dealerScore = Score.fromHand dealerHand
                    , dealerState = Flash.makeStateFromHand dealerHand
                    , deck = newDeck
                    , flash = Flash.shuffleDeckFlash playerHand dealerHand
                    , playerCanHit = not isBlackJack
                    , playerCanStand = not isBlackJack
                    , playerCanSurrender = not isBlackJack
                    , playerCanNewGame = isBlackJack
                    , playerHand = playerHand
                    , playerScore = Score.fromHand playerHand
                    , playerState = Flash.makeStateFromHand playerHand
                    , round = model.round + 1
                    , playerPocket = Flash.disburse flash model.playerPocket model.wager
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


zeroFloor x =
    if x /= 0 then
        x - 1
    else
        0


view : Model -> Html Msg
view model =
    div [ attribute "style" "display: flex; min-width: 100%; min-height: 100%; font-family: Palatino;" ]
        [ div [ attribute "style" "flex-grow:1; max-width: 20%; min-width: 20%; background: DARKGREEN; color: white" ]
            [ h1 [ attribute "style" "text-align: center;" ] [ text "Game history" ]
            , p []
                [ text "Total Games: "
                , text <|
                    toString <|
                        zeroFloor <|
                            List.length model.history
                ]
            , p []
                [ text "Wins: "
                , text <| toString <| Statistics.wins model.history
                ]
            , text "Win Percentage: "
            , text <| Statistics.safeWinPercentage <| Statistics.winPercentage model.history
            , if (Statistics.wins model.history) > 0 then
                text "%"
              else
                text ""
            , p [] [ text <| "Peak: " ++ (toString <| showPeak model.history) ]
            , showHistory model.history
            ]
        , div [ attribute "style" "flex-grow:1; min-width: 40%; max-width: 40%; padding-left: 10%;" ]
            [ h1 [] [ text "♠️ ♥️ BlackJack ♣️ ♦️" ]
            , div [] [ text ("Round: " ++ (toString model.round)) ]
            , pre [] [ text <| Flash.toString model.flash ]
            , div [] <|
                if model.playerCanNewGame then
                    [ button [ onClick NewGame ] [ text "Deal" ]
                    , label [] [ text "wager: $ " ]
                    , input [ type_ "text", onInput UpdateBet, attribute "autofocus" "true", value <| toString model.wager ] []
                    , label [] [ text " (min $10) " ]
                    ]
                else
                    [ button [ onClick NewGame, attribute "disabled" "true" ] [ text "Deal" ]
                    , label [] [ text "wager: $ " ]
                    , input [ type_ "text", attribute "disabled" "true", value <| toString model.wager ] []
                    , label [] [ text " (min $10) " ]
                    ]
            , div [ attribute "style" "margin-top: 20px" ]
                [ if model.playerCanHit then
                    button [ onClick Hit, attribute "style" "background:lime; color:black;" ] [ text "HIT" ]
                  else
                    button [ onClick Hit, attribute "disabled" "true" ] [ text "HIT" ]
                , if model.playerCanStand then
                    button [ onClick Stand, attribute "style" "background:red; color:white" ] [ text "STAND" ]
                  else
                    button [ onClick Stand, attribute "disabled" "true" ] [ text "STAND" ]
                , if model.playerCanSurrender then
                    button [ onClick Surrender, attribute "style" "background:white; color:black" ] [ text "SURRENDER" ]
                  else
                    button [ onClick Surrender, attribute "disabled" "true" ] [ text "SURRENDER" ]
                ]
            , h2 [] [ text "Player" ]
            , div [ attribute "style" "font-size: 102px;" ] [ text <| "💲" ++ (toString model.playerPocket) ]
            , div [ attribute "style" "font-size: 102px;" ] <|
                List.map text <|
                    Card.showPlayerHand model.playerHand
            , div [] [ text (toString model.playerScore) ]
            , h2 [] [ text "Dealer" ]
            , button [ onClick ToggleShowDealerHand ] [ text "👀" ]
            , div [ attribute "style" "font-size: 102px;" ] <|
                if model.dealerHandVisible then
                    List.map text <|
                        Card.showPlayerHand model.dealerHand
                else
                    [ text <|
                        Card.showDealerHand model.dealerHand
                    ]
            , div []
                [ text <|
                    toString <|
                        if model.dealerHandVisible then
                            toString model.dealerScore
                        else
                            ""
                ]
              --, h2 [] [ text "Deck" ]
              --, button [ onClick ToggleShowDeck ] [ text "🔎" ]
              --, div []
              --    [ text <|
              --        toString <|
              --            if model.deckVisible then
              --                model.deck
              --            else
              --                []
              --   ]
            ]
        , div [ attribute "style" " flex-grow:1; background: OLIVE; color: white; padding-left: 20px;" ]
            [ h1 []
                [ text "Basic Strategy" ]
            , BasicStrategy.legend
            , BasicStrategy.basicStrategy model.playerHand model.dealerHand
            ]
        ]


rejectStart : List Game -> List Game
rejectStart history =
    let
        f x =
            case x.winner of
                Start _ ->
                    False

                _ ->
                    True
    in
        List.filter f history


showPeak : List Game -> Int
showPeak history =
    Maybe.withDefault 0 <| List.maximum <| List.map (\x -> x.pocket) history


showHistory : List Game -> Html msg
showHistory history =
    ol [ attribute "reversed" "true" ] <|
        List.map (\x -> li [] [ text x ]) <|
            List.map Flash.toString <|
                List.map (\x -> x.winner) <|
                    rejectStart history
