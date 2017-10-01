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
import Flash exposing (PlayerState(Start, Continue, Win, Lose, Tie, Surrender))
import BasicStrategy
import BasicStrategyView
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
    { basicStrategyVisible : Bool
    , dealerHand : List String
    , cheating : Bool
    , dealerScore : Score
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
    , winPercentage : String
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
    , winPercentage = Statistics.safeWinPercentage <| Statistics.winPercentage model.history
    , pocket = model.playerPocket
    }


type History
    = List Game


init : ( Model, Cmd Msg )
init =
    ( { basicStrategyVisible = False
      , cheating = False
      , dealerHand = []
      , dealerScore = Score.zero
      , deck = []
      , deckVisible = False
      , flash = Start "Welcome to Blackjack - Place a bet and Click Deal below to start!"
      , history = []
      , playerCanHit = False
      , playerCanNewGame = True
      , playerCanStand = False
      , playerCanSurrender = False
      , playerHand = []
      , playerScore = Score.zero
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
    | Cheat
    | ToggleShowDeck
    | ToggleBasicStrategy


shuffleDeck : Cmd Msg
shuffleDeck =
    Random.generate ShuffleDeck <|
        Random.Array.shuffle <|
            Array.fromList Card.initDeck


allowedWager : Int -> String -> Int
allowedWager pocket amount =
    let
        wager =
            Result.withDefault 1 <| String.toInt amount
    in
        if pocket < 10 then
            0
        else if wager < pocket then
            wager
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

        ToggleBasicStrategy ->
            ( { model
                | basicStrategyVisible = not model.basicStrategyVisible
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
                | cheating = True
                , flash = Flash.Surrender "You Surrendered!"
                , playerCanHit = False
                , playerCanSurrender = False
                , playerCanStand = False
                , playerCanNewGame = True
                , playerPocket = Flash.disburse (Flash.Surrender "You Surrendered!") model.playerPocket model.wager
              }
            , Cmd.none
            )

        Cheat ->
            ( { model
                | cheating = True
              }
            , Cmd.none
            )

        NewGame ->
            ( { model
                | cheating = False
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

                flash =
                    Flash.standFlash model.playerScore dealerScore
            in
                ( { model
                    | dealerHand = dealerHand
                    , cheating = True
                    , dealerScore = dealerScore
                    , deck = newDeck
                    , flash = flash
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
                    , deck = newDeck
                    , flash = Flash.shuffleDeckFlash playerHand dealerHand
                    , playerCanHit = not isBlackJack
                    , playerCanStand = not isBlackJack
                    , playerCanSurrender = not isBlackJack
                    , playerCanNewGame = isBlackJack
                    , playerHand = playerHand
                    , playerScore = Score.fromHand playerHand
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


totalGames : List a -> String
totalGames history =
    let
        zeroFloor : Int -> Int
        zeroFloor x =
            if x /= 0 then
                x - 1
            else
                0
    in
        List.length history |> zeroFloor |> toString


view : Model -> Html Msg
view model =
    div [ attribute "class" "fullscreen" ]
        [ div [ attribute "class" "history-column" ]
            [ h1 [] [ text "Game history" ]
            , p []
                [ text "Total Games: "
                , text <| totalGames model.history
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
        , div [ attribute "class" "play-table-column" ]
            [ h1 [] [ text "♠️ ♥️ BlackJack ♣️ ♦️" ]
            , div [ attribute "class" "player-pocket" ] [ text <| "$" ++ (toString model.playerPocket) ]
            , div [ attribute "class" "flash" ] [ text <| Flash.toString model.flash ]
            , div [ attribute "class" "new-game" ] <|
                if model.playerCanNewGame then
                    [ button [ onClick NewGame ] [ text "Deal" ]
                    , label [] [ text "Wager: $ " ]
                    , input [ type_ "text", onInput UpdateBet, attribute "autofocus" "true", value <| toString model.wager ] []
                    ]
                else
                    [ button [ onClick NewGame, attribute "disabled" "true" ] [ text "Deal" ]
                    , label [] [ text "wager: $ " ]
                    , input [ type_ "text", attribute "disabled" "true", value <| toString model.wager ] []
                    ]
            , div [ attribute "class" "action-buttons" ]
                [ if model.playerCanStand then
                    button [ onClick Stand, attribute "class" "stand-button" ] [ text "STAND" ]
                  else
                    button [ onClick Stand, attribute "class" "stand-button", attribute "disabled" "true" ] [ text "STAND" ]
                , if model.playerCanHit then
                    button [ onClick Hit, attribute "class" "hit-button" ] [ text "HIT" ]
                  else
                    button [ onClick Hit, attribute "class" "hit-button", attribute "disabled" "true" ] [ text "HIT" ]
                , if model.playerCanSurrender then
                    button [ onClick Surrender, attribute "class" "surrender-button" ] [ text "SURRENDER" ]
                  else
                    button [ onClick Surrender, attribute "class" "surrender-button", attribute "disabled" "true" ] [ text "SURRENDER" ]
                ]
            , div [ attribute "class" "show-cards" ]
                [ div [ attribute "style" "font-size: 102px;" ] <|
                    List.map text <|
                        Card.showPlayerHand model.playerHand
                , h2 [] [ text "Dealer" ]
                , button [ onClick Cheat ] [ text "cheat" ]
                , div [ attribute "style" "font-size: 102px;" ] <|
                    if model.cheating then
                        List.map text <|
                            Card.showPlayerHand model.dealerHand
                    else
                        [ text <|
                            Card.showDealerHand model.dealerHand
                        ]
                ]
            ]
        , div [ attribute "class" "strategy-column" ]
            [ h1 []
                [ text "Basic Strategy" ]
            , button [ onClick ToggleBasicStrategy ] [ text "show/hide" ]
            , if model.basicStrategyVisible then
                div []
                    [ div []
                        [ text <|
                            basicTactic model.playerHand model.dealerHand
                        ]
                    , BasicStrategyView.legend
                    , BasicStrategyView.basicStrategy model.playerHand model.dealerHand
                    ]
              else
                div [] []
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
    Maybe.withDefault 0 <| List.maximum <| List.map .pocket history


basicTactic : List String -> List String -> String
basicTactic playerHand dealerHand =
    let
        hardScore =
            case dealerHand of
                [ n, last ] ->
                    (Score.fromHand [ last ]).hard

                _ ->
                    0

        result =
            BasicStrategy.getHardStrategy
                (Score.fromHand playerHand).hard
                hardScore
    in
        case result of
            Just string ->
                toString string

            Nothing ->
                ""


showHistory : List Game -> Html msg
showHistory history =
    ol [ attribute "reversed" "true" ] <|
        List.map (\x -> li [] [ text ((Flash.toString x.winner) ++ "  " ++ (x.winPercentage) ++ "%") ]) <|
            rejectStart history
