module Flash
    exposing
        ( hitFlash
        , initFlash
        , initState
        , makeState
        , makeStateFromHand
        , ScoreState
        , shuffleDeckFlash
        , standFlash
        )

import Score
    exposing
        ( Score
        , makeScoreFromHand
        , zero
        )


type ScoreState
    = Blackjack
    | Under
    | Bust


initState : ScoreState
initState =
    makeState Score.zero


initFlash : String
initFlash =
    "Welcome To BlackJack!"


standFlash : Score -> Score -> ScoreState -> String
standFlash playerScore dealerScore dealerState =
    case dealerState of
        Blackjack ->
            "Dealer has 21 - Dealer Wins!"

        Bust ->
            "Dealer Busts! You Win!"

        Under ->
            maybeDealerWin playerScore dealerScore


maybeDealerWin : Score -> Score -> String
maybeDealerWin playerScore dealerScore =
    if playerScore.hard < 22 then
        if dealerScore.soft > playerScore.hard then
            "Dealer has a higher hand - Dealer Wins!"
        else
            "You have a higher hand - You Win!"
    else if dealerScore.soft > playerScore.soft then
        "Dealer has a higher hand - Dealer Wins!"
    else if dealerScore.soft == playerScore.soft then
        "Its a tie!"
    else
        "You have a higher hand - You Win!"


hitFlash : Score -> String -> String
hitFlash score passthrough =
    case makeState <| score of
        Blackjack ->
            "21 - You Win!"

        Under ->
            passthrough

        Bust ->
            "Bust! - You Lose!"


shuffleDeckFlash : List String -> List String -> String
shuffleDeckFlash playerHand dealerHand =
    case makeStateFromHand playerHand of
        Blackjack ->
            "Blackjack on deal! - You Win!"

        Under ->
            case makeStateFromHand dealerHand of
                Blackjack ->
                    "Dealer Blackjack on deal! - You Lose!"

                Under ->
                    "Welcome To BlackJack!"

                Bust ->
                    "Dealer Bust on Deal, this should never happen!"

        Bust ->
            "Player Bust on Deal, this should never happen!"


makeState : Score -> ScoreState
makeState { hard, soft } =
    if soft > 21 then
        Bust
    else if soft == 21 || hard == 21 then
        Blackjack
    else
        Under


makeStateFromHand : List String -> ScoreState
makeStateFromHand playerHand =
    makeState <| makeScoreFromHand playerHand
