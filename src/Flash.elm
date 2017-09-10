module Flash
    exposing
        ( hitFlash
        , initFlash
        , initState
        , makeState
        , makeStateFromHand
        , playerCanHit
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
    = Blackjack String
    | Under String
    | Bust String


initState : ScoreState
initState =
    makeState Score.zero


initFlash : String
initFlash =
    "Welcome To BlackJack!"


standFlash : Score -> Score -> ScoreState -> String
standFlash playerScore dealerScore dealerState =
    case dealerState of
        Blackjack _ ->
            "Dealer has 21 - Dealer Wins!"

        Bust _ ->
            "Dealer Busts! You Win!"

        Under _ ->
            maybeDealerWin playerScore dealerScore


maybeDealerWin : Score -> Score -> String
maybeDealerWin playerScore dealerScore =
    if playerScore.hard < 22 then
        if dealerScore.soft > playerScore.hard then
            "Dealer has a higher hand - Dealer Wins!"
        else if dealerScore.soft == playerScore.hard then
            "Its a tie!"
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
        Blackjack _ ->
            "21 - You Win!"

        Under _ ->
            passthrough

        Bust _ ->
            "Bust! - You Lose!"


shuffleDeckFlash : List String -> List String -> String
shuffleDeckFlash playerHand dealerHand =
    case makeStateFromHand playerHand of
        Blackjack _ ->
            "Blackjack on deal! - You Win!"

        Under _ ->
            case makeStateFromHand dealerHand of
                Blackjack _ ->
                    "Dealer Blackjack on deal! - You Lose!"

                Under _ ->
                    "Welcome To BlackJack!"

                Bust _ ->
                    "Dealer Bust on Deal, this should never happen!"

        Bust _ ->
            "Player Bust on Deal, this should never happen!"


makeState : Score -> ScoreState
makeState { hard, soft } =
    if soft > 21 then
        Bust ""
    else if soft == 21 || hard == 21 then
        Blackjack ""
    else
        Under ""


playerCanHit : Score -> Bool
playerCanHit score =
    case makeState score of
        Under _ ->
            True

        _ ->
            False


makeStateFromHand : List String -> ScoreState
makeStateFromHand playerHand =
    makeState <| makeScoreFromHand playerHand
