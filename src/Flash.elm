module Flash
    exposing
        ( disburse
        , hitFlash
        , initFlash
        , isBlackjackHand
        , makeStateFromHand
        , playerCanHit
        , PlayerState(Start, Continue, Win, Lose, Tie, Surrender)
        , shuffleDeckFlash
        , standFlash
        , toString
        )

import Score exposing (Score)


type ScoreState
    = Blackjack
    | Under
    | Bust


type PlayerState
    = Start String
    | Continue
    | Win String
    | Lose String
    | Surrender String
    | Tie String


toString : PlayerState -> String
toString playerState =
    case playerState of
        Win string ->
            string

        Lose string ->
            string

        Tie string ->
            string

        Surrender string ->
            string

        Start string ->
            string

        _ ->
            ""


disburse : PlayerState -> Int -> Int -> Int
disburse state pocket wager =
    case state of
        Win _ ->
            pocket + wager

        Lose _ ->
            pocket - wager

        Surrender _ ->
            pocket - (round ((toFloat wager) * 0.5))

        _ ->
            pocket


initFlash : String
initFlash =
    "Welcome To BlackJack!"


standFlash : Score -> Score -> PlayerState
standFlash playerScore dealerScore =
    case makeState dealerScore of
        Blackjack ->
            Lose "❌ - Dealer has 21 - Dealer Wins! - ❌"

        Bust ->
            Win "💰 - Dealer Busts! You Win! 💰"

        Under ->
            maybeDealerWin playerScore dealerScore


maybeDealerWin : Score -> Score -> PlayerState
maybeDealerWin playerScore dealerScore =
    if playerScore.hard < 22 then
        if dealerScore.soft > playerScore.hard then
            Lose "❌ - Dealer has a higher hand - You Lose!"
        else if dealerScore.soft == playerScore.hard then
            Tie "\x1F937\x200D♂️ - Its a tie!"
        else
            Win "💰 - You have a higher hand - You Win!"
    else if dealerScore.soft > playerScore.soft then
        Lose "❌  - Dealer has a higher hand - You Lose!"
    else if dealerScore.soft == playerScore.soft then
        Tie "\x1F937\x200D♂️ - Its a tie!"
    else
        Win "💰 - You have a higher hand - You Win!"


hitFlash : Score -> String -> PlayerState
hitFlash score passthrough =
    case makeState score of
        Blackjack ->
            Win "💰 - 21 - You Win!"

        Under ->
            Continue

        Bust ->
            Lose "❌ - Bust! - You Lose!"


shuffleDeckFlash : List String -> List String -> PlayerState
shuffleDeckFlash playerHand dealerHand =
    case makeStateFromHand playerHand of
        Blackjack ->
            Win "💰 - Blackjack on deal! - You Win!"

        Under ->
            case makeStateFromHand dealerHand of
                Blackjack ->
                    Lose "❌ - Dealer Blackjack on deal! - You Lose!"

                Under ->
                    Start "Welcome To BlackJack!"

                Bust ->
                    Start "\x1F937\x200D♂️ - Dealer Bust on Deal, this should never happen!"

        Bust ->
            Start "\x1F937\x200D♂️ - Player Bust on Deal, this should never happen!"


makeState : Score -> ScoreState
makeState { hard, soft } =
    if soft > 21 then
        Bust
    else if soft == 21 || hard == 21 then
        Blackjack
    else
        Under


playerCanHit : Score -> Bool
playerCanHit score =
    case makeState score of
        Under ->
            True

        _ ->
            False


isBlackjack : ScoreState -> Bool
isBlackjack score =
    case score of
        Blackjack ->
            True

        _ ->
            False


isBlackjackHand : List String -> Bool
isBlackjackHand hand =
    isBlackjack <| makeState <| Score.fromHand hand


makeStateFromHand : List String -> ScoreState
makeStateFromHand playerHand =
    makeState <| Score.fromHand playerHand
