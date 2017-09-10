module Score exposing (..)

import Card


type alias Score =
    { soft : Int, hard : Int }


type ScoreState
    = Blackjack
    | Under
    | Bust


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


scoreFace : String -> number
scoreFace card =
    case card of
        "Two" ->
            2

        "Three" ->
            3

        "Four" ->
            4

        "Five" ->
            5

        "Six" ->
            6

        "Seven" ->
            7

        "Eight" ->
            8

        "Nine" ->
            9

        "Ten" ->
            10

        "Jack" ->
            10

        "Queen" ->
            10

        "King" ->
            10

        _ ->
            0


makeScoreCardTuple : String -> ( String, Score )
makeScoreCardTuple c =
    ( c, makeScore c )


scoresUnderBust : List String -> List { hard : Int, soft : Int }
scoresUnderBust deck =
    List.filter (\x -> x.soft < 22) <| scanlScores <| List.map makeScore deck


dealerStandUnder : List String -> Int
dealerStandUnder deck =
    List.length <| scoresUnderBust deck


cardsUnderBust : List String -> List String
cardsUnderBust deck =
    let
        length =
            List.length <| scoresUnderBust deck
    in
        List.take length deck



--softScoreFromHand :
--foldl1 : (String -> String -> String) -> List String -> String


scanlScores : List Score -> List Score
scanlScores list =
    let
        head =
            List.head list

        tail =
            List.tail list

        score =
            { soft = 0, hard = 0 }

        f x y =
            { soft = x.soft + y.soft, hard = x.hard + y.hard }
    in
        List.scanl f (Maybe.withDefault score head) (Maybe.withDefault [] tail)


softScoreFromHand : List String -> Int
softScoreFromHand x =
    (makeScoreFromHand x).soft


makeScore : String -> Score
makeScore string =
    let
        face =
            Card.extractFace string

        hard =
            scoreHard face

        soft =
            scoreSoft face
    in
        Score soft hard


makeScoreFromHand : List String -> Score
makeScoreFromHand hand =
    let
        faces =
            List.map Card.extractFace hand

        sum f =
            List.foldr (+) 0 <| List.map f faces

        hard =
            sum scoreHard

        soft =
            sum scoreSoft
    in
        Score soft hard


scoreHard : String -> number
scoreHard hand =
    case hand of
        "Ace" ->
            11

        _ ->
            scoreFace hand


scoreSoft : String -> number
scoreSoft card =
    case card of
        "Ace" ->
            1

        _ ->
            scoreFace card


handMinusAce : List String -> List String
handMinusAce cards =
    List.filter (\f -> not (f == "Ace")) (List.map Card.extractFace cards)


scoreMinusAce : List String -> number
scoreMinusAce cards =
    Maybe.withDefault 0 <| List.head <| List.map scoreHard <| handMinusAce cards


isPair : List String -> Bool
isPair xs =
    let
        safeHead z =
            Maybe.withDefault "" <| List.head z

        x =
            safeHead xs

        safeTail z =
            Maybe.withDefault [] <| List.tail z

        y =
            safeHead <| safeTail xs
    in
        x == y


handIsPair : List String -> Bool
handIsPair fullCards =
    if not ((List.length fullCards) == 2) then
        False
    else if isPair <| List.map Card.extractFace fullCards then
        True
    else
        False


makeState : Score -> ScoreState
makeState { hard, soft } =
    if soft > 21 then
        Bust
    else if soft == 21 || hard == 21 then
        Blackjack
    else
        Under


makeStateFromHand playerHand =
    makeState <| makeScoreFromHand playerHand


dealerStandHand dealerHand deck =
    dealerStandUnder (dealerHand ++ deck)
        - 2


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
    case makeState <| makeScoreFromHand playerHand of
        Blackjack ->
            "Blackjack on deal! - You Win!"

        Under ->
            case makeState <| makeScoreFromHand dealerHand of
                Blackjack ->
                    "Dealer Blackjack on deal! - You Lose!"

                Under ->
                    "Welcome To BlackJack!"

                Bust ->
                    "Dealer Bust on Deal, this should never happen!"

        Bust ->
            "Player Bust on Deal, this should never happen!"
