module Score
    exposing
        ( dealStand
        , handIsPair
        , makeScoreFromHand
        , scoreMinusAce
        , Score
        , zero
        )

import Card


type alias Score =
    { soft : Int, hard : Int }


zero : Score
zero =
    Score 0 0


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


under22 : Score -> Bool
under22 x =
    x.soft < 22


dealStand : Int -> List String -> List String -> ( List String, List String )
dealStand softScore dealerHand deck =
    if softScore < 18 then
        Card.dealDealerStand dealerHand deck <|
            dealerStandHand dealerHand deck
    else
        ( dealerHand, deck )


scoresUnderBust : List String -> List Score
scoresUnderBust deck =
    List.filter under22 <| scanlScores <| List.map makeScore deck


dealerStandUnder : List String -> Int
dealerStandUnder deck =
    List.length <| scoresUnderBust deck


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


notAce : String -> Bool
notAce string =
    not (string == "Ace")


handMinusAce : List String -> List String
handMinusAce cards =
    List.filter notAce (List.map Card.extractFace cards)


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


dealerStandHand : List String -> List String -> Int
dealerStandHand dealerHand deck =
    dealerStandUnder (dealerHand ++ deck)
        - 2
