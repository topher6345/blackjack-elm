module Score exposing (..)

import Card


type alias Score =
    { soft : Int, hard : Int }


type ScoreState
    = Blackjack
    | Under
    | Bust


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


cardsUnderBust : List String -> List Score
cardsUnderBust deck =
    List.filter (\x -> x.soft < 22) <| scanlScores <| List.map makeScore deck



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


makeState : Score -> ScoreState
makeState { hard, soft } =
    if soft > 21 then
        Bust
    else if soft == 21 || hard == 21 then
        Blackjack
    else
        Under
