module Score
    exposing
        ( hardDealerScore
        , hasPair
        , hasAce
        , fromHand
        , fromHandHard
        , fromHandMinusAce
        , makeScore
        , scoresUnderBust
        , Score
        , zero
        )

-- A module for operations on computing Scores given a Hand

import Card


type alias Score =
    { soft : Int, hard : Int }


type alias Hand =
    List String


zero : Score
zero =
    Score 0 0


scoreFace : String -> Int
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


fromHandHard : Hand -> Int
fromHandHard hand =
    (fromHand hand).hard


fromHand : Hand -> Score
fromHand hand =
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


scoreHard : String -> Int
scoreHard hand =
    case hand of
        "Ace" ->
            11

        _ ->
            scoreFace hand


scoreSoft : String -> Int
scoreSoft card =
    case card of
        "Ace" ->
            1

        _ ->
            scoreFace card


notAce : String -> Bool
notAce string =
    not (string == "Ace")


handMinusAce : Hand -> Hand
handMinusAce cards =
    List.filter notAce (List.map Card.extractFace cards)


fromHandMinusAce : Hand -> Int
fromHandMinusAce cards =
    cards
        |> handMinusAce
        |> List.map scoreHard
        |> List.head
        |> Maybe.withDefault 0


hasPair : Hand -> Bool
hasPair xs =
    case List.map Card.extractFace xs of
        [ a, b ] ->
            a == b

        _ ->
            False


hasAce : Score -> Bool
hasAce score =
    score.hard /= score.soft


hardDealerScore : List String -> Int
hardDealerScore dealerHand =
    case dealerHand of
        [ n, last ] ->
            (fromHand [ last ]).hard

        _ ->
            0


addScores : Score -> Score -> Score
addScores x y =
    { soft = x.soft + y.soft, hard = x.hard + y.hard }


under22 : Score -> Bool
under22 x =
    x.soft < 22


scoresUnderBust : Hand -> List Score
scoresUnderBust deck =
    List.map makeScore deck |> scanlScores |> List.filter under22


scanlScores : List Score -> List Score
scanlScores list =
    let
        score =
            { soft = 0, hard = 0 }

        head =
            List.head list |> Maybe.withDefault score

        tail =
            List.tail list |> Maybe.withDefault []
    in
        List.scanl addScores head tail
