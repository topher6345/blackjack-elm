module Score
    exposing
        ( hasPair
        , hasAce
        , fromHand
        , fromHandMinusAce
        , makeScore
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
    Maybe.withDefault 0 <| List.head <| List.map scoreHard <| handMinusAce cards


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
