module Score exposing (..)


type alias Score =
    { soft : Int, hard : Int }


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


scoreHand : a -> Score
scoreHand xs =
    Score 0 0
