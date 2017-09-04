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


extractFace : String -> String
extractFace x =
    Maybe.withDefault "" <| List.head <| String.words x


scoreHand : List String -> Score
scoreHand hand =
    let
        cards =
            List.map extractFace hand

        hard =
            List.foldr (+) 0 <| List.map scoreHard cards

        soft =
            List.foldr (+) 0 <| List.map scoreSoft cards
    in
        Score soft hard
