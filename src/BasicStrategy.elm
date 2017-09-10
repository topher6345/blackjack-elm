module BasicStrategy
    exposing
        ( basicStrategy
        , legend
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Score


-- https://mbylstra.github.io/html-to-elm/
-- https://en.wikipedia.org/wiki/Blackjack#Blackjack_strategy


legend : Html msg
legend =
    dl []
        [ dd []
            [ span [ attribute "style" "background:red; color:black" ]
                [ b []
                    [ text "S" ]
                ]
            , text "= Stand"
            ]
        , dd []
            [ span [ attribute "style" "background:lime; color:black" ]
                [ b []
                    [ text "H" ]
                ]
            , text "= Hit"
            ]
        , dd []
            [ span [ attribute "style" "background:cyan; color:black" ]
                [ b []
                    [ text "Dh" ]
                ]
            , text "= Double (if not allowed, then hit)"
            ]
        , dd []
            [ span [ attribute "style" "background:cyan; color:black" ]
                [ b []
                    [ text "Ds" ]
                ]
            , text "= Double (if not allowed, then stand)"
            ]
        , dd []
            [ span [ attribute "style" "background:yellow; color:black" ]
                [ b []
                    [ text "SP" ]
                ]
            , text "= Split"
            ]
        , dd []
            [ span [ attribute "style" "background:white; color:black" ]
                [ b []
                    [ text "SU" ]
                ]
            , text "= Surrender (if not allowed, then hit)"
            ]
        ]


stand : Html msg
stand =
    td [ attribute "style" "background:red; color:black" ]
        [ text "S" ]


hit : Html msg
hit =
    td [ attribute "style" "background:lime; color:black" ]
        [ text "H" ]


sp : Html msg
sp =
    td [ attribute "style" "background:yellow; color:black" ]
        [ text "SP" ]


su : Html msg
su =
    td [ attribute "style" "background:white; color:black" ]
        [ text "SU" ]


dh : Html msg
dh =
    td [ attribute "style" "background:cyan; color:black" ]
        [ text "Dh" ]


ds : Html msg
ds =
    td [ attribute "style" "background:cyan; color:black" ]
        [ text "Ds" ]


purpleAttribute : Attribute msg
purpleAttribute =
    attribute "style" "background:purple; color:black"


blankStyle : Attribute msg
blankStyle =
    attribute "style" ""


hasAce : Score.Score -> Bool
hasAce playerScore =
    playerScore.hard /= playerScore.soft


playerScoreAttributes : Int -> List Int -> Attribute a
playerScoreAttributes score range =
    case List.member score range of
        True ->
            purpleAttribute

        False ->
            blankStyle


dealerScoreAttribute : Int -> Int -> Attribute a
dealerScoreAttribute score value =
    case score == value of
        True ->
            purpleAttribute

        False ->
            blankStyle


dealerSoftScoreAttribute : Bool -> Int -> Int -> Attribute a
dealerSoftScoreAttribute hasAce score value =
    case hasAce of
        True ->
            case score == value of
                True ->
                    purpleAttribute

                False ->
                    blankStyle

        False ->
            blankStyle


playerSoftScoreAttribute : Bool -> Int -> List Int -> Attribute msg
playerSoftScoreAttribute hasAce score range =
    case hasAce of
        True ->
            playerScoreAttributes score range

        False ->
            blankStyle


basicStrategy :
    { a
        | dealerHand : List String
        , playerHand : List String
        , playerScore : Score.Score
    }
    -> Html msg
basicStrategy model =
    let
        -- Multiple call sites in table
        playerScore =
            model.playerScore

        -- Multiple call sites in table
        playerHasAce =
            hasAce playerScore

        playerScoreWithoutAce =
            Score.scoreMinusAce model.playerHand

        dealerScore =
            Score.makeScoreFromHand <| Maybe.withDefault [] <| List.tail model.dealerHand

        playerHandIsPair =
            Score.handIsPair model.playerHand
    in
        table [ attribute "border" "1", attribute "style" "text-align:center" ]
            [ tbody []
                [ tr []
                    [ th [ attribute "rowspan" "2" ]
                        [ text "Player hand" ]
                    , th [ attribute "colspan" "10" ]
                        [ text "Dealer's face-up card" ]
                    ]
                , tr []
                    [ td [ dealerScoreAttribute dealerScore.hard 2 ]
                        [ text "2" ]
                    , td [ dealerScoreAttribute dealerScore.hard 3 ]
                        [ text "3" ]
                    , td [ dealerScoreAttribute dealerScore.hard 4 ]
                        [ text "4" ]
                    , td [ dealerScoreAttribute dealerScore.hard 5 ]
                        [ text "5" ]
                    , td [ dealerScoreAttribute dealerScore.hard 6 ]
                        [ text "6" ]
                    , td [ dealerScoreAttribute dealerScore.hard 7 ]
                        [ text "7" ]
                    , td [ dealerScoreAttribute dealerScore.hard 8 ]
                        [ text "8" ]
                    , td [ dealerScoreAttribute dealerScore.hard 9 ]
                        [ text "9" ]
                    , td [ dealerScoreAttribute dealerScore.hard 10 ]
                        [ text "10" ]
                    , td [ dealerScoreAttribute dealerScore.hard 11 ]
                        [ text "A" ]
                    ]
                , tr []
                    [ th [ attribute "colspan" "11" ]
                        [ text "Hard totals (excluding pairs)" ]
                    ]
                , tr []
                    [ th [ playerScoreAttributes playerScore.hard (List.range 17 20) ]
                        [ text "17–20" ]
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    ]
                , tr []
                    [ th [ playerScoreAttributes playerScore.hard (List.range 16 16) ]
                        [ text "16" ]
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , hit
                    , hit
                    , su
                    , su
                    , su
                    ]
                , tr []
                    [ th [ playerScoreAttributes playerScore.hard (List.range 15 15) ]
                        [ text "15" ]
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , hit
                    , hit
                    , hit
                    , su
                    , hit
                    ]
                , tr []
                    [ th [ playerScoreAttributes playerScore.hard (List.range 13 14) ]
                        [ text "13–14" ]
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ playerScoreAttributes playerScore.hard (List.range 12 12) ]
                        [ text "12" ]
                    , hit
                    , hit
                    , stand
                    , stand
                    , stand
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ playerScoreAttributes playerScore.hard (List.range 11 11) ]
                        [ text "11" ]
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    ]
                , tr []
                    [ th [ playerScoreAttributes playerScore.hard (List.range 10 10) ]
                        [ text "10" ]
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ playerScoreAttributes playerScore.hard (List.range 9 9) ]
                        [ text "9" ]
                    , hit
                    , dh
                    , dh
                    , dh
                    , dh
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ playerScoreAttributes playerScore.hard (List.range 5 8) ]
                        [ text "5–8" ]
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ attribute "colspan" "11" ]
                        [ text "Soft totals" ]
                    ]
                , tr []
                    [ td []
                        []
                    , td [ dealerSoftScoreAttribute playerHasAce dealerScore.soft 2 ]
                        [ text "2" ]
                    , td [ dealerSoftScoreAttribute playerHasAce dealerScore.soft 3 ]
                        [ text "3" ]
                    , td [ dealerSoftScoreAttribute playerHasAce dealerScore.soft 4 ]
                        [ text "4" ]
                    , td [ dealerSoftScoreAttribute playerHasAce dealerScore.soft 5 ]
                        [ text "5" ]
                    , td [ dealerSoftScoreAttribute playerHasAce dealerScore.soft 6 ]
                        [ text "6" ]
                    , td [ dealerSoftScoreAttribute playerHasAce dealerScore.soft 7 ]
                        [ text "7" ]
                    , td [ dealerSoftScoreAttribute playerHasAce dealerScore.soft 8 ]
                        [ text "8" ]
                    , td [ dealerSoftScoreAttribute playerHasAce dealerScore.soft 9 ]
                        [ text "9" ]
                    , td [ dealerSoftScoreAttribute playerHasAce dealerScore.soft 10 ]
                        [ text "10" ]
                    , td [ dealerSoftScoreAttribute playerHasAce dealerScore.soft 1 ]
                        [ text "A" ]
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHasAce playerScoreWithoutAce <| List.range 9 9 ]
                        [ text "A,9" ]
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHasAce playerScoreWithoutAce <| List.range 8 8 ]
                        [ text "A,8" ]
                    , stand
                    , stand
                    , stand
                    , stand
                    , ds
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHasAce playerScoreWithoutAce <| List.range 7 7 ]
                        [ text "A,7" ]
                    , ds
                    , ds
                    , ds
                    , ds
                    , ds
                    , stand
                    , stand
                    , hit
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHasAce playerScoreWithoutAce <| List.range 6 6 ]
                        [ text "A,6" ]
                    , hit
                    , dh
                    , dh
                    , dh
                    , dh
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHasAce playerScoreWithoutAce <| List.range 4 5 ]
                        [ text "A,4–A,5" ]
                    , hit
                    , hit
                    , dh
                    , dh
                    , dh
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHasAce playerScoreWithoutAce <| List.range 2 3 ]
                        [ text "A,2–A,3" ]
                    , hit
                    , hit
                    , hit
                    , dh
                    , dh
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ attribute "colspan" "11" ]
                        [ text "Pairs" ]
                    ]
                , tr []
                    [ td []
                        []
                    , td [ dealerSoftScoreAttribute playerHandIsPair dealerScore.soft 2 ]
                        [ text "2" ]
                    , td [ dealerSoftScoreAttribute playerHandIsPair dealerScore.soft 3 ]
                        [ text "3" ]
                    , td [ dealerSoftScoreAttribute playerHandIsPair dealerScore.soft 4 ]
                        [ text "4" ]
                    , td [ dealerSoftScoreAttribute playerHandIsPair dealerScore.soft 5 ]
                        [ text "5" ]
                    , td [ dealerSoftScoreAttribute playerHandIsPair dealerScore.soft 6 ]
                        [ text "6" ]
                    , td [ dealerSoftScoreAttribute playerHandIsPair dealerScore.soft 7 ]
                        [ text "7" ]
                    , td [ dealerSoftScoreAttribute playerHandIsPair dealerScore.soft 8 ]
                        [ text "8" ]
                    , td [ dealerSoftScoreAttribute playerHandIsPair dealerScore.soft 9 ]
                        [ text "9" ]
                    , td [ dealerSoftScoreAttribute playerHandIsPair dealerScore.soft 10 ]
                        [ text "10" ]
                    , td [ dealerSoftScoreAttribute playerHandIsPair dealerScore.soft 11 ]
                        [ text "A" ]
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHandIsPair playerScore.hard [ 22 ] ]
                        [ text "A,A" ]
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHandIsPair playerScore.soft [ 20 ] ]
                        [ text "10,10" ]
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    , stand
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHandIsPair playerScore.soft [ 18 ] ]
                        [ text "9,9" ]
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , stand
                    , sp
                    , sp
                    , stand
                    , stand
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHandIsPair playerScore.soft [ 16 ] ]
                        [ text "8,8" ]
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHandIsPair playerScore.soft [ 14 ] ]
                        [ text "7,7" ]
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , hit
                    , hit
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHandIsPair playerScore.soft [ 12 ] ]
                        [ text "6,6" ]
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHandIsPair playerScore.soft [ 10 ] ]
                        [ text "5,5" ]
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , dh
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHandIsPair playerScore.soft [ 8 ] ]
                        [ text "4,4" ]
                    , hit
                    , hit
                    , hit
                    , sp
                    , sp
                    , hit
                    , hit
                    , hit
                    , hit
                    , hit
                    ]
                , tr []
                    [ th [ playerSoftScoreAttribute playerHandIsPair playerScore.soft [ 4, 6 ] ]
                        [ text "2,2–3,3" ]
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , sp
                    , hit
                    , hit
                    , hit
                    , hit
                    ]
                ]
            ]
