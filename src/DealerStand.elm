module DealerStand exposing (dealStand)

import Card
import Score exposing (Score)


type alias Hand =
    List String


dealStand : Int -> Hand -> Hand -> ( Hand, Hand )
dealStand softScore dealerHand deck =
    if softScore < 18 then
        Card.dealDealerStand dealerHand deck <|
            dealerStandHand dealerHand deck
    else
        ( dealerHand, deck )


dealerStandHand : Hand -> Hand -> Int
dealerStandHand dealerHand deck =
    dealerStandUnder (dealerHand ++ deck)
        - 2


dealerStandUnder : Hand -> Int
dealerStandUnder deck =
    List.length <| scoresUnderBust deck


scoresUnderBust : Hand -> List Score
scoresUnderBust deck =
    let
        under22 : Score -> Bool
        under22 x =
            x.soft < 22
    in
        List.filter under22 <| scanlScores <| List.map Score.makeScore deck


scanlScores : List Score -> List Score
scanlScores list =
    let
        score : Score
        score =
            { soft = 0, hard = 0 }

        head : Score
        head =
            Maybe.withDefault score <| List.head list

        tail : List Score
        tail =
            Maybe.withDefault [] <| List.tail list

        add : Score -> Score -> Score
        add x y =
            { soft = x.soft + y.soft, hard = x.hard + y.hard }
    in
        List.scanl add head tail
