module DealerStand exposing (dealStand)

import Card exposing (dealN)
import Score exposing (Score, addScores)


type alias Hand =
    List String


dealStand : Int -> Hand -> Hand -> ( Hand, Hand )
dealStand softScore dealerHand deck =
    if softScore < 18 then
        dealDealerStand dealerHand deck <|
            dealerStandHand dealerHand deck
    else
        ( dealerHand, deck )


dealDealerStand : Hand -> Hand -> Int -> ( Hand, Hand )
dealDealerStand dealerHand deck standunder =
    Card.dealN dealerHand deck standunder


dealerStandHand : Hand -> Hand -> Int
dealerStandHand dealerHand deck =
    dealerStandUnder (dealerHand ++ deck) - 2


dealerStandUnder : Hand -> Int
dealerStandUnder deck =
    scoresUnderBust deck |> List.length


under22 : Score -> Bool
under22 x =
    x.soft < 22


scoresUnderBust : Hand -> List Score
scoresUnderBust deck =
    List.map Score.makeScore deck |> scanlScores |> List.filter under22


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
        List.scanl Score.addScores head tail
