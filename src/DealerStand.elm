module DealerStand exposing (dealStand)

import Card exposing (dealN)
import Score exposing (scoresUnderBust)


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
    Score.scoresUnderBust deck |> List.length
