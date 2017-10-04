module BlackjackTest exposing (..)

import Card
import Score
import StrLib
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Card"
        [ describe "dealN"
            [ test "dealN 1" <|
                \_ -> Card.dealN [ "Ace Hearts" ] [ "Ace Spades" ] 1 |> Expect.equal ( [ "Ace Hearts", "Ace Spades" ], [] )
            , test "dealN 0" <|
                \_ -> Card.dealN [ "Ace Hearts" ] [ "Ace Spades" ] 0 |> Expect.equal ( [ "Ace Hearts" ], [ "Ace Spades" ] )
            , test "dealN 1 from empty is no-op" <|
                \_ -> Card.dealN [ "Ace Hearts" ] [] 1 |> Expect.equal ( [ "Ace Hearts" ], [] )
            ]
        , describe "isPair"
            [ test "isPair Aces" <|
                \_ -> Score.hasPair [ "Ace Hearts", "Ace Spades" ] |> Expect.equal True
            , test "isPair Three Cards" <|
                \_ -> Score.hasPair [ "Ace Hearts", "Ace Spades", "Ace Diamonds" ] |> Expect.equal False
            , test "isPair Not Pair" <|
                \_ -> Score.hasPair [ "Five Hearts", "Ace Spades" ] |> Expect.equal False
            ]
        , describe "showDealerHand"
            [ test "showDealerHand Aces" <|
                \_ -> (Card.showDealerHand [ "Ace Hearts", "Ace Spades" ]) |> Expect.equal [ "🂠", "🂡" ]
            , test "showDealerHand empty" <|
                \_ -> (Card.showDealerHand []) |> Expect.equal []
            ]
        , describe "StrLib"
            [ test "commaize 4 digits" <|
                \_ -> StrLib.commaize "1234" |> Expect.equal "1,234"
            , test "commaize 5 digits" <|
                \_ -> StrLib.commaize "10234" |> Expect.equal "10,234"
            , test "commaize 3 digits" <|
                \_ -> StrLib.commaize "123" |> Expect.equal "123"
            ]
        ]
