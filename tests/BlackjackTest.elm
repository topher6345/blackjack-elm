module BlackjackTest exposing (..)

import Card
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
        ]
