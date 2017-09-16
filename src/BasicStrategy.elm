module BasicStrategy exposing (..)

import Array exposing (Array)


type Tactic
    = Stand
    | Hit
    | SplitHit
    | Surrender
    | DoubleHit
    | DoubleStand



-- [ Stand, Stand, Stand, Stand, Stand, Hit, Hit, Surrender, Surrender, Surrender ]


type alias StrategyRow =
    Array Tactic


type alias StrategyMatrix =
    Array StrategyRow


hard17to20 : StrategyRow
hard17to20 =
    Array.fromList [ Stand, Stand, Stand, Stand, Stand, Stand, Stand, Stand, Stand, Stand ]


hard16 : StrategyRow
hard16 =
    Array.fromList [ Stand, Stand, Stand, Stand, Stand, Hit, Hit, Surrender, Surrender, Surrender ]


hard15 : StrategyRow
hard15 =
    Array.fromList [ Stand, Stand, Stand, Stand, Stand, Hit, Hit, Hit, Surrender, Hit ]


hard13to14 : StrategyRow
hard13to14 =
    Array.fromList [ Stand, Stand, Stand, Stand, Stand, Hit, Hit, Hit, Hit, Hit ]


hard12 : StrategyRow
hard12 =
    Array.fromList [ Hit, Hit, Stand, Stand, Stand, Hit, Hit, Hit, Hit, Hit ]


hard11 : StrategyRow
hard11 =
    Array.fromList [ DoubleHit, DoubleHit, DoubleHit, DoubleHit, DoubleHit, DoubleHit, DoubleHit, DoubleHit, DoubleHit, DoubleHit ]


hard10 : StrategyRow
hard10 =
    Array.fromList [ DoubleHit, DoubleHit, DoubleHit, DoubleHit, DoubleHit, DoubleHit, DoubleHit, DoubleHit, Hit, Hit ]


hard9 : StrategyRow
hard9 =
    Array.fromList [ Hit, DoubleHit, DoubleHit, DoubleHit, DoubleHit, Hit, Hit, Hit, Hit, Hit ]


hard5to8 : StrategyRow
hard5to8 =
    Array.fromList [ Hit, Hit, Hit, Hit, Hit, Hit, Hit, Hit, Hit, Hit ]


hardMatrix : StrategyMatrix
hardMatrix =
    Array.fromList
        [ hard17to20
        , hard16
        , hard15
        , hard13to14
        , hard12
        , hard11
        , hard10
        , hard9
        , hard5to8
        ]


getStrategyMatix : Array (Array a) -> Int -> Int -> Maybe a
getStrategyMatix matrix x y =
    case Array.get x matrix of
        Just row ->
            Array.get y row

        Nothing ->
            Nothing
