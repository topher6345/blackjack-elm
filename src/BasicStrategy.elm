module BasicStrategy exposing (..)


type Tactic
    = Stand
    | Hit
    | Split
    | Surrender
    | DoubleHit
    | DoubleStand


type alias StrategyRow =
    { i2 : Tactic
    , i3 : Tactic
    , i4 : Tactic
    , i5 : Tactic
    , i6 : Tactic
    , i7 : Tactic
    , i8 : Tactic
    , i9 : Tactic
    , i10 : Tactic
    , iA : Tactic
    }


fromStrategyRow : StrategyRow -> Int -> Maybe Tactic
fromStrategyRow row score =
    case score of
        11 ->
            Just row.iA

        10 ->
            Just row.i10

        9 ->
            Just row.i9

        8 ->
            Just row.i8

        7 ->
            Just row.i7

        6 ->
            Just row.i6

        5 ->
            Just row.i5

        4 ->
            Just row.i4

        3 ->
            Just row.i3

        2 ->
            Just row.i2

        _ ->
            Nothing


type alias HardStrategy =
    { hard17to20 : StrategyRow
    , hard16 : StrategyRow
    , hard15 : StrategyRow
    , hard13to14 : StrategyRow
    , hard12 : StrategyRow
    , hard11 : StrategyRow
    , hard10 : StrategyRow
    , hard9 : StrategyRow
    , hard5to8 : StrategyRow
    }


type alias SoftStrategy =
    { a9 : StrategyRow
    , a8 : StrategyRow
    , a7 : StrategyRow
    , a6 : StrategyRow
    , a4OrA5 : StrategyRow
    , a2OrA3 : StrategyRow
    }


type alias PairStrategy =
    { pa : StrategyRow
    , p10 : StrategyRow
    , p9 : StrategyRow
    , p8 : StrategyRow
    , p7 : StrategyRow
    , p6 : StrategyRow
    , p5 : StrategyRow
    , p4 : StrategyRow
    , p2to3 : StrategyRow
    }


getHardStrategy : Int -> Int -> Maybe Tactic
getHardStrategy playerScore dealerScore =
    case fromHardStrategy playerScore of
        Just row ->
            fromStrategyRow row dealerScore

        _ ->
            Nothing


fromHardStrategy : Int -> Maybe StrategyRow
fromHardStrategy playerScore =
    case playerScore of
        16 ->
            Just hardStrategy.hard16

        15 ->
            Just hardStrategy.hard15

        12 ->
            Just hardStrategy.hard12

        11 ->
            Just hardStrategy.hard11

        10 ->
            Just hardStrategy.hard10

        9 ->
            Just hardStrategy.hard9

        score ->
            if score >= 17 && score <= 20 then
                Just hardStrategy.hard17to20
            else if score >= 13 && score <= 14 then
                Just hardStrategy.hard13to14
            else if score >= 5 && score <= 8 then
                Just hardStrategy.hard5to8
            else
                Nothing


hardStrategy : HardStrategy
hardStrategy =
    { hard17to20 =
        StrategyRow Stand Stand Stand Stand Stand Stand Stand Stand Stand Stand
    , hard16 =
        StrategyRow Stand Stand Stand Stand Stand Hit Hit Surrender Surrender Surrender
    , hard15 =
        StrategyRow Stand Stand Stand Stand Stand Hit Hit Hit Surrender Hit
    , hard13to14 =
        StrategyRow Stand Stand Stand Stand Stand Hit Hit Hit Hit Hit
    , hard12 =
        StrategyRow Hit Hit Stand Stand Stand Hit Hit Hit Hit Hit
    , hard11 =
        StrategyRow DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit
    , hard10 =
        StrategyRow DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit Hit Hit
    , hard9 =
        StrategyRow Hit DoubleHit DoubleHit DoubleHit DoubleHit Hit Hit Hit Hit Hit
    , hard5to8 =
        StrategyRow Hit Hit Hit Hit Hit Hit Hit Hit Hit Hit
    }


softStrategy : SoftStrategy
softStrategy =
    { a9 = StrategyRow Stand Stand Stand Stand Stand Stand Stand Stand Stand Stand
    , a8 = StrategyRow Stand Stand Stand Stand DoubleStand Stand Stand Stand Stand Stand
    , a7 = StrategyRow DoubleStand DoubleStand DoubleStand DoubleStand DoubleStand Stand Stand Hit Hit Hit
    , a6 = StrategyRow Hit DoubleHit DoubleHit DoubleHit DoubleHit Hit Hit Hit Hit Hit
    , a4OrA5 = StrategyRow Hit Hit DoubleHit DoubleHit DoubleHit Hit Hit Hit Hit Hit
    , a2OrA3 = StrategyRow Hit Hit Hit DoubleHit DoubleHit Hit Hit Hit Hit Hit
    }


pairStrategy : PairStrategy
pairStrategy =
    { pa = StrategyRow Split Split Split Split Split Split Split Split Split Split
    , p10 = StrategyRow Stand Stand Stand Stand Stand Stand Stand Stand Stand Stand
    , p9 = StrategyRow Split Split Split Split Split Stand Split Split Stand Stand
    , p8 = StrategyRow Split Split Split Split Split Split Split Split Split Split
    , p7 = StrategyRow Split Split Split Split Split Split Hit Hit Hit Hit
    , p6 = StrategyRow Split Split Split Split Split Hit Hit Hit Hit Hit
    , p5 = StrategyRow DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit DoubleHit Split Split
    , p4 = StrategyRow Hit Hit Hit Split Split Hit Hit Hit Hit Hit
    , p2to3 = StrategyRow Split Split Split Split Split Split Hit Hit Hit Hit
    }
