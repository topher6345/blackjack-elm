module Statistics exposing (..)

import Flash exposing (PlayerState(Start, Continue, Win, Lose, Tie))


isWin : PlayerState -> Bool
isWin state =
    case state of
        Win _ ->
            True

        _ ->
            False


wins : List { a | winner : PlayerState } -> Int
wins history =
    List.length <| List.filter isWin <| List.map (\x -> x.winner) history


winPercentage : List { a | winner : PlayerState } -> Float
winPercentage history =
    (toFloat <| wins history) / (toFloat <| (List.length history - 1))


safeWinPercentage : Float -> String
safeWinPercentage float =
    if isNaN float then
        ""
    else if float < 0.1 then
        ""
    else
        toString <| round (float * 100)


showPeak : List { b | pocket : Int } -> Int
showPeak history =
    Maybe.withDefault 0 <| List.maximum <| List.map .pocket history


rejectStart :
    List { a | winner : PlayerState }
    -> List { a | winner : PlayerState }
rejectStart history =
    let
        f x =
            case x.winner of
                Start _ ->
                    False

                _ ->
                    True
    in
        List.filter f history
