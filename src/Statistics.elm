module Statistics
    exposing
        ( wins
        , showPeak
        , histories
        , winPercentageDisplay
        )

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
    List.map .winner history
        |> List.filter isWin
        |> List.length


winPercentageDisplay : List { a | winner : PlayerState } -> String
winPercentageDisplay history =
    winPercentage history |> safeWinPercentage


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
    List.map .pocket history
        |> List.maximum
        |> Maybe.withDefault 0


isStart : { a | winner : PlayerState } -> Bool
isStart game =
    case game.winner of
        Start _ ->
            False

        _ ->
            True


rejectStart :
    List { a | winner : PlayerState }
    -> List { a | winner : PlayerState }
rejectStart history =
    history |> List.filter isStart


histories :
    List { b | pocket : a, winPercentage : String, winner : PlayerState }
    -> List String
histories history =
    rejectStart history |> List.map gameLi


gameLi :
    { b
        | pocket : a
        , winPercentage : String
        , winner : PlayerState
    }
    -> String
gameLi game =
    liText (toString game.pocket)
        (Flash.toString game.winner)
        (winPercentageString game.winPercentage)


liText : String -> String -> String -> String
liText pocket winner percentage =
    " $" ++ pocket ++ " " ++ winner ++ "  " ++ percentage


winPercentageString : String -> String
winPercentageString percentage =
    case String.toFloat percentage of
        Ok _ ->
            percentage ++ "%"

        Err _ ->
            ""
