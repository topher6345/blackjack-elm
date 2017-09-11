module Statistics exposing (..)


wins : List { a | winner : String } -> Int
wins history =
    List.length <| List.filter (\x -> String.contains "Win" x) <| List.map (\x -> x.winner) history


winPercentage : List { a | winner : String } -> Float
winPercentage history =
    (toFloat <| wins history) / (toFloat <| (List.length history - 1))


safeWinPercentage float =
    if isNaN float then
        ""
    else if float < 0.1 then
        ""
    else
        toString <| round (float * 100)
