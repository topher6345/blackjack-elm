module StrLib exposing (commaize, totalGames)


reverseLetters : String -> List String
reverseLetters string =
    String.reverse string |> String.split ""


commaize : String -> String
commaize string =
    reverseLetters string |> commaizeLetters 0 |> String.reverse


commaizeLetters : Int -> List String -> String
commaizeLetters count list =
    case list of
        x :: xs ->
            if (count > 2) && (count % 3 == 0) then
                "," ++ x ++ commaizeLetters (count + 1) xs
            else
                x ++ commaizeLetters (count + 1) xs

        _ ->
            ""


zeroFloor : Int -> Int
zeroFloor x =
    if x /= 0 then
        x - 1
    else
        0


totalGames : List a -> String
totalGames history =
    List.length history |> zeroFloor |> toString
