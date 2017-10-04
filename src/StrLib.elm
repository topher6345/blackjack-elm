module StrLib exposing (commaize, totalGames)


commaize : String -> String
commaize string =
    String.reverse <| commaizeLetters (String.split "" (String.reverse string)) 0


commaizeLetters : List String -> Int -> String
commaizeLetters list count =
    case list of
        x :: xs ->
            if (count > 2) && (count % 3 == 0) then
                "," ++ x ++ commaizeLetters xs (count + 1)
            else
                x ++ commaizeLetters xs (count + 1)

        _ ->
            ""


totalGames : List a -> String
totalGames history =
    let
        zeroFloor : Int -> Int
        zeroFloor x =
            if x /= 0 then
                x - 1
            else
                0
    in
        List.length history |> zeroFloor |> toString
