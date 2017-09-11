module Card
    exposing
        ( dealN
        , dealDealerStand
        , showDealerHand
        , dealShuffled
        , extractFace
        , initDeck
        , showPlayerHand
        )


type Face
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type Suit
    = Spades
    | Hearts
    | Diamonds
    | Clubs


type alias Card =
    { face : Face, suit : Suit }


type alias DealerHand =
    { invisible : Card, visible : List Card }


type alias PlayerHand =
    List Card


initDeck : List String
initDeck =
    List.map cardToString orderedDeck


cardToString : Card -> String
cardToString card =
    (toString card.face) ++ " " ++ (toString card.suit)


flatZip : List Suit -> List Face -> List Card
flatZip s f =
    List.concatMap (\face -> List.map (\suit -> Card face suit) s) f


orderedDeck : List Card
orderedDeck =
    let
        faces =
            [ Ace
            , Two
            , Three
            , Four
            , Five
            , Six
            , Seven
            , Eight
            , Nine
            , Ten
            , Jack
            , Queen
            , King
            ]

        suits =
            [ Spades
            , Hearts
            , Diamonds
            , Clubs
            ]
    in
        flatZip suits faces


dealShuffled : List a -> ( List a, List a, List a )
dealShuffled deck =
    let
        ( playerHand, d2 ) =
            dealN [] deck 2

        ( dealerHand, d3 ) =
            dealN [] d2 2
    in
        ( d3, dealerHand, playerHand )


extractFace : String -> String
extractFace x =
    Maybe.withDefault "" <| List.head <| String.words x


dealN : List a -> List a -> Int -> ( List a, List a )
dealN to from n =
    let
        c1 =
            List.take n from

        c2 =
            List.drop n from
    in
        ( to ++ c1, c2 )


showPlayerHand : List String -> List String
showPlayerHand hand =
    List.map cardStringToGlyph hand


showDealerHand : List String -> String
showDealerHand hand =
    let
        tail =
            Maybe.withDefault [] <| List.tail hand

        headOfTail =
            Maybe.withDefault "" <| List.head tail

        dealerCards =
            cardStringToGlyph headOfTail
    in
        "🂠" ++ dealerCards


dealDealerStand : List a -> List a -> Int -> ( List a, List a )
dealDealerStand dealerHand deck standunder =
    dealN dealerHand deck standunder


cardStringToGlyph : String -> String
cardStringToGlyph string =
    case string of
        "Ace Spades" ->
            "🂡"

        "Ace Hearts" ->
            "🂱"

        "Ace Diamonds" ->
            "🃁"

        "Ace Clubs" ->
            "🃑"

        "Two Spades" ->
            "🂢"

        "Two Hearts" ->
            "🂲"

        "Two Diamonds" ->
            "🃂"

        "Two Clubs" ->
            "🃒"

        "Three Spades" ->
            "🂣"

        "Three Hearts" ->
            "🂳"

        "Three Diamonds" ->
            "🃃"

        "Three Clubs" ->
            "🃓"

        "Four Spades" ->
            "🂤"

        "Four Hearts" ->
            "🂴"

        "Four Diamonds" ->
            "🃄"

        "Four Clubs" ->
            "🃔"

        "Five Spades" ->
            "🂥"

        "Five Hearts" ->
            "🂵"

        "Five Diamonds" ->
            "🃅"

        "Five Clubs" ->
            "🃕"

        "Six Spades" ->
            "🂦"

        "Six Hearts" ->
            "🂶"

        "Six Diamonds" ->
            "🃆"

        "Six Clubs" ->
            "🃖"

        "Seven Spades" ->
            "🂧"

        "Seven Hearts" ->
            "🂷"

        "Seven Diamonds" ->
            "🃇"

        "Seven Clubs" ->
            "🃗"

        "Eight Spades" ->
            "🂨"

        "Eight Hearts" ->
            "🂸"

        "Eight Diamonds" ->
            "🃈"

        "Eight Clubs" ->
            "🃘"

        "Nine Spades" ->
            "🂩"

        "Nine Hearts" ->
            "🂹"

        "Nine Diamonds" ->
            "🃉"

        "Nine Clubs" ->
            "🃙"

        "Ten Spades" ->
            "🂪"

        "Ten Hearts" ->
            "🂺"

        "Ten Diamonds" ->
            "🃊"

        "Ten Clubs" ->
            "🃚"

        "Jack Spades" ->
            "🂫"

        "Jack Hearts" ->
            "🂻"

        "Jack Diamonds" ->
            "🃋"

        "Jack Clubs" ->
            "🃛"

        "Queen Spades" ->
            "🂭"

        "Queen Hearts" ->
            "🂽"

        "Queen Diamonds" ->
            "🃍"

        "Queen Clubs" ->
            "🃝"

        "King Spades" ->
            "🂮"

        "King Hearts" ->
            "🂾"

        "King Diamonds" ->
            "🃎"

        "King Clubs" ->
            "🃞"

        _ ->
            ""
