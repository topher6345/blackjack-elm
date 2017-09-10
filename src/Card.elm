module Card exposing (..)


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
    let
        stringify x =
            (toString x.face) ++ " " ++ (toString x.suit)
    in
        List.map stringify cards


cards : List Card
cards =
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
        List.concatMap (\face -> List.map (\suit -> Card face suit) suits) faces


extractFace : String -> String
extractFace x =
    Maybe.withDefault "" <| List.head <| String.words x


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
            "Fdsafdsafdsa"
