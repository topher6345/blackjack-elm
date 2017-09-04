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


type alias Score =
    { soft : Int, hard : Int }


type alias DealerHand =
    { invisible : Card, visible : List Card }


type alias PlayerHand =
    List Card



--compareScores : Score -> Score -> Boolean


initDeck : List String
initDeck =
    let
        stringify x =
            (toString x.face) ++ (toString x.suit)
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
