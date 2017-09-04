-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html


module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Array
import Random
import Random.Array
import Card exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { round : Int
    , deck : List String
    , playerHand : List String
    , dealerHand : List String
    }


initDeck : List String
initDeck =
    let
        stringify x =
            (toString x.face) ++ (toString x.suit)

        cards =
            List.map stringify Card.cards
    in
        cards


init : ( Model, Cmd Msg )
init =
    ( Model 0 [] [] [], shuffleDeck )



-- UPDATE


type Msg
    = NewGame
    | ShuffleDeck (Array.Array String)


shuffleDeck : Cmd Msg
shuffleDeck =
    Random.generate ShuffleDeck (Random.Array.shuffle (Array.fromList initDeck))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, shuffleDeck )

        ShuffleDeck cards ->
            let
                playerHand =
                    Array.toList (Array.slice 0 2 cards)

                dealerHand =
                    Array.toList (Array.slice 3 5 cards)

                newDeck =
                    Array.toList (Array.slice 5 -1 cards)

                newRound =
                    model.round + 1
            in
                ( { model
                    | deck = newDeck
                    , round = newRound
                    , dealerHand = dealerHand
                    , playerHand = playerHand
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "BlackJack" ]
        , div [] [ text (toString model.round) ]
        , h2 [] [ text "Player" ]
        , div [] [ text (toString model.playerHand) ]
        , h2 [] [ text "Dealer" ]
        , div [] [ text (toString model.dealerHand) ]
        , button [ onClick NewGame ] [ text "New Game" ]
        , button [ onClick NewGame ] [ text "Hit" ]
        , button [ onClick NewGame ] [ text "Stand" ]
        , button [ onClick NewGame ] [ text "Surrender" ]
        ]
