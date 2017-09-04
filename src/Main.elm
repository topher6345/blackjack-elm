-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html


module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Array
import Random
import Random.Array
import Card exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { dieFace : Int
    , deck : List String
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
    ( Model 0 initDeck, Cmd.none )



-- UPDATE


type Msg
    = Roll
    | NewFace (Array.Array String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.Array.shuffle (Array.fromList initDeck)) )

        NewFace newFace ->
            ( { model | deck = Array.toList newFace }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text (toString model.dieFace) ]
        , div
            []
            [ text (toString model) ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]
