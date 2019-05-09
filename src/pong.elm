module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Vector exposing (Vec, add, scale, sub, vec)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Player =
    { pos : Vec
    , score : Int
    }


type alias Ball =
    { pos : Vec
    , dir : Vec
    }


type alias Model =
    { player1 : Player
    , player2 : Player
    , ball : Ball
    , isRoundFinished : Bool
    }


init : Model
init =
    { player1 = { pos = vec 0 0, score = 0 }
    , player2 = { pos = vec 0 0, score = 0 }
    , ball = { pos = vec 0 0, dir = vec 0 0 }
    , isRoundFinished = False
    }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Scale


updatePlayerPos : (Vec -> Vec) -> Player -> Player
updatePlayerPos operation player =
    { player | pos = operation player.pos }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | player1 = updatePlayerPos (add (vec 1 0)) model.player1 }

        Decrement ->
            { model | player1 = updatePlayerPos (sub (vec 1 0)) model.player1 }

        Scale ->
            { model | player1 = updatePlayerPos (scale 1.1) model.player1 }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (Debug.toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Scale ] [ text "Scale" ]
        ]
