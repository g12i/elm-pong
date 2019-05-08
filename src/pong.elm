module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Vector exposing (Vec, add, scale, sub, vec)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Vec


init : Model
init =
    vec 0 0



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Scale


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            add (vec 1 0) model

        Decrement ->
            sub (vec 1 0) model

        Scale ->
            scale 1.1 model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (Debug.toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Scale ] [ text "Scale" ]
        ]
