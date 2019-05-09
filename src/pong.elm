module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Vector exposing (Vec, add, scale, sub, vec)


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



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
    , pressedKeys : List Key
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { player1 = { pos = vec 0 0, score = 0 }
      , player2 = { pos = vec 0 0, score = 0 }
      , ball = { pos = vec 0 0, dir = vec 0 0 }
      , isRoundFinished = False
      , pressedKeys = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Scale
    | KeyMsg Keyboard.Msg
    | Tick


updatePlayerPos : (Vec -> Vec) -> Player -> Player
updatePlayerPos operation player =
    { player | pos = operation player.pos }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | player1 = updatePlayerPos (add (vec 1 0)) model.player1 }, Cmd.none )

        Decrement ->
            ( { model | player1 = updatePlayerPos (sub (vec 1 0)) model.player1 }, Cmd.none )

        Scale ->
            ( { model | player1 = updatePlayerPos (scale 1.1) model.player1 }, Cmd.none )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none )

        Tick ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        , Browser.Events.onAnimationFrame (\_ -> Tick)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (Debug.toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Scale ] [ text "Scale" ]
        ]
