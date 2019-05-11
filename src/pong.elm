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
      , ball = { pos = vec 0 0, dir = vec 2 1 }
      , isRoundFinished = False
      , pressedKeys = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyMsg Keyboard.Msg
    | Tick


updatePlayerPos : (Vec -> Vec) -> Player -> Player
updatePlayerPos operation player =
    { player | pos = operation player.pos }


updateBall : Ball -> Ball
updateBall ball =
    { ball | pos = add ball.pos ball.dir }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none )

        Tick ->
            let
                arrows =
                    Keyboard.Arrows.arrows model.pressedKeys

                wasd =
                    Keyboard.Arrows.wasd model.pressedKeys

                newPlayer1 =
                    updatePlayerPos (add (vec 0 (toFloat arrows.y))) model.player1

                newPlayer2 =
                    updatePlayerPos (add (vec 0 (toFloat wasd.y))) model.player2

                newBall =
                    updateBall model.ball
            in
            ( { model | player1 = newPlayer1, player2 = newPlayer2, ball = newBall }, Cmd.none )



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
        [ div [] [ text (Debug.toString model) ]
        ]
