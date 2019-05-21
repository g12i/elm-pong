module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Html exposing (Html, div, pre)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Svg exposing (circle, line, rect, svg)
import Svg.Attributes exposing (..)
import Vector exposing (Vec, add, getX, getY, invertX, invertY, normalize, scale, sub, vec)


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


boardWidth =
    800


boardHeight =
    500


paddleWidth =
    20


paddleHeight =
    50


ballSize =
    5


paddleMovementFactor =
    -2.25



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


initModel : () -> Model
initModel _ =
    { player1 = { pos = vec ((boardWidth / -2) + paddleWidth / 2) 0, score = 0 }
    , player2 = { pos = vec ((boardWidth / 2) - paddleWidth / 2) 0, score = 0 }
    , ball =
        { pos = vec 0 0
        , dir = vec 2 1 |> normalize |> scale 2.5
        }
    , isRoundFinished = False
    , pressedKeys = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel (), Cmd.none )



-- UPDATE


type Msg
    = KeyMsg Keyboard.Msg
    | Tick


updatePlayerPos : (Vec -> Vec) -> Player -> Player
updatePlayerPos operation player =
    let
        dirSign =
            if getY player.pos > 0 then
                1

            else
                -1

        newPos =
            operation player.pos

        halfBoardHeight =
            boardHeight / 2

        halfPaddleHeight =
            paddleHeight / 2

        newPosCapped =
            if abs (getY newPos) + halfPaddleHeight > halfBoardHeight then
                vec (getX newPos) ((halfBoardHeight - halfPaddleHeight) * dirSign)

            else
                newPos
    in
    { player | pos = newPosCapped }


gameFinished : Ball -> Bool
gameFinished ball =
    abs (getX ball.pos) + (ballSize / 2) >= (boardWidth / 2)


detectPaddleColision : Ball -> Player -> Bool
detectPaddleColision ball player =
    let
        ballAbsX =
            abs (getX ball.pos)

        xExceeds =
            ballAbsX >= (boardWidth / 2) - paddleWidth

        ballTop =
            getY ball.pos + (ballSize / 2)

        ballBottom =
            getY ball.pos - (ballSize / 2)

        paddleTop =
            getY player.pos + (paddleHeight / 2)

        paddleBottom =
            getY player.pos - (paddleHeight / 2)

        yWithinPaddle =
            ballBottom >= paddleBottom && ballTop <= paddleTop
    in
    xExceeds && yWithinPaddle


updateBall : Ball -> Player -> Player -> Ball
updateBall ball player1 player2 =
    let
        collidesWithPlayer =
            detectPaddleColision ball

        collidesWithWall =
            abs (getY ball.pos) >= (boardHeight / 2)

        ballDir =
            if collidesWithWall then
                invertY ball.dir

            else if collidesWithPlayer player1 || collidesWithPlayer player2 then
                ball.dir
                    |> invertX
                    |> scale 1.1

            else
                ball.dir
    in
    { ball | dir = ballDir, pos = add ball.pos ballDir }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none )

        Tick ->
            if gameFinished model.ball then
                let
                    newModel =
                        initModel ()
                in
                ( newModel, Cmd.none )

            else
                let
                    arrows =
                        Keyboard.Arrows.arrows model.pressedKeys

                    wasd =
                        Keyboard.Arrows.wasd model.pressedKeys

                    newPlayer1 =
                        updatePlayerPos (add (vec 0 (paddleMovementFactor * toFloat wasd.y))) model.player1

                    newPlayer2 =
                        updatePlayerPos (add (vec 0 (paddleMovementFactor * toFloat arrows.y))) model.player2

                    newBall =
                        updateBall model.ball model.player1 model.player2
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


paddleView : Player -> String -> Html Msg
paddleView player fillC =
    rect
        [ width (String.fromInt paddleWidth)
        , height (String.fromInt paddleHeight)
        , x (String.fromFloat (getX player.pos))
        , y (String.fromFloat (getY player.pos))
        , fill fillC
        , style ("transform: translate(" ++ String.fromFloat (paddleWidth / -2) ++ "px, " ++ String.fromFloat (paddleHeight / -2) ++ "px);")
        ]
        []


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width (String.fromInt boardWidth)
            , height (String.fromInt boardHeight)
            , viewBox
                (String.concat
                    (List.map
                        (\a -> a ++ " ")
                        [ String.fromFloat (boardWidth / -2)
                        , String.fromFloat (boardHeight / -2)
                        , String.fromInt boardWidth
                        , String.fromInt boardHeight
                        ]
                    )
                )
            ]
            [ rect
                [ width (String.fromInt boardWidth)
                , height (String.fromInt boardHeight)
                , x (String.fromFloat (boardWidth / -2))
                , y (String.fromFloat (boardHeight / -2))
                ]
                []
            , circle
                [ fill "red"
                , cx (String.fromFloat (getX model.ball.pos))
                , cy (String.fromFloat (getY model.ball.pos))
                , r (String.fromFloat ballSize)
                ]
                []
            , paddleView model.player1 "red"
            , paddleView model.player2 "green"
            , Svg.text_
                [ fill "white"
                , x (String.fromFloat (boardWidth / -2))
                , y (String.fromFloat (boardHeight / -2))
                , style "transform: translate(8px,18px)"
                ]
                [ Svg.text (String.fromInt model.player1.score) ]
            , Svg.text_
                [ fill "white"
                ]
                [ Svg.text (String.fromInt model.player2.score) ]
            , line
                [ x1 "0"
                , y1 "0"
                , stroke "white"
                , x2 (String.fromFloat (getX model.ball.pos))
                , y2 (String.fromFloat (getY model.ball.pos))
                ]
                []
            , line
                [ x1 (String.fromFloat (getX model.ball.pos))
                , y1 (String.fromFloat (getY model.ball.pos))
                , stroke "yellow"
                , x2 (String.fromFloat (getX (add model.ball.pos model.ball.dir)))
                , y2 (String.fromFloat (getY (add model.ball.pos model.ball.dir)))
                ]
                []
            , line
                [ x1 "0"
                , y1 "0"
                , stroke "green"
                , x2 (String.fromFloat (getX model.player1.pos))
                , y2 (String.fromFloat (getY model.player1.pos))
                ]
                []
            , line
                [ x1 "0"
                , y1 "0"
                , stroke "blue"
                , x2 (String.fromFloat (getX model.player2.pos))
                , y2 (String.fromFloat (getY model.player2.pos))
                ]
                []
            ]
        , pre
            [ style "max-width: 560px; white-space: pre-wrap"
            ]
            [ Html.text (Debug.toString model) ]
        ]
