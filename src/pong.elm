module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Html exposing (Html, div, pre)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Svg exposing (circle, line, rect, svg)
import Svg.Attributes exposing (..)
import Vector exposing (Vec, add, getX, getY, invertX, invertY, normalize, scale, setY, sub, vec)


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
    10


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


midY =
    (boardHeight / 2) - (paddleHeight / 2)


initModel : () -> Model
initModel _ =
    { player1 = { pos = vec 0 midY, score = 0 }
    , player2 = { pos = vec (boardWidth - paddleWidth) midY, score = 0 }
    , ball =
        { pos = vec ((boardWidth / 2) - (ballSize / 2)) ((boardHeight / 2) - (ballSize / 2))
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
            if getY newPos <= 0 then
                setY 0 newPos

            else if getY newPos + paddleHeight >= boardHeight then
                setY (boardHeight - paddleHeight) newPos

            else
                newPos
    in
    { player | pos = newPosCapped }


gameFinished : Ball -> Bool
gameFinished ball =
    let
        ballX =
            getX ball.pos
    in
    ballX + ballSize < 0 || ballX > boardWidth


detectPaddleColision : Ball -> Player -> Bool
detectPaddleColision ball player =
    let
        ballX =
            getX ball.pos

        xHitsPaddle =
            ballX <= paddleWidth || ballX + ballSize >= boardWidth - paddleWidth

        ballTop =
            getY ball.pos

        ballBottom =
            getY ball.pos + ballSize

        paddleTop =
            getY player.pos

        paddleBottom =
            getY player.pos + paddleHeight

        yWithinPaddle =
            ballBottom >= paddleTop && ballTop <= paddleBottom
    in
    xHitsPaddle && yWithinPaddle


reflectAgainstPlayer : Ball -> Player -> Vec
reflectAgainstPlayer ball player =
    let
        ballPos =
            ball.pos

        ballDiameter =
            vec (ballSize / 2) (ballSize / 2)

        paddleDiameter =
            vec (paddleWidth / 2) (paddleHeight / 2)

        ballCenter =
            add ballPos ballDiameter

        paddleCenter =
            add player.pos paddleDiameter

        newDir =
            sub paddleCenter ballCenter |> normalize |> scale 2.5
    in
    newDir


updateBall : Ball -> Player -> Player -> Ball
updateBall ball player1 player2 =
    let
        collidesWithPlayer =
            detectPaddleColision ball

        ballY =
            getY ball.pos

        collidesWithWall =
            ballY <= 0 || ballY + ballSize >= boardHeight

        ballDir =
            if collidesWithWall then
                invertY ball.dir

            else if collidesWithPlayer player1 then
                reflectAgainstPlayer ball player1

            else if collidesWithPlayer player2 then
                reflectAgainstPlayer ball player2

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


join : List Int -> String
join list =
    String.concat
        (List.map
            (\a -> String.fromInt a ++ " ")
            list
        )


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width (String.fromInt boardWidth)
            , height (String.fromInt boardHeight)
            , viewBox
                (join
                    [ 0
                    , 0
                    , boardWidth
                    , boardHeight
                    ]
                )
            ]
            [ renderBackground
            , renderBall model.ball.pos
            , renderPaddle model.player1
            , renderPaddle model.player2
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
            ]
        ]


renderBackground =
    rect
        [ width (String.fromInt boardWidth)
        , height (String.fromInt boardHeight)
        , x (String.fromInt 0)
        , y (String.fromInt 0)
        ]
        []


renderBall : Vec -> Html Msg
renderBall pos =
    rect
        [ width (String.fromInt ballSize)
        , height (String.fromInt ballSize)
        , x (String.fromFloat (getX pos))
        , y (String.fromFloat (getY pos))
        , fill "red"
        ]
        []


renderPaddle : Player -> Html Msg
renderPaddle player =
    rect
        [ width (String.fromInt paddleWidth)
        , height (String.fromInt paddleHeight)
        , x (String.fromFloat (getX player.pos))
        , y (String.fromFloat (getY player.pos))
        , fill "white"
        ]
        []
