module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Color
import Constants
import Game.TwoD as Game
import Game.TwoD.Camera as Camera
import Game.TwoD.Render as Render exposing (Renderable, circle, rectangle)
import Html exposing (Html, div, text)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Vector exposing (Vec, add, getX, getY, invertX, invertY, scale, setY, vec)


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Player =
    { pos : Vec
    , score : Int
    , id : String
    }


type alias Ball =
    { pos : Vec
    , dir : Vec
    , radius : Float
    }


type alias Model =
    { leftPlayer : Player
    , rightPlayer : Player
    , ball : Ball
    , pressedKeys : List Key
    }


initModel : () -> Model
initModel _ =
    let
        player1Pos =
            vec (Constants.boardWidth / -2) 0

        player2Pos =
            vec (Constants.boardWidth / 2) 0
    in
    { leftPlayer =
        { pos = player1Pos
        , score = 0
        , id = "LEFT"
        }
    , rightPlayer =
        { pos = player2Pos
        , score = 0
        , id = "RIGHT"
        }
    , ball =
        { pos = vec 0 0
        , dir = vec 3 1 |> scale Constants.ballMovementFactor
        , radius = Constants.ballSize / 2
        }
    , pressedKeys = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel (), Cmd.none )



-- UPDATE


type Msg
    = KeyMsg Keyboard.Msg
    | Tick


capPlayerPos : Vec -> Vec
capPlayerPos newPos =
    let
        min =
            Constants.boardHeight / -2

        max =
            Constants.boardHeight / 2 - Constants.paddleHeight

        y =
            getY newPos
    in
    if y <= min then
        setY min newPos

    else if y >= max then
        setY max newPos

    else
        newPos


updatePlayerPos : (Vec -> Vec) -> Player -> Player
updatePlayerPos transformation player =
    let
        newPos =
            transformation player.pos
    in
    { player | pos = capPlayerPos newPos }


isGameFinished : Model -> Maybe String
isGameFinished model =
    let
        ballX =
            getX model.ball.pos

        finished =
            abs ballX + model.ball.radius * 2 >= Constants.boardWidth / 2
    in
    if finished then
        if ballX > 0 then
            Just model.leftPlayer.id

        else
            Just model.rightPlayer.id

    else
        Nothing


detectPaddleColision : Ball -> Player -> Bool
detectPaddleColision ball player =
    let
        xHitsPaddle =
            abs (getX ball.pos) + ball.radius >= abs (getX player.pos) - Constants.paddleWidth

        ballTop =
            getY ball.pos + (ball.radius / 2)

        ballBottom =
            getY ball.pos - (ball.radius / 2)

        paddleTop =
            getY player.pos + (Constants.paddleHeight / 2)

        paddleBottom =
            getY player.pos - (Constants.paddleHeight / 2)

        yWithinPaddle =
            ballBottom > paddleBottom && ballTop < paddleTop
    in
    xHitsPaddle && yWithinPaddle


reflectAgainstPlayer : Ball -> Player -> Vec
reflectAgainstPlayer ball _ =
    let
        newDir =
            invertX ball.dir |> scale Constants.ballMovementFactor
    in
    newDir


updateBall : Model -> Ball
updateBall model =
    let
        ball =
            model.ball

        collidesWithPlayer =
            detectPaddleColision ball

        ballY =
            getY ball.pos

        collidesWithWall =
            (ballY + (Constants.ballSize / 2)) >= Constants.boardHeight / 2 || ballY <= Constants.boardHeight / -2

        ballDir =
            if collidesWithWall then
                invertY ball.dir

            else if getX ball.pos < 0 && collidesWithPlayer model.leftPlayer then
                reflectAgainstPlayer ball model.leftPlayer

            else if getX ball.pos > 0 && collidesWithPlayer model.rightPlayer then
                reflectAgainstPlayer ball model.rightPlayer

            else
                ball.dir
    in
    { ball | dir = ballDir, pos = add ball.pos ballDir }


finishRound : String -> Model -> Model
finishRound scoredPlayerId model =
    let
        freshModel =
            initModel ()

        updateIfScored player =
            if scoredPlayerId == player.id then
                { player | score = player.score + 1 }

            else
                player

        newPlayer1 =
            model.leftPlayer |> updatePlayerPos (\_ -> freshModel.leftPlayer.pos) |> updateIfScored

        newPlayer2 =
            model.rightPlayer |> updatePlayerPos (\_ -> freshModel.rightPlayer.pos) |> updateIfScored
    in
    { model | leftPlayer = newPlayer1, rightPlayer = newPlayer2, ball = freshModel.ball }


updatePositions : Model -> Model
updatePositions model =
    let
        arrows =
            Keyboard.Arrows.arrows model.pressedKeys

        wasd =
            Keyboard.Arrows.wasd model.pressedKeys

        player1MovementY =
            Constants.paddleMovementFactor * toFloat wasd.y

        newPlayer1 =
            updatePlayerPos (add (vec 0 player1MovementY)) model.leftPlayer

        player2MovementY =
            Constants.paddleMovementFactor * toFloat arrows.y

        newPlayer2 =
            updatePlayerPos (add (vec 0 player2MovementY)) model.rightPlayer

        newBall =
            updateBall model
    in
    { model | leftPlayer = newPlayer1, rightPlayer = newPlayer2, ball = newBall }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none )

        Tick ->
            case isGameFinished model of
                Just scoredPlayerId ->
                    ( finishRound scoredPlayerId model, Cmd.none )

                Nothing ->
                    ( updatePositions model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        , Browser.Events.onAnimationFrame (\_ -> Tick)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text (String.fromInt model.leftPlayer.score)
        , text (String.fromInt model.rightPlayer.score)
        , Game.renderCentered { time = 0, camera = Camera.fixedArea (Constants.boardWidth * Constants.boardHeight) ( 0, 0 ), size = ( Constants.boardWidth, Constants.boardHeight ) }
            [ renderBall model.ball
            , renderPaddle model.leftPlayer.pos
            , renderPaddle model.rightPlayer.pos
            , Render.shape rectangle { color = Color.rgb 0.2 0.2 0.2, position = ( Constants.boardWidth / -2, 0 ), size = ( Constants.boardWidth, 1 ) }
            , Render.shape rectangle { color = Color.rgb 0.2 0.2 0.2, position = ( 0, Constants.boardHeight / -2 ), size = ( 1, Constants.boardWidth ) }
            , renderBackground
            ]
        ]



-- view : Model -> Html Msg
-- view model =
--     div []
--         [ svg
--             [ width (String.fromInt Constants.boardWidth)
--             , height (String.fromInt Constants.boardHeight)
--             , viewBox
--                 (join
--                     [ 0
--                     , 0
--                     , Constants.boardWidth
--                     , Constants.boardHeight
--                     ]
--                 )
--             ]
--             [
--             , renderScore 50 model.player1.score
--             , renderScore (Constants.boardWidth - 50) model.player2.score
--             ]
--         ]


renderBackground =
    Render.shape rectangle { color = Color.black, position = ( Constants.boardWidth / -2, Constants.boardHeight / -2 ), size = ( Constants.boardWidth, Constants.boardHeight ) }


renderBall : Ball -> Renderable
renderBall ball =
    Render.shape circle
        { color = Color.red
        , position = ( getX ball.pos - ball.radius, getY ball.pos - ball.radius )
        , size = ( ball.radius * 2, ball.radius * 2 )
        }


renderPaddle : Vec -> Renderable
renderPaddle pos =
    let
        x =
            if pos.x > 0 then
                pos.x - Constants.paddleWidth

            else
                pos.x
    in
    Render.shape rectangle
        { color = Color.white
        , position = ( x, pos.y - Constants.paddleHeight / 2 )
        , size = ( Constants.paddleWidth, Constants.paddleHeight )
        }



--
-- renderScore : Float -> Int -> Html Msg
-- renderScore xCoord score =
--     Svg.text_
--         [ fill "white"
--         , x (String.fromFloat xCoord)
--         , y "50"
--         ]
--         [ Svg.text (String.fromInt score) ]
