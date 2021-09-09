module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Constants
import Html exposing (Html, div)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, viewBox, width, x, y)
import Vector exposing (Vec, add, getX, getY, invertY, normalize, scale, setY, sub, vec)


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
    }


type alias Model =
    { player1 : Player
    , player2 : Player
    , ball : Ball
    , pressedKeys : List Key
    }


initModel : () -> Model
initModel _ =
    let
        paddleMidY =
            (Constants.boardHeight / 2) - (Constants.paddleHeight / 2)

        ballMidX =
            (Constants.boardWidth / 2) - (Constants.ballSize / 2)

        ballMidY =
            (Constants.boardHeight / 2) - (Constants.ballSize / 2)
    in
    { player1 =
        { pos = vec 0 paddleMidY
        , score = 0
        , id = "lorem"
        }
    , player2 =
        { pos = vec (Constants.boardWidth - Constants.paddleWidth) paddleMidY
        , score = 0
        , id = "ipsum"
        }
    , ball =
        { pos = vec ballMidX ballMidY
        , dir = vec 2 1 |> normalize |> scale Constants.ballMovementFactor
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
            0

        max =
            Constants.boardHeight - Constants.paddleHeight

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
            ballX + Constants.ballSize < 0 || ballX > Constants.boardWidth
    in
    if finished then
        if ballX > 0 then
            Just model.player1.id

        else
            Just model.player2.id

    else
        Nothing


detectPaddleColision : Ball -> Player -> Bool
detectPaddleColision ball player =
    let
        ballX =
            getX ball.pos

        xHitsPaddle =
            ballX <= Constants.paddleWidth || ballX + Constants.ballSize >= Constants.boardWidth - Constants.paddleWidth

        ballTop =
            getY ball.pos

        ballBottom =
            getY ball.pos + Constants.ballSize

        paddleTop =
            getY player.pos

        paddleBottom =
            getY player.pos + Constants.paddleHeight

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
            vec (Constants.ballSize / 2) (Constants.ballSize / 2)

        paddleDiameter =
            vec (Constants.paddleWidth / 2) (Constants.paddleHeight / 2)

        ballCenter =
            add ballPos ballDiameter

        paddleCenter =
            add player.pos paddleDiameter

        newDir =
            sub paddleCenter ballCenter |> normalize |> scale Constants.ballMovementFactor
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
            ballY <= 0 || ballY + Constants.ballSize >= Constants.boardHeight

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
            model.player1 |> updatePlayerPos (\_ -> freshModel.player1.pos) |> updateIfScored

        newPlayer2 =
            model.player2 |> updatePlayerPos (\_ -> freshModel.player2.pos) |> updateIfScored
    in
    { model | player1 = newPlayer1, player2 = newPlayer2, ball = freshModel.ball }


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
            updatePlayerPos (add (vec 0 player1MovementY)) model.player1

        player2MovementY =
            Constants.paddleMovementFactor * toFloat arrows.y

        newPlayer2 =
            updatePlayerPos (add (vec 0 player2MovementY)) model.player2

        newBall =
            updateBall model.ball model.player1 model.player2
    in
    { model | player1 = newPlayer1, player2 = newPlayer2, ball = newBall }


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
            [ width (String.fromInt Constants.boardWidth)
            , height (String.fromInt Constants.boardHeight)
            , viewBox
                (join
                    [ 0
                    , 0
                    , Constants.boardWidth
                    , Constants.boardHeight
                    ]
                )
            ]
            [ renderBackground
            , renderBall model.ball.pos
            , renderPaddle model.player1.pos
            , renderPaddle model.player2.pos
            , renderScore 50 model.player1.score
            , renderScore (Constants.boardWidth - 50) model.player2.score
            ]
        ]


renderBackground =
    rect
        [ width (String.fromInt Constants.boardWidth)
        , height (String.fromInt Constants.boardHeight)
        , x (String.fromInt 0)
        , y (String.fromInt 0)
        ]
        []


renderBall : Vec -> Html Msg
renderBall pos =
    rect
        [ width (String.fromInt Constants.ballSize)
        , height (String.fromInt Constants.ballSize)
        , x (pos |> getX |> String.fromFloat)
        , y (pos |> getY |> String.fromFloat)
        , fill "red"
        ]
        []


renderPaddle : Vec -> Html Msg
renderPaddle pos =
    rect
        [ width (String.fromInt Constants.paddleWidth)
        , height (String.fromInt Constants.paddleHeight)
        , x (pos |> getX |> String.fromFloat)
        , y (pos |> getY |> String.fromFloat)
        , fill "white"
        ]
        []


renderScore : Float -> Int -> Html Msg
renderScore xCoord score =
    Svg.text_
        [ fill "white"
        , x (String.fromFloat xCoord)
        , y "50"
        ]
        [ Svg.text (String.fromInt score) ]
