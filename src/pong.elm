module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Html exposing (Html, div, pre)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Svg exposing (circle, rect, svg)
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
    , id : Int
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
    { player1 = { pos = vec 0 midY, score = 0, id = 1 }
    , player2 = { pos = vec (boardWidth - paddleWidth) midY, score = 0, id = 2 }
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


capPlayerPos : Vec -> Vec
capPlayerPos newPos =
    let
        min =
            0

        max =
            boardHeight - paddleHeight

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
updatePlayerPos operation player =
    let
        newPos =
            operation player.pos
    in
    { player | pos = capPlayerPos newPos }


isGameFinished : Model -> Maybe Int
isGameFinished model =
    let
        ballX =
            getX model.ball.pos

        finished =
            ballX + ballSize < 0 || ballX > boardWidth
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
            case isGameFinished model of
                Just scoredPlayerId ->
                    let
                        newModel =
                            initModel ()

                        updateIfScored : Player -> Int
                        updateIfScored player =
                            if scoredPlayerId == player.id then
                                player.score + 1

                            else
                                player.score

                        newPlayer1 =
                            { pos = newModel.player1.pos
                            , score = updateIfScored model.player1
                            , id = model.player1.id
                            }

                        newPlayer2 =
                            { pos = newModel.player2.pos
                            , score = updateIfScored model.player2
                            , id = model.player2.id
                            }
                    in
                    ( { newModel | player1 = newPlayer1, player2 = newPlayer2 }, Cmd.none )

                Nothing ->
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
            , renderScore "50" model.player1
            , renderScore (String.fromFloat (boardWidth - 50)) model.player2
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


renderScore : String -> Player -> Html Msg
renderScore xCoord player =
    Svg.text_
        [ fill "white"
        , x xCoord
        , y "50"
        ]
        [ Svg.text (String.fromInt player.score) ]
