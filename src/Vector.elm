module Vector exposing (Vec, add, getX, getY, invertX, invertY, normalize, scale, setX, setY, sub, vec)


type alias Vec =
    { x : Float
    , y : Float
    }


vec : Float -> Float -> Vec
vec x y =
    { x = x, y = y }


getX : Vec -> Float
getX v =
    v.x


setX : Float -> Vec -> Vec
setX x a =
    { a | x = x }


getY : Vec -> Float
getY a =
    a.y


setY : Float -> Vec -> Vec
setY y a =
    { a | y = y }


add : Vec -> Vec -> Vec
add b a =
    { x = a.x + b.x, y = a.y + b.y }


sub : Vec -> Vec -> Vec
sub b a =
    { x = a.x - b.x, y = a.y - b.y }


scale : Float -> Vec -> Vec
scale k a =
    { x = a.x * k, y = a.y * k }


invertX : Vec -> Vec
invertX a =
    { x = a.x * -1, y = a.y }


invertY : Vec -> Vec
invertY a =
    { x = a.x, y = a.y * -1 }


normalize : Vec -> Vec
normalize a =
    let
        x =
            getX a

        y =
            getY a

        s =
            sqrt ((x * x) + (y * y))
    in
    { x = x / s, y = y / s }
