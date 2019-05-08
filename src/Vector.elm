module Vector exposing (Vec, add, getX, getY, scale, setX, setY, sub, vec)


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
