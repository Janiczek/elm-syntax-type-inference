module XY exposing (XY, add, fromInts, sub)


type alias XY =
    ( Int, Int )


fromInts : Int -> Int -> XY
fromInts x y =
    ( x, y )


add : XY -> XY -> XY
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


sub : XY -> XY -> XY
sub ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )
