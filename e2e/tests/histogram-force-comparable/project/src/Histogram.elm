module Histogram exposing (Threshold, sturges)


type alias Threshold comparable =
    comparable -> List comparable


sturges : Threshold Float
sturges x =
    [ toFloat (ceiling x) ]
