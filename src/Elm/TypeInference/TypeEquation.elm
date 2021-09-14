module Elm.TypeInference.TypeEquation exposing (TypeEquation, toString)

import Elm.TypeInference.Type as Type exposing (Type)


type alias TypeEquation =
    ( Type, Type )


toString : TypeEquation -> String
toString ( t1, t2 ) =
    [ t1, t2 ]
        |> List.map Type.toString
        |> String.join " ≡ "
