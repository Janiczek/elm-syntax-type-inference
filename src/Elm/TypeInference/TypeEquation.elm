module Elm.TypeInference.TypeEquation exposing (TypeEquation, dropLabel, toString)

import Elm.TypeInference.Type as Type exposing (Type)


type alias TypeEquation =
    ( Type, Type, String )


toString : TypeEquation -> String
toString ( t1, t2, source ) =
    [ t1, t2 ]
        |> List.map Type.toString
        |> String.join " ≡ "
        |> (\eq -> eq ++ " (" ++ source ++ ")")


dropLabel : TypeEquation -> ( Type, Type )
dropLabel ( t1, t2, _ ) =
    ( t1, t2 )
