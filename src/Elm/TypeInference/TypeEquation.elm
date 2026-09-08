module Elm.TypeInference.TypeEquation exposing (TypeEquation, dropLabel, toString)

import Elm.TypeInference.Type as Type exposing (MonoType)


{-| Always mono types: schemes (foralls) live in environment and are
instantiated to mono types before equation is generated.
-}
type alias TypeEquation =
    ( MonoType, MonoType, String )


toString : TypeEquation -> String
toString ( t1, t2, source ) =
    [ t1, t2 ]
        |> List.map Type.monoTypeToString
        |> String.join " ≡ "
        |> (\eq -> eq ++ " (" ++ source ++ ")")


dropLabel : TypeEquation -> ( MonoType, MonoType )
dropLabel ( t1, t2, _ ) =
    ( t1, t2 )
