module Elm.TypeInference.TypeEquation exposing (TypeEquation, dropLabel)

import Elm.TypeInference.Type.Internal exposing (MonoType)


{-| Always mono types: schemes (foralls) live in environment and are
instantiated to mono types before equation is generated.
-}
type alias TypeEquation =
    ( MonoType, MonoType, String )


dropLabel : TypeEquation -> ( MonoType, MonoType )
dropLabel ( t1, t2, _ ) =
    ( t1, t2 )
