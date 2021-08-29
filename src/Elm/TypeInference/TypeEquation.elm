module Elm.TypeInference.TypeEquation exposing (TypeEquation, equals)

import Elm.TypeInference.Type exposing (TypeOrId)


type alias TypeEquation =
    ( TypeOrId, TypeOrId )


equals : TypeOrId -> TypeOrId -> TypeEquation
equals t1 t2 =
    ( t1, t2 )
