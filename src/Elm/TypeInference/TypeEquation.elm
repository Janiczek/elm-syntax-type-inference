module Elm.TypeInference.TypeEquation exposing (TypeEquation)

import Elm.TypeInference.Type exposing (TypeOrId)


type alias TypeEquation =
    ( TypeOrId, TypeOrId )
