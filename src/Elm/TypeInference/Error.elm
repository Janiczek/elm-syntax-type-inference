module Elm.TypeInference.Error exposing (TypeInferenceError(..))

import Elm.Qualifiedness exposing (Qualified)
import Elm.Type exposing (TypeOrId)


type TypeInferenceError
    = TypeMismatch (TypeOrId Qualified) (TypeOrId Qualified)
    | OccursCheckFailed Int (TypeOrId Qualified)
