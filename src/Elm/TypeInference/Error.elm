module Elm.TypeInference.Error exposing (Error(..))

import Elm.Syntax.ExpressionV2 exposing (TypedExpr)
import Elm.TypeInference.Type exposing (TypeOrId)


type Error
    = TypeMismatch TypeOrId TypeOrId
    | OccursCheckFailed Int TypeOrId
    | ImpossibleAstPattern TypedExpr
