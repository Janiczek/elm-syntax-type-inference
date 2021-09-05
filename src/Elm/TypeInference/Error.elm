module Elm.TypeInference.Error exposing (Error(..))

import Elm.Syntax.ExpressionV2 exposing (TypedExpr)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Type exposing (TypeOrId)


type Error
    = -- Syntax errors
      ImpossibleAstPattern TypedExpr
    | ImpossibleTypePattern TypeAnnotation
      -- Var qualification errors
    | VarNotFound { usedIn : FullModuleName, varName : VarName }
    | AmbiguousName { usedIn : FullModuleName, varName : VarName, possibleModules : List FullModuleName }
      -- Type errors
    | TypeMismatch TypeOrId TypeOrId
    | OccursCheckFailed Int TypeOrId
