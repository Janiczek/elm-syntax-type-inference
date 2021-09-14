module Elm.TypeInference.Error exposing (Error(..))

import Elm.Syntax.ExpressionV2 exposing (TypedExpr)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.PatternV2 exposing (TypedPattern)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Type exposing (MonoType, SuperType, Type, TypeVar)


type Error
    = -- Syntax errors
      ImpossibleExpr TypedExpr
    | ImpossiblePattern TypedPattern
    | ImpossibleType TypeAnnotation
      -- Var qualification errors
    | VarNotFound { usedIn : FullModuleName, varName : VarName }
    | AmbiguousName { usedIn : FullModuleName, varName : VarName, possibleModules : List FullModuleName }
      -- Type errors
    | TypeMismatch Type Type
    | TypeMismatchMono MonoType MonoType
    | InfiniteType TypeVar MonoType
    | SuperTypeMismatch SuperType MonoType
