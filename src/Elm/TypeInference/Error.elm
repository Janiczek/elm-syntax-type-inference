module Elm.TypeInference.Error exposing (Error(..))

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Type exposing (MonoType, SuperType, TypeVar)


type Error
    = -- Syntax errors
      ImpossibleExpr (Node Expression)
    | ImpossiblePattern (Node Pattern)
    | ImpossibleType TypeAnnotation
    | MissingModuleName
      -- Var qualification errors
    | VarNotFound { usedIn : FullModuleName, varName : VarName }
    | AmbiguousName { usedIn : FullModuleName, varName : VarName, possibleModules : List FullModuleName }
      -- Type errors
    | TypeMismatchMono MonoType MonoType
    | InfiniteType TypeVar MonoType
    | SuperTypeMismatch SuperType MonoType
