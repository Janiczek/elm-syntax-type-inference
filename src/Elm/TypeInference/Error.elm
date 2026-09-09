module Elm.TypeInference.Error exposing (Error(..), fromTypeAnnotationError)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Type exposing (FromTypeAnnotationError(..), MonoType, SuperType, TypeVar)
import Elm.Type


type Error
    = -- Syntax errors
      ImpossibleExpr (Node Expression)
    | ImpossiblePattern (Node Pattern)
    | ImpossibleType TypeAnnotation
    | ImpossibleDocsType Elm.Type.Type
    | MissingModuleName
      -- Var qualification errors
    | VarNotFound { usedIn : FullModuleName, varName : VarName }
    | AmbiguousName { usedIn : FullModuleName, varName : VarName, possibleModules : List FullModuleName }
    | AmbiguousModuleOwner { moduleName : String, possiblePackages : List String }
      -- Type errors
    | TypeMismatchMono MonoType MonoType
    | InfiniteType TypeVar MonoType
    | SuperTypeMismatch SuperType MonoType


fromTypeAnnotationError : FromTypeAnnotationError -> Error
fromTypeAnnotationError err =
    case err of
        ImpossibleAnnotation typeAnnotation ->
            ImpossibleType typeAnnotation

        AmbiguousModuleName ambiguity ->
            AmbiguousModuleOwner ambiguity
