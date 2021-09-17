module Elm.TypeInference.TodoSomehowUseThis exposing (..)

import AssocList
import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Elm.TypeInference.Type as Type
    exposing
        ( MonoType
        , Type(..)
        )
import Elm.TypeInference.TypeEquation exposing (TypeEquation)


type alias Env =
    Dict VarName Type


typeOf : VarName -> Env -> Maybe Type
typeOf =
    Dict.get


solveStuffTodo : Dict ( FullModuleName, VarName ) MonoType -> List TypeEquation -> MonoType -> Result Error Type
solveStuffTodo typeAliases equations type_ =
    -- TODO use this from within Elm.TypeInference
    case Debug.todo "runSolve typeAliases equations" of
        Err err ->
            Err err

        Ok substitutions ->
            Ok <| Type.closeOver <| SubstitutionMap.substituteMono substitutions type_


myOtherTodo =
    Debug.todo "look at Diehl Infer.infer, lam / let / ... and how he's using `inEnv`, `generalize` and `local`"
