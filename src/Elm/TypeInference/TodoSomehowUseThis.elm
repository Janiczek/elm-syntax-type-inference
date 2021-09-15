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


lookupEnv : FullModuleName -> VarName -> TIState MonoType
lookupEnv thisModule var =
    -- TODO findModuleOfVar also?
    State.do State.getTypeEnv <| \env ->
    case Dict.get var env of
        Nothing ->
            State.error <|
                VarNotFound
                    { usedIn = thisModule
                    , varName = var
                    }

        Just type_ ->
            instantiate type_


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


instantiate : Type -> TIState MonoType
instantiate (Forall boundVars monoType) =
    State.do (State.traverse (always State.getNextIdAndTick) boundVars) <| \varIds ->
    let
        subst : SubstitutionMap
        subst =
            List.map2 (\var id -> ( var, Type.id_ id ))
                boundVars
                varIds
                |> AssocList.fromList
    in
    SubstitutionMap.substituteMono subst monoType
        |> State.pure
