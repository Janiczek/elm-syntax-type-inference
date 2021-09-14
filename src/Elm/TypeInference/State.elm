module Elm.TypeInference.State exposing
    ( TIState, State, init
    , pure, fromTuple, run, map, map2, map3, andMap, mapError, do, andThen, traverse, combine, error
    , findModuleOfVar
    , getNextIdAndTick
    , getVarTypes, getTypesForVar, addVarType
    , getTypeEnv, withBinding, withSubstitutions
    )

{-| State useful during various phases of the type inference algorithm.


# General

@docs TIState, State, init


# Utilities

@docs pure, fromTuple, run, map, map2, map3, andMap, mapError, do, andThen, traverse, combine, error


# Var module lookup

@docs findModuleOfVar


# Next ID

@docs getNextIdAndTick


# Var types

@docs getVarTypes, getTypesForVar, addVarType


# Type env

@docs getTypeEnv, withBinding, withSubstitutions

-}

import Dict exposing (Dict)
import Elm.Syntax.ExpressionV2 exposing (TypedExpr)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.File.Extra as File
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.PatternV2 exposing (TypedPattern)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.State.VarModuleLookup as VarModuleLookup
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Elm.TypeInference.Type exposing (Id, MonoType(..), Type(..), TypeVar)
import Elm.TypeInference.TypeEquation exposing (TypeEquation)



-- GENERAL


type alias State =
    { {- ID counter, making sure every expression gets its own unique ID
         number. As long as we only expose `getNextIdAndTick` as a way to get
         the ID, they'll automatically increment.
      -}
      nextId : Id
    , {- A dict from variable names to their types/type IDs.

         This is mainly important because we allow any order of declarations vs
         their usages. We can't really look stuff up in the environment as we
         find the usages; we need to defer that until later.

         Usually one of these IDs will be something of substance (type taken
         from the actual expression in the declaration of a var) and the rest
         will be just type IDs of its usages. That way we can link them
         together.
      -}
      varTypes : Dict ( FullModuleName, VarName ) (List Type)
    , {- Environment holding types for our various variables and bindings.

         This is not global state, as it will change and flow as we go in and out
         of let..in expressions and lambdas etc.
      -}
      typeEnv : Dict VarName Type
    }


type alias TIState a =
    State -> ( Result Error a, State )


pure : a -> TIState a
pure a =
    \s -> ( Ok a, s )


error : Error -> TIState a
error error_ =
    \s -> ( Err error_, s )


fromTuple : ( Result Error a, State ) -> TIState a
fromTuple tuple =
    \_ -> tuple


run : State -> TIState a -> ( Result Error a, State )
run state stateFn =
    stateFn state


map : (a -> b) -> TIState a -> TIState b
map userFn stateFn =
    \state ->
        stateFn state
            |> Tuple.mapFirst (Result.map userFn)


mapError : (State -> Error -> Error) -> TIState a -> TIState a
mapError fn stateFn =
    \state ->
        let
            ( result, newState ) =
                stateFn state
        in
        ( Result.mapError (fn newState) result
        , newState
        )


andMap : TIState a -> TIState (a -> b) -> TIState b
andMap aM fnM =
    \state ->
        let
            ( fnResult, fnState ) =
                fnM state
        in
        case fnResult of
            Err errFn ->
                ( Err errFn, fnState )

            Ok fn ->
                let
                    ( aResult, aState ) =
                        aM fnState
                in
                ( Result.map fn aResult, aState )


map2 : (a -> b -> c) -> TIState a -> TIState b -> TIState c
map2 userFn aM bM =
    pure userFn
        |> andMap aM
        |> andMap bM


map3 : (a -> b -> c -> d) -> TIState a -> TIState b -> TIState c -> TIState d
map3 userFn aM bM cM =
    pure userFn
        |> andMap aM
        |> andMap bM
        |> andMap cM


andThen : (a -> TIState b) -> TIState a -> TIState b
andThen userFn stateFn =
    \state ->
        let
            ( result, nextState ) =
                stateFn state
        in
        case result of
            Err err ->
                ( Err err, nextState )

            Ok a ->
                userFn a nextState


do : TIState a -> (a -> TIState b) -> TIState b
do m fn =
    andThen fn m


traverse : (a -> TIState b) -> List a -> TIState (List b)
traverse f list =
    combine (List.map f list)


combine : List (TIState a) -> TIState (List a)
combine list =
    List.foldr
        (map2 (::))
        (pure [])
        list


get : TIState State
get =
    \state -> ( Ok state, state )


put : State -> TIState ()
put state =
    \_ -> ( Ok (), state )


modify : (State -> State) -> TIState ()
modify fn =
    \state -> ( Ok (), fn state )



-- OUR API


init : Dict VarName Type -> State
init env =
    { nextId = 0
    , varTypes = Dict.empty
    , typeEnv = env
    }


tickId : TIState ()
tickId =
    modify (\state -> { state | nextId = state.nextId + 1 })


getNextIdAndTick : TIState Id
getNextIdAndTick =
    do get <| \{ nextId } ->
    do tickId <| \() ->
    pure nextId



-- VAR TYPES


getVarTypes : TIState (Dict ( FullModuleName, VarName ) (List Type))
getVarTypes =
    get
        |> map .varTypes


getTypesForVar : FullModuleName -> VarName -> TIState (List Type)
getTypesForVar moduleName varName =
    getVarTypes
        |> map (Dict.get ( moduleName, varName ) >> Maybe.withDefault [])


addVarType : FullModuleName -> VarName -> Type -> TIState ()
addVarType moduleName varName type_ =
    modify
        (\state ->
            { state
                | varTypes =
                    state.varTypes
                        |> Dict.update ( moduleName, varName )
                            (\maybeTypes ->
                                case maybeTypes of
                                    Nothing ->
                                        Just [ type_ ]

                                    Just types ->
                                        Just (type_ :: types)
                            )
            }
        )



-- VAR QUALIFICATION


{-| We have roughly these options:

  - bar = >baz< (baz being defined elsewhere in this module)
  - import Foo exposing (baz); bar = >baz<
  - import Foo; bar = >Foo.baz<
  - import Foo as F; bar = >F.baz<

In all these cases we need to find the full unaliased module name of the var.

-}
findModuleOfVar :
    Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> TIState FullModuleName
findModuleOfVar files thisFile maybeModuleName varName =
    let
        orElseLazy : (() -> Result Error (Maybe FullModuleName)) -> Result Error (Maybe FullModuleName) -> Result Error (Maybe FullModuleName)
        orElseLazy after before =
            case before of
                Err err ->
                    Err err

                Ok (Just name) ->
                    Ok (Just name)

                Ok Nothing ->
                    after ()

        foundModuleName : Result Error (Maybe FullModuleName)
        foundModuleName =
            VarModuleLookup.unqualifiedVarInThisModule thisFile maybeModuleName varName
                |> orElseLazy (\() -> VarModuleLookup.unqualifiedVarInImportedModule files thisFile maybeModuleName varName)
                |> orElseLazy (\() -> VarModuleLookup.qualifiedVarInImportedModule files maybeModuleName varName)
                |> orElseLazy (\() -> VarModuleLookup.qualifiedVarInAliasedModule files thisFile maybeModuleName varName)
    in
    case foundModuleName of
        Err err ->
            error err

        Ok Nothing ->
            error <|
                VarNotFound
                    { varName = varName
                    , usedIn = File.moduleName thisFile
                    }

        Ok (Just moduleName) ->
            pure moduleName



-- TYPE ENV


getTypeEnv : TIState (Dict VarName Type)
getTypeEnv =
    get
        |> map .typeEnv


withBinding : VarName -> Type -> TIState a -> TIState a
withBinding var type_ m =
    withModifiedTypeEnv
        {- Diehl removes the key from the dict first... but I think we don't
           need to do that as on collision the new item wins.
        -}
        (Dict.insert var type_)
        m


withSubstitutions : SubstitutionMap -> TIState a -> TIState a
withSubstitutions subst m =
    withModifiedTypeEnv
        (SubstitutionMap.substituteTypeEnv subst)
        m


withModifiedTypeEnv : (Dict VarName Type -> Dict VarName Type) -> TIState a -> TIState a
withModifiedTypeEnv fn stateFn =
    \state -> stateFn { state | typeEnv = fn state.typeEnv }
