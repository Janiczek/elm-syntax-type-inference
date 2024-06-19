module Elm.TypeInference.State exposing
    ( TIState, State, init
    , pure, error, fromTuple, fromMaybe, run
    , map, map2, map3, andMap, mapError
    , do, andThen, traverse, combine
    , getNextIdAndTick
    , getVarTypes, getTypesForVar, addVarType
    , getTypeEnv, addBinding, removeBinding, addSubstitutions, existsInEnv, lookupEnv
    )

{-| State useful during various phases of the type inference algorithm.


# General

@docs TIState, State, init


# Utilities

@docs pure, error, fromTuple, fromMaybe, run
@docs map, map2, map3, andMap, mapError
@docs do, andThen, traverse, combine


# Next ID

@docs getNextIdAndTick


# Var types:

@docs getVarTypes, getTypesForVar, addVarType


# Type env: useful for let..in scoping etc.

@docs getTypeEnv, addBinding, removeBinding, addSubstitutions, existsInEnv, lookupEnv

-}

import AssocList
import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Elm.TypeInference.Type as Type exposing (Id, MonoType, Type(..))



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


fromMaybe : Error -> Maybe a -> TIState a
fromMaybe err maybe =
    case maybe of
        Nothing ->
            error err

        Just value ->
            pure value


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


modify : (State -> State) -> TIState ()
modify fn =
    \state -> ( Ok (), fn state )



-- OUR API


init : Dict VarName Type -> State
init env =
    { nextId = 0
    , varTypes = Dict.empty
    , typeEnv =
        -- When testing, you can populate this with types without having actual definitions present.
        env
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



-- TYPE ENV


getTypeEnv : TIState (Dict VarName Type)
getTypeEnv =
    get
        |> map .typeEnv


modifyTypeEnv : (Dict VarName Type -> Dict VarName Type) -> TIState ()
modifyTypeEnv fn =
    modify (\state -> { state | typeEnv = fn state.typeEnv })


addBinding : VarName -> Type -> TIState ()
addBinding var type_ =
    {- Diehl removes the key from the dict first... but I think we don't
       need to do that as on collision the new item wins.
    -}
    modifyTypeEnv (Dict.insert var type_)


removeBinding : VarName -> TIState ()
removeBinding var =
    modifyTypeEnv (Dict.remove var)


addSubstitutions : SubstitutionMap -> TIState ()
addSubstitutions subst =
    modifyTypeEnv (SubstitutionMap.substituteTypeEnv subst)


lookupEnv : FullModuleName -> VarName -> TIState MonoType
lookupEnv thisModule var =
    do getTypeEnv <| \env ->
    case Dict.get var env of
        Nothing ->
            let
                _ =
                    Debug.log "lookupEnv" ( var, env )
            in
            error <|
                VarNotFound
                    { usedIn = thisModule
                    , varName = var
                    }

        Just type_ ->
            instantiate type_


existsInEnv : VarName -> TIState Bool
existsInEnv varName =
    getTypeEnv
        |> map (Dict.member varName)


instantiate : Type -> TIState MonoType
instantiate (Forall boundVars monoType) =
    do (traverse (always getNextIdAndTick) boundVars) <| \varIds ->
    let
        subst : SubstitutionMap
        subst =
            List.map2 (\var id -> ( var, Type.id_ id ))
                boundVars
                varIds
                |> AssocList.fromList
    in
    SubstitutionMap.substituteMono subst monoType
        |> pure
