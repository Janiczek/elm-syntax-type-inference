module Elm.TypeInference.State exposing
    ( TIState, State, init
    , pure, fromTuple, run, map, map2, andMap, mapError, do, traverse, combine
    , getNextIdAndTick
    , getVarTypes, getTypesForVar, addVarType
    , getIdTypes, getTypeForId, insertTypeForId
    , getTypeAliases, getTypeAlias
    , impossibleAstPattern, typeMismatch, occursCheckFailed
    )

{-| State useful during various phases of the type inference algorithm.

  - next ID: TODO write some more
  - var types: TODO write some more
  - id types: TODO write some more
  - type aliases: TODO write some more
  - type inference errors: TODO write some more


# General

@docs TIState, State, init


# Useful stuff

@docs pure, fromTuple, run, map, map2, andMap, mapError, do, traverse, combine


# Next ID

@docs getNextIdAndTick


# Var types

@docs getVarTypes, getTypesForVar, addVarType


# ID types

@docs getIdTypes, getTypeForId, insertTypeForId


# Type aliases

@docs getTypeAliases, getTypeAlias


# Errors

@docs impossibleAstPattern, typeMismatch, occursCheckFailed

-}

import Dict exposing (Dict)
import Elm.Syntax.ExpressionV2 exposing (TypedExpr)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.Type exposing (Id, Type, TypeOrId, TypeOrId_(..))



-- GENERAL


type alias State =
    { {- ID counter, making sure every expression gets its own unique ID
         number. As long as we only expose `getNextIdAndTick` as a way to get
         the ID, they'll automatically increment.
      -}
      nextId : Id
    , {- A dict from variable names to their types/type IDs.

         Usually one of these will be something of substance (type taken from
         the actual expression in the declaration of a var) and the rest will
         be just type IDs of its usages. That way we can link them together.
      -}
      varTypes : Dict ( ModuleName, VarName ) (List TypeOrId)
    , {- A dict from type variable IDs to inferred types.

         Note IDs can point to other IDs (eg. dict entry `(1,Id 2)`) so you
         might need to walk this dict multiple times.
      -}
      idTypes : Dict Id TypeOrId
    , {- All known type aliases and what they resolve to.

         Essentially read-only.

         TODO: in elm-in-elm we have `ConcreteType` which basically disallows IDs.
         Perhaps we should also use it here (make impossible states impossible)?
         Library API tradeoffs - too many types etc...
      -}
      typeAliases : Dict ( ModuleName, VarName ) Type
    }


type alias TIState a =
    State -> ( Result Error a, State )


pure : a -> TIState a
pure a =
    \s -> ( Ok a, s )


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


mapError : (Error -> Error) -> TIState a -> TIState a
mapError fn stateFn =
    \state ->
        stateFn state
            |> Tuple.mapFirst (Result.mapError fn)


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


init : Dict ( ModuleName, VarName ) Type -> State
init typeAliases =
    { nextId = 0
    , varTypes = Dict.empty
    , idTypes = Dict.empty
    , typeAliases = typeAliases
    }


tickId : TIState ()
tickId =
    modify (\state -> { state | nextId = state.nextId + 1 })



--elm-format-ignore-begin
getNextIdAndTick : TIState Id
getNextIdAndTick =
    do get <| \{ nextId } ->
    do tickId <| \() ->
    pure nextId
--elm-format-ignore-end
-- VAR TYPES


getVarTypes : TIState (Dict ( ModuleName, VarName ) (List TypeOrId))
getVarTypes =
    get
        |> map .varTypes


getTypesForVar : ModuleName -> VarName -> TIState (List TypeOrId)
getTypesForVar moduleName varName =
    getVarTypes
        |> map (Dict.get ( moduleName, varName ) >> Maybe.withDefault [])


addVarType : ModuleName -> VarName -> TypeOrId -> TIState ()
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



-- ID TYPES


getIdTypes : TIState (Dict Id TypeOrId)
getIdTypes =
    get
        |> map .idTypes


getTypeForId : Id -> TIState (Maybe TypeOrId)
getTypeForId id =
    getIdTypes
        |> map (Dict.get id)


insertTypeForId : Id -> TypeOrId -> TIState ()
insertTypeForId id typeOrId =
    do get <|
        \state ->
            case typeOrId of
                Id id_ ->
                    case Dict.get id_ state.idTypes of
                        Nothing ->
                            put { state | idTypes = Dict.insert id typeOrId state.idTypes }

                        Just another ->
                            insertTypeForId id another

                Type _ ->
                    put { state | idTypes = Dict.insert id typeOrId state.idTypes }



-- TYPE ALIASES


getTypeAliases : TIState (Dict ( ModuleName, VarName ) Type)
getTypeAliases =
    get
        |> map .typeAliases


getTypeAlias : ModuleName -> VarName -> TIState (Maybe Type)
getTypeAlias moduleName varName =
    getTypeAliases
        |> map (Dict.get ( moduleName, varName ))



-- ERRORS


impossibleAstPattern : TypedExpr -> TIState a
impossibleAstPattern expr =
    \state -> ( Err (ImpossibleAstPattern expr), state )


typeMismatch : TypeOrId -> TypeOrId -> TIState a
typeMismatch t1 t2 =
    \state -> ( Err (TypeMismatch t1 t2), state )


occursCheckFailed : Id -> TypeOrId -> TIState a
occursCheckFailed id type_ =
    \state -> ( Err (OccursCheckFailed id type_), state )
