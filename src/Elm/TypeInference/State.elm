module Elm.TypeInference.State exposing
    ( TIState, State, initState
    , pure, map, map2, andMap, do, traverse, combine
    , getNextIdAndTick
    , addVarType
    , impossibleAstPattern
    )

{-| State useful during various phases of the type inference algorithm.

  - next ID: TODO write some more
  - var types: TODO write some more
  - type inference errors: TODO write some more


# General

@docs TIState, State, initState


# Useful stuff

@docs pure, map, map2, andMap, do, traverse, combine


# Next ID

@docs getNextIdAndTick


# Var types

@docs addVarType


# Errors

@docs impossibleAstPattern

-}

import Dict exposing (Dict)
import Elm.Syntax.ExpressionV2 exposing (TypedExpr)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.Type exposing (Id, TypeOrId)



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
    }


type alias TIState a =
    State -> ( Result Error a, State )


pure : a -> TIState a
pure a =
    \s -> ( Ok a, s )


map : (a -> b) -> TIState a -> TIState b
map userFn stateFn =
    \state ->
        stateFn state
            |> Tuple.mapFirst (Result.map userFn)


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


modify : (State -> State) -> TIState ()
modify fn =
    \state -> ( Ok (), fn state )



-- OUR API


initState : State
initState =
    { nextId = 0
    , varTypes = Dict.empty
    }


tickId : TIState ()
tickId =
    modify (\state -> { state | nextId = state.nextId + 1 })


getNextId : TIState Id
getNextId =
    get |> map .nextId


getNextIdAndTick : TIState Id
getNextIdAndTick =
    do get <|
        \{ nextId } ->
            do tickId <|
                \() ->
                    pure nextId



-- VAR TYPES


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



-- ERRORS


impossibleAstPattern : TypedExpr -> TIState a
impossibleAstPattern expr =
    \state -> ( Err (ImpossibleAstPattern expr), state )
