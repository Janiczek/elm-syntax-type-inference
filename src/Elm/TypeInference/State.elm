module Elm.TypeInference.State exposing
    ( TIState, State, init
    , pure, fromTuple, run, map, map2, map3, andMap, mapError, do, andThen, traverse, combine
    , findModuleOfVar
    , getNextIdAndTick
    , getVarTypes, getTypesForVar, addVarType
    , getIdTypes, getTypeForId, insertTypeForId
    , getTypeAliases, getTypeAlias
    , impossibleAstPattern, impossibleTypePattern, typeMismatch, occursCheckFailed, varNotFound, ambiguousName
    )

{-| State useful during various phases of the type inference algorithm.

  - next ID: TODO write some more
  - var types: TODO write some more
  - id types: TODO write some more
  - type aliases: TODO write some more
  - type inference errors: TODO write some more


# General

@docs TIState, State, init


# Utilities

@docs pure, fromTuple, run, map, map2, map3, andMap, mapError, do, andThen, traverse, combine


# Var module lookup

@docs findModuleOfVar


# Next ID

@docs getNextIdAndTick


# Var types

@docs getVarTypes, getTypesForVar, addVarType


# ID types

@docs getIdTypes, getTypeForId, insertTypeForId


# Type aliases

@docs getTypeAliases, getTypeAlias


# Errors

@docs impossibleAstPattern, impossibleTypePattern, typeMismatch, occursCheckFailed, varNotFound, ambiguousName

-}

import Dict exposing (Dict)
import Elm.Syntax.ExpressionV2 exposing (TypedExpr)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.File.Extra as File
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.State.VarModuleLookup as VarModuleLookup
import Elm.TypeInference.Type exposing (Id, Type, TypeOrId(..))



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

         Read-only.

         TODO: in elm-in-elm we have `ConcreteType` which basically disallows IDs.
         Perhaps we should also use it here (make impossible states impossible)?
         Library API tradeoffs - too many types etc...
      -}
      typeAliases : Dict ( FullModuleName, VarName ) Type
    }


type alias TIState a =
    State -> ( Result Error a, State )


pure : a -> TIState a
pure a =
    \s -> ( Ok a, s )


fromError : Error -> TIState a
fromError error =
    \s -> ( Err error, s )


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


init : Dict ( FullModuleName, VarName ) Type -> State
init typeAliases =
    { nextId = 0
    , varTypes = Dict.empty
    , idTypes = Dict.empty
    , typeAliases = typeAliases
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
    do get <| \state ->
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


getTypeAliases : TIState (Dict ( FullModuleName, VarName ) Type)
getTypeAliases =
    get
        |> map .typeAliases


getTypeAlias : FullModuleName -> VarName -> TIState (Maybe Type)
getTypeAlias moduleName varName =
    getTypeAliases
        |> map (Dict.get ( moduleName, varName ))



-- ERRORS


impossibleAstPattern : TypedExpr -> TIState a
impossibleAstPattern expr =
    fromError <| ImpossibleAstPattern expr


impossibleTypePattern : TypeAnnotation -> TIState a
impossibleTypePattern type_ =
    fromError <| ImpossibleTypePattern type_


typeMismatch : TypeOrId -> TypeOrId -> TIState a
typeMismatch t1 t2 =
    fromError <| TypeMismatch t1 t2


occursCheckFailed : Id -> TypeOrId -> TIState a
occursCheckFailed id type_ =
    fromError <| OccursCheckFailed id type_


varNotFound : { varName : VarName, usedIn : FullModuleName } -> TIState a
varNotFound rec =
    fromError <| VarNotFound rec


ambiguousName :
    { varName : VarName
    , usedIn : FullModuleName
    , possibleModules : List FullModuleName
    }
    -> TIState a
ambiguousName rec =
    fromError <| AmbiguousName rec


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
            fromError err

        Ok Nothing ->
            varNotFound
                { varName = varName
                , usedIn = File.moduleName thisFile
                }

        Ok (Just moduleName) ->
            pure moduleName
