module Elm.TypeInference.State exposing
    ( TIState, State, PackageName, GlobalKey, init
    , pure, error, fromTuple, fromMaybe, run
    , map, map2, map3, andMap, mapError
    , do, andThen, traverse, combine
    , getNextIdAndTick
    , getNodeIds, idForNode, aliasNodeId
    , getSubst, composeSubst
    , getLexicalEnv, addBinding, existsInEnv, lookupEnv, withScopedEnv
    , getGlobalEnv, addGlobalBinding, lookupGlobalEnv
    , generalize
    )

{-| State useful during various phases of the type inference algorithm.


# General

@docs TIState, State, PackageName, GlobalKey, init


# Utilities

@docs pure, error, fromTuple, fromMaybe, run
@docs map, map2, map3, andMap, mapError
@docs do, andThen, traverse, combine


# Next ID

@docs getNextIdAndTick


# Node IDs: the `Range -> Id` dict we're building up

@docs getNodeIds, idForNode, aliasNodeId


# The accumulated solution

@docs getSubst, composeSubst


# Lexical env: scoping (lambda args, let..in, case branches)

@docs getLexicalEnv, addBinding, existsInEnv, lookupEnv, withScopedEnv


# Global env: top-level declarations, constructors, ports, (later) dependencies

@docs getGlobalEnv, addGlobalBinding, lookupGlobalEnv


# Generalization

@docs generalize

-}

import AssocList
import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Elm.TypeInference.Type as Type exposing (Id, MonoType, Type(..))
import RangeLike exposing (RangeLike)



-- GENERAL


{-| `""` for the first-party project being inferred,
package name ("foo/bar") for deps from `docs.json` (TODO)
-}
type alias PackageName =
    String


{-| Key into `globalEnv`: we need to qualify by package as well because of
situations where full qualified module names are not unique (eg.
`stil4m/elm-syntax` has its own `Char.Extra` while a project using it might use
`Char.Extra` from `elmcraft/core-extra`).
-}
type alias GlobalKey =
    ( PackageName, FullModuleName, VarName )


type alias State =
    { {- ID counter, making sure every expression gets its own unique ID
         number. As long as we only expose `getNextIdAndTick` as a way to get
         the ID, they'll automatically increment.
      -}
      nextId : Id
    , -- Type ID for each AST node. Ends up being TypeLookupTable.
      nodeIds : Dict FullModuleName (Dict RangeLike Id)
    , {- Environment holding types for lexical bindings: lambda args, let..in,
         case branch patterns. Scoped in and out via `withScopedEnv`, unlike
         `globalEnv` below.
      -}
      lexicalEnv : Dict VarName Type
    , {- Top-level declarations, constructors, ports, and (later) dependency
         values.
         Never scoped away: once binding group solves and generalizes, the final
         type stays here forever.
      -}
      globalEnv : Dict GlobalKey Type
    , {- The solution accumulated so far.
         Never scoped away, a constraint discovered inside a lambda about an
         outer binding must survive the lambda.
      -}
      subst : SubstitutionMap
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
    , nodeIds = Dict.empty
    , lexicalEnv =
        -- When testing, you can populate this with types without having actual definitions present.
        env
    , globalEnv = Dict.empty
    , subst = SubstitutionMap.empty
    }


tickId : TIState ()
tickId =
    modify (\state -> { state | nextId = state.nextId + 1 })


getNextIdAndTick : TIState Id
getNextIdAndTick =
    do get <| \{ nextId } ->
    do tickId <| \() ->
    pure nextId



-- NODE IDS


getNodeIds : TIState (Dict FullModuleName (Dict RangeLike Id))
getNodeIds =
    get
        |> map .nodeIds


{-| Give the node a fresh type ID and remember it under the node's range.
-}
idForNode : FullModuleName -> Node a -> TIState Id
idForNode moduleName node =
    do getNextIdAndTick <| \theId ->
    do (aliasNodeId moduleName (Node.range node) theId) <| \() ->
    pure theId


{-| Make another range point to an already assigned ID.

Needed for when elm-syntax gives two nodes the same range:

  - `Declaration` and its `FunctionImplementation` when there's no documentation and no signature
  - similarly for `LetDeclaration`

-}
aliasNodeId : FullModuleName -> Range -> Id -> TIState ()
aliasNodeId moduleName range theId =
    modify
        (\state ->
            { state
                | nodeIds =
                    state.nodeIds
                        |> Dict.update moduleName
                            (Maybe.withDefault Dict.empty
                                >> Dict.insert (RangeLike.fromRange range) theId
                                >> Just
                            )
            }
        )



-- THE ACCUMULATED SOLUTION


getSubst : TIState SubstitutionMap
getSubst =
    get
        |> map .subst


{-| Add a new subst to what we already have.
Left-biased: `newSubst` wins over `state.subst` (but that should never matter,
each group binds its own fresh vars).
-}
composeSubst : SubstitutionMap -> TIState ()
composeSubst newSubst =
    modify (\state -> { state | subst = SubstitutionMap.compose newSubst state.subst })



-- LEXICAL ENV


getLexicalEnv : TIState (Dict VarName Type)
getLexicalEnv =
    get
        |> map .lexicalEnv


modifyLexicalEnv : (Dict VarName Type -> Dict VarName Type) -> TIState ()
modifyLexicalEnv fn =
    modify (\state -> { state | lexicalEnv = fn state.lexicalEnv })


addBinding : VarName -> Type -> TIState ()
addBinding var type_ =
    {- Diehl removes the key from the dict first... but I think we don't
       need to do that as on collision the new item wins.
    -}
    modifyLexicalEnv (Dict.insert var type_)


{-| Run `action`, then restore `lexicalEnv` back.
Leave `nextId`, `nodeIds`, `globalEnv` and `subst` updated.

This makes args, let bindings etc. not leak into the rest of the program.

-}
withScopedEnv : TIState a -> TIState a
withScopedEnv action =
    \state ->
        let
            ( result, newState ) =
                action state
        in
        ( result, { newState | lexicalEnv = state.lexicalEnv } )


existsInEnv : VarName -> TIState Bool
existsInEnv varName =
    getLexicalEnv
        |> map (Dict.member varName)


{-| Look up a lexical binding (let..in var, lambda arg, ...), substituting all
typevars that we can.
-}
lookupEnv : FullModuleName -> VarName -> TIState MonoType
lookupEnv thisModule var =
    do getLexicalEnv <| \env ->
    case Dict.get var env of
        Nothing ->
            error <|
                VarNotFound
                    { usedIn = thisModule
                    , varName = var
                    }

        Just type_ ->
            do getSubst <| \subst ->
            instantiate (SubstitutionMap.substitute subst type_)



-- GLOBAL ENV


getGlobalEnv : TIState (Dict GlobalKey Type)
getGlobalEnv =
    get
        |> map .globalEnv


addGlobalBinding : GlobalKey -> Type -> TIState ()
addGlobalBinding key type_ =
    modify (\state -> { state | globalEnv = Dict.insert key type_ state.globalEnv })


{-| Look up a global name (top-level/constructor/port/dependency), substituting
all typevars that we can.
-}
lookupGlobalEnv : FullModuleName -> VarName -> TIState MonoType
lookupGlobalEnv moduleName var =
    do getGlobalEnv <| \env ->
    case Dict.get ( "", moduleName, var ) env of
        Nothing ->
            error <|
                VarNotFound
                    { usedIn = moduleName
                    , varName = var
                    }

        Just type_ ->
            do getSubst <| \subst ->
            instantiate (SubstitutionMap.substitute subst type_)


instantiate : Type -> TIState MonoType
instantiate (Forall boundVars monoType) =
    do (traverse (always getNextIdAndTick) boundVars) <| \varIds ->
    let
        subst : SubstitutionMap
        subst =
            List.map2
                (\(( _, super ) as var) freshId ->
                    ( var
                    , -- keep the constraint (eg. `number`)
                      Type.freshVar super freshId
                    )
                )
                boundVars
                varIds
                |> AssocList.fromList
    in
    SubstitutionMap.substituteMono subst monoType
        |> pure



-- GENERALIZATION


generalize : Dict VarName Type -> MonoType -> TIState Type
generalize env monoType =
    do getSubst <| \subst ->
    let
        substitutedEnv : Dict VarName Type
        substitutedEnv =
            SubstitutionMap.substituteTypeEnv subst env

        substitutedMono : MonoType
        substitutedMono =
            SubstitutionMap.substituteMono subst monoType
    in
    pure <| Type.generalize (Type.freeVarsTypeEnv substitutedEnv) substitutedMono
