module Elm.TypeInference.State exposing
    ( TIState, State, GlobalKey, init, empty
    , pure, error, fromResult, run
    , map, map2, mapError
    , do, andThen, foldl, traverse
    , getNextIdAndTick
    , getNodeIds, idForNode, aliasNodeId
    , getSubst, modifySubst, substituteEquation
    , addBinding, existsInEnv, lookupEnv, withScopedEnv
    , addGlobalBinding, lookupGlobalEnv, getGlobalEnv
    , enterLevel, leaveLevel, setIdLevel, generalizeWith, instantiate
    )

{-| State useful during various phases of the type inference algorithm.


# General

@docs TIState, State, GlobalKey, init, empty


# Utilities

@docs pure, error, fromResult, run
@docs map, map2, mapError
@docs do, andThen, foldl, traverse


# Next ID

@docs getNextIdAndTick


# Node IDs: the `Range -> Id` dict we're building up

@docs getNodeIds, idForNode, aliasNodeId


# The accumulated solution

@docs getSubst, modifySubst, substituteEquation


# Lexical env: scoping (lambda args, let..in, case branches)

@docs addBinding, existsInEnv, lookupEnv, withScopedEnv


# Global env: top-level declarations, constructors, ports, (later) dependencies

@docs addGlobalBinding, lookupGlobalEnv, getGlobalEnv


# Generalization

@docs enterLevel, leaveLevel, setIdLevel, generalizeWith, instantiate

-}

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Elm.TypeInference.Type.Internal as Type exposing (Id, MonoType, PackageName, Type(..))
import Elm.TypeInference.TypeVar as TypeVar exposing (TypeVar)
import Elm.TypeInference.VarSet as VarSet
import RangeLike exposing (RangeLike)



-- GENERAL


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
    , {- Type ID for each AST node of the module being inferred. Ends up being
         its TypeLookupTable. Inference runs one module at a time, so no module
         name needs to be part of the key.
      -}
      nodeIds : Dict RangeLike Id
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
    , -- Enclosing let-depth. Fresh vars are stamped with this; generalization
      -- quantifies vars whose stamp is strictly deeper.
      currentLevel : Int
    }


type alias TIState a =
    State -> ( Result Error a, State )


pure : a -> TIState a
pure a =
    \s -> ( Ok a, s )


error : Error -> TIState a
error error_ =
    \s -> ( Err error_, s )


fromResult : Result Error a -> TIState a
fromResult result =
    case result of
        Err err ->
            error err

        Ok value ->
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


{-| Tail-recursive left fold.

Needed because a naive list recursion with continuations would blow up the stack
on realistic code. This will provide constant stack instead.

-}
foldl : (a -> b -> TIState b) -> b -> List a -> TIState b
foldl fn init_ list =
    \state -> foldlHelp fn init_ list state


foldlHelp : (a -> b -> TIState b) -> b -> List a -> State -> ( Result Error b, State )
foldlHelp fn acc list state =
    case list of
        [] ->
            ( Ok acc, state )

        x :: rest ->
            case fn x acc state of
                ( Err err, newState ) ->
                    ( Err err, newState )

                ( Ok newAcc, newState ) ->
                    foldlHelp fn newAcc rest newState


{-| Tail-recursive, for the same reason as [`foldl`](#foldl): the naive
`List.foldr (map2 (::)) (pure [])` recurses once per element when the composed
action is _run_, which blows the stack on large projects.
-}
traverse : (a -> TIState b) -> List a -> TIState (List b)
traverse f list =
    \state -> traverseHelp f [] list state


traverseHelp : (a -> TIState b) -> List b -> List a -> State -> ( Result Error (List b), State )
traverseHelp f acc list state =
    case list of
        [] ->
            ( Ok (List.reverse acc), state )

        x :: rest ->
            case f x state of
                ( Err err, newState ) ->
                    ( Err err, newState )

                ( Ok b, newState ) ->
                    traverseHelp f (b :: acc) rest newState


get : TIState State
get =
    \state -> ( Ok state, state )


modify : (State -> State) -> TIState ()
modify fn =
    \state -> ( Ok (), fn state )



-- OUR API


empty : State
empty =
    init
        { lexicalEnv = Dict.empty
        , globalEnv = Dict.empty
        }


{-| `lexicalEnv` is a test hook: you can populate it with types without having
actual definitions present. `globalEnv` is how a module's inference is seeded
with the dependencies and with the interfaces of the modules it imports.
-}
init :
    { lexicalEnv : Dict VarName Type
    , globalEnv : Dict GlobalKey Type
    }
    -> State
init env =
    { nextId = 0
    , nodeIds = Dict.empty
    , lexicalEnv = env.lexicalEnv
    , globalEnv = env.globalEnv
    , subst = SubstitutionMap.empty
    , currentLevel = 0
    }


getNextIdAndTick : TIState Id
getNextIdAndTick =
    \state ->
        ( Ok state.nextId
        , { state
            | nextId = state.nextId + 1
            , subst = SubstitutionMap.stampId state.nextId state.currentLevel state.subst
          }
        )


enterLevel : TIState ()
enterLevel =
    modify (\state -> { state | currentLevel = state.currentLevel + 1 })


leaveLevel : TIState ()
leaveLevel =
    modify (\state -> { state | currentLevel = state.currentLevel - 1 })


{-| Move a previously allocated id to the current let-depth.
-}
setIdLevel : Id -> TIState ()
setIdLevel id =
    do get <|
        \state ->
            modifySubst (SubstitutionMap.setIdLevel id state.currentLevel)



-- NODE IDS


getNodeIds : TIState (Dict RangeLike Id)
getNodeIds =
    get
        |> map .nodeIds


{-| Give the node a fresh type ID and remember it under the node's range.
-}
idForNode : Node a -> TIState Id
idForNode node =
    do getNextIdAndTick <|
        \theId ->
            do (aliasNodeId (Node.range node) theId) <|
                \() ->
                    pure theId


{-| Make another range point to an already assigned ID.

Needed for when elm-syntax gives two nodes the same range:

  - `Declaration` and its `FunctionImplementation` when there's no documentation and no signature
  - similarly for `LetDeclaration`

-}
aliasNodeId : Range -> Id -> TIState ()
aliasNodeId range theId =
    modify
        (\state ->
            { state | nodeIds = Dict.insert (RangeLike.fromRange range) theId state.nodeIds }
        )



-- THE ACCUMULATED SOLUTION


getSubst : TIState SubstitutionMap
getSubst =
    get
        |> map .subst


{-| The store only ever grows, so there is nothing to merge: every writer
threads the same store forward.
-}
modifySubst : (SubstitutionMap -> SubstitutionMap) -> TIState ()
modifySubst fn =
    modify (\state -> { state | subst = fn state.subst })


setSubst : SubstitutionMap -> TIState ()
setSubst subst =
    modify (\state -> { state | subst = subst })


{-| Substitute a `MonoType`, writing back any newly-discovered ground
resolutions / path-compressed chains into `state.subst` so later lookups
benefit too.
-}
substituteMono : MonoType -> TIState MonoType
substituteMono monoType =
    do getSubst <|
        \subst ->
            let
                ( monoType_, subst1 ) =
                    SubstitutionMap.substituteMono subst monoType
            in
            do (setSubst subst1) <|
                \() ->
                    pure monoType_


{-| Substitute both sides of one equation, reporting for each whether it came
out ground (var-free) -- which `Unify` uses to skip the structural walk
entirely.

Both sides in one action on purpose: `Unify.unifyMany` runs this per equation,
and threading the store through the monad twice instead of once is pure
allocation on the hottest path in the whole library.

-}
substituteEquation : MonoType -> MonoType -> TIState ( ( MonoType, Bool ), ( MonoType, Bool ) )
substituteEquation t1 t2 =
    \state ->
        let
            ( st1, flags1, subst1 ) =
                SubstitutionMap.substituteMonoTracked state.subst t1

            ( st2, flags2, subst2 ) =
                SubstitutionMap.substituteMonoTracked subst1 t2
        in
        ( Ok
            ( ( st1, SubstitutionMap.resultIsGround flags1 )
            , ( st2, SubstitutionMap.resultIsGround flags2 )
            )
        , { state | subst = subst2 }
        )


{-| Same as `substituteMono`, but for a whole (possibly quantified) `Type`.
-}
substitute : Type -> TIState Type
substitute type_ =
    do getSubst <|
        \subst ->
            let
                ( type__, subst1 ) =
                    SubstitutionMap.substituteTracked subst type_
            in
            do (setSubst subst1) <|
                \() ->
                    pure type__



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
    do getLexicalEnv <|
        \env ->
            case Dict.get var env of
                Nothing ->
                    error <|
                        VarNotFound
                            { usedIn = thisModule
                            , varName = var
                            }

                Just type_ ->
                    do (substitute type_) <|
                        \substituted ->
                            instantiate substituted



-- GLOBAL ENV


{-| The whole global env, for extracting a module's `Interface` once it's
inferred.
-}
getGlobalEnv : TIState (Dict GlobalKey Type)
getGlobalEnv =
    get
        |> map .globalEnv


addGlobalBinding : GlobalKey -> Type -> TIState ()
addGlobalBinding key type_ =
    modify (\state -> { state | globalEnv = Dict.insert key type_ state.globalEnv })


{-| Look up a global name (top-level/constructor/port/dependency).

`globalEnv` only ever holds closed schemes (top-level `solveGroup` always
generalizes against an empty outer lexical env, and annotations/constructors/
ports/dependency types are closed by construction via `Type.closeOver`), so
unlike `lookupEnv` there is nothing to substitute here: a closed scheme has no
free vars left for `state.subst` to resolve.

-}
lookupGlobalEnv : PackageName -> FullModuleName -> VarName -> TIState MonoType
lookupGlobalEnv package moduleName var =
    do getGlobalEnv <|
        \env ->
            case Dict.get ( package, moduleName, var ) env of
                Nothing ->
                    error <|
                        VarNotFound
                            { usedIn = moduleName
                            , varName = var
                            }

                Just type_ ->
                    instantiate type_


{-| Give a scheme's quantified variables fresh ids.

Deliberately **not** `SubstitutionMap.substituteMono`: that follows chains, and
here the renaming's targets share an id space with its domain. A scheme reaches
us from whichever module defined it (or from a dependency's docs), while the
fresh ids come from the module currently being inferred -- so a fresh id can
land exactly on another of this same scheme's bound ids, and a chain-following
substitution would rename through it and collapse two distinct quantified
variables into one. `Type.mapVarsMono` replaces each variable once.

-}
instantiate : Type -> TIState MonoType
instantiate (Forall boundVars monoType) =
    case boundVars of
        [] ->
            pure monoType

        _ ->
            do (traverse (always getNextIdAndTick) boundVars) <|
                \varIds ->
                    let
                        renaming : Dict VarSet.VarKey TypeVar
                        renaming =
                            List.map2
                                (\(( _, super ) as var) freshId ->
                                    ( VarSet.varKey var
                                    , -- keep the constraint (eg. `number`)
                                      ( TypeVar.Generated freshId, super )
                                    )
                                )
                                boundVars
                                varIds
                                |> Dict.fromList
                    in
                    monoType
                        |> Type.mapVarsMono
                            (\var ->
                                Dict.get (VarSet.varKey var) renaming
                                    |> Maybe.withDefault var
                            )
                        |> pure



-- GENERALIZATION


generalizeWith : MonoType -> TIState Type
generalizeWith monoType =
    do (substituteMono monoType) <|
        \substitutedMono ->
            do get <|
                \state ->
                    let
                        boundIds : List TypeVar
                        boundIds =
                            Type.freeVarsMono substitutedMono
                                |> VarSet.toList
                                |> List.filter
                                    (\var -> SubstitutionMap.levelOf var state.subst > state.currentLevel)
                    in
                    pure (Forall boundIds substitutedMono)
