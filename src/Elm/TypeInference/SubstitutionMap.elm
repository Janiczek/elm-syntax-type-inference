module Elm.TypeInference.SubstitutionMap exposing
    ( SubstitutionMap
    , empty
    , fromList
    , bindRoot
    , linkTo
    , union
    , substituteMono
    , substituteMonoPure
    , substituteMonoTracked
    , substituteTracked
    , Flags, resultIsGround
    , stampId, setIdLevel, levelOf
    )

{-| The accumulated solution: a union-find store mapping type variables to the
types they've been found equal to.

Each variable is either unbound, `Link`ed to another variable of the same
equivalence class, or `Bound` to a non-variable type. Unification "finds" both
sides' roots and either links the two roots or binds one of them -- which means
**a cycle can never be created**: a link always goes from one root to a
_different_ root, and a bind only ever happens after the occurs check in
`Unify.bind`.

That's the whole point of the shape. The previous design composed immutable
maps, which could stitch two individually-acyclic chains into a cycle (`s1`
says `a -> b`, `s2` says `b -> a`) and needed a `breakCycles` repair pass, a
left-bias contract, and a `setSubst`-vs-`composeSubst` distinction documented
in prose. None of that exists here: there is one store, it only ever grows, and
it is threaded through `TIState` rather than merged.

`Ground` is the cache: a fully-resolved, var-free type. It is monotone and
never needs invalidation -- a var-free type has nothing left to substitute, so
once a var resolves to a ground type that answer can never change. A resolution
that is _not_ ground must never be recorded as `Ground` (it would go stale when
an inner var is later bound); it becomes a `Bound` instead, which is still
path compression, just one that has to be walked again later.

Keeping the cache in the same dictionary as the links is worth a surprising
amount: zonking a type variable is the hottest operation in the library, and a
separate ground dictionary made every single variable cost two dictionary
lookups instead of one. (Measured at ~7% of a whole elm-geometry run, per
lookup.)

@docs SubstitutionMap
@docs empty
@docs fromList
@docs bindRoot
@docs linkTo
@docs union
@docs substituteMono
@docs substituteMonoPure
@docs substituteMonoTracked
@docs substituteTracked
@docs Flags, resultIsGround
@docs stampId, setIdLevel, levelOf

-}

import Bitwise
import Dict exposing (Dict)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Type.Internal as Type
    exposing
        ( Id
        , MonoType(..)
        , Type(..)
        )
import Elm.TypeInference.TypeVar as TypeVar exposing (TypeVar)
import Elm.TypeInference.VarSet as VarSet


type alias SubstitutionMap =
    { slots : Dict Key Slot
    , -- Union by rank, to keep `find` chains short.
      ranks : Dict Key Int
    , -- Let-depth of each generated id at the moment it was created (OCaml
      -- ranks). Lowered to the min of the two sides on unify, so
      -- generalization can quantify "vars younger than the enclosing let"
      -- without scanning the lexical environment.
      levels : Dict Id Int
    }


type Slot
    = -- Same type as this other variable, which is closer to the root.
      Link TypeVar
    | -- Resolves to this non-variable type, which may itself still mention
      -- unresolved variables.
      Bound MonoType
    | -- Resolves to this variable-free type. Final: nothing can change it.
      Ground MonoType


type alias Key =
    VarSet.VarKey


key : TypeVar -> Key
key =
    VarSet.varKey


empty : SubstitutionMap
empty =
    { slots = Dict.empty
    , ranks = Dict.empty
    , levels = Dict.empty
    }


{-| Build a store directly from var/type pairs, for tests that want to pin down
resolution behavior on a hand-written chain.

The caller guarantees the result is acyclic. The solver never builds a store
this way -- it goes through [`linkTo`](#linkTo) / [`union`](#union) /
[`bindRoot`](#bindRoot), which can't create a cycle by construction.

-}
fromList : List ( TypeVar, MonoType ) -> SubstitutionMap
fromList list =
    { slots =
        List.foldl
            (\( var, type_ ) acc -> Dict.insert (key var) (slotFor type_) acc)
            Dict.empty
            list
    , ranks = Dict.empty
    , levels = Dict.empty
    }


slotFor : MonoType -> Slot
slotFor type_ =
    case type_ of
        TypeVar other ->
            Link other

        _ ->
            Bound type_



-- UNION-FIND


findHelp : SubstitutionMap -> List TypeVar -> TypeVar -> ( TypeVar, SubstitutionMap )
findHelp store path var =
    case Dict.get (key var) store.slots of
        Just (Link next) ->
            findHelp store (var :: path) next

        _ ->
            ( var
            , { store
                | slots =
                    List.foldl
                        (\pathVar acc -> Dict.insert (key pathVar) (Link var) acc)
                        store.slots
                        path
              }
            )


{-| Bind a root variable to a non-variable type.

The caller must have run the occurs check first (`Unify.bind` does).

-}
bindRoot : TypeVar -> MonoType -> SubstitutionMap -> SubstitutionMap
bindRoot var type_ store =
    { store | slots = Dict.insert (key var) (Bound type_) store.slots }
        |> adjustLevels (levelOf var store) type_


{-| Point one root at another, with the direction chosen by the caller.

Used when the two variables carry different typeclass constraints and the more
constrained one has to win, regardless of rank.

-}
linkTo : { child : TypeVar, parent : TypeVar } -> SubstitutionMap -> SubstitutionMap
linkTo { child, parent } store =
    -- No rank bookkeeping: constraints only differ between two vars rarely, so
    -- leaving these classes at rank 0 costs nothing measurable and keeps the
    -- common path (`union`) free of extra dictionary work.
    { store | slots = Dict.insert (key child) (Link parent) store.slots }
        |> setVarLevel parent (min (levelOf child store) (levelOf parent store))


{-| Merge two distinct unbound roots, letting rank pick the representative.
-}
union : TypeVar -> TypeVar -> SubstitutionMap -> SubstitutionMap
union a b store =
    let
        keyA : Key
        keyA =
            key a

        keyB : Key
        keyB =
            key b

        rankA : Int
        rankA =
            rankOf store keyA

        rankB : Int
        rankB =
            rankOf store keyB

        mergedLevel : Int
        mergedLevel =
            min (levelOf a store) (levelOf b store)
    in
    if rankA < rankB then
        { store | slots = Dict.insert keyA (Link b) store.slots }
            |> setVarLevel b mergedLevel

    else if rankB < rankA then
        { store | slots = Dict.insert keyB (Link a) store.slots }
            |> setVarLevel a mergedLevel

    else
        { store
            | slots = Dict.insert keyB (Link a) store.slots
            , ranks = Dict.insert keyA (rankA + 1) store.ranks
        }
            |> setVarLevel a mergedLevel


rankOf : SubstitutionMap -> Key -> Int
rankOf store k =
    Dict.get k store.ranks
        |> Maybe.withDefault 0


{-| Record the let-depth of a freshly allocated generated id.
-}
stampId : Id -> Int -> SubstitutionMap -> SubstitutionMap
stampId id level store =
    { store | levels = Dict.insert id level store.levels }


{-| Overwrite an id's let-depth. Used when a binding-group placeholder was
allocated before `enterLevel`.
-}
setIdLevel : Id -> Int -> SubstitutionMap -> SubstitutionMap
setIdLevel =
    stampId


levelOf : TypeVar -> SubstitutionMap -> Int
levelOf var store =
    case Tuple.first var of
        TypeVar.Generated id ->
            Dict.get id store.levels
                |> Maybe.withDefault 0

        TypeVar.Named _ ->
            0


setVarLevel : TypeVar -> Int -> SubstitutionMap -> SubstitutionMap
setVarLevel var level store =
    case Tuple.first var of
        TypeVar.Generated id ->
            { store | levels = Dict.insert id level store.levels }

        TypeVar.Named _ ->
            store


{-| Lower every unbound generated var in `type_` whose level is above
`target`. The type is assumed already substituted, so remaining `TypeVar`s are
roots.
-}
adjustLevels : Int -> MonoType -> SubstitutionMap -> SubstitutionMap
adjustLevels target type_ store =
    let
        inFields : Dict VarName MonoType -> SubstitutionMap -> SubstitutionMap
        inFields fields acc =
            Dict.foldl (\_ fieldType inner -> adjustLevels target fieldType inner) acc fields
    in
    case type_ of
        TypeVar var ->
            if levelOf var store > target then
                setVarLevel var target store

            else
                store

        Function { from, to } ->
            store
                |> adjustLevels target from
                |> adjustLevels target to

        Int ->
            store

        Float ->
            store

        Char ->
            store

        String ->
            store

        Bool ->
            store

        List listItemType ->
            adjustLevels target listItemType store

        Unit ->
            store

        Tuple2 t1 t2 ->
            store
                |> adjustLevels target t1
                |> adjustLevels target t2

        Tuple3 t1 t2 t3 ->
            store
                |> adjustLevels target t1
                |> adjustLevels target t2
                |> adjustLevels target t3

        Record { fields } ->
            inFields fields store

        ExtensibleRecord r ->
            store
                |> adjustLevels target r.extensionTypevar
                |> inFields r.fields

        UserDefinedType r ->
            List.foldl (adjustLevels target) store r.args

        WebGLShader r ->
            store
                |> inFields r.attributes
                |> inFields r.uniforms
                |> inFields r.varyings



-- SUBSTITUTION (ZONKING)


{-| Substitute a whole (possibly quantified) `Type`.

A scheme's quantified vars must not resolve through the store: they're bound
here, not free. They shouldn't be in the store's domain at all, but rather than
assume it, this walks a copy with them removed -- and throws that copy away
afterwards, so the store itself is never corrupted by the exclusion.

-}
substituteTracked : SubstitutionMap -> Type -> ( Type, SubstitutionMap )
substituteTracked store (Forall boundVars monoType) =
    case boundVars of
        [] ->
            let
                ( monoType_, store1 ) =
                    substituteMono store monoType
            in
            ( Forall [] monoType_, store1 )

        _ ->
            let
                restricted : SubstitutionMap
                restricted =
                    List.foldl
                        (\var acc -> { acc | slots = Dict.remove (key var) acc.slots })
                        store
                        boundVars

                ( monoType_, _ ) =
                    substituteMono restricted monoType
            in
            ( Forall boundVars monoType_, store )


{-| `Tuple.first << substituteMono`, for the one-shot renaming maps
(`State.instantiate`) that have no chains and nothing worth caching.
-}
substituteMonoPure : SubstitutionMap -> MonoType -> MonoType
substituteMonoPure store monoType =
    Tuple.first (substituteMono store monoType)


{-| Substitute, threading back an updated store: newly-discovered ground
resolutions get cached, and chains get path-compressed.
-}
substituteMono : SubstitutionMap -> MonoType -> ( MonoType, SubstitutionMap )
substituteMono store monoType =
    let
        ( result, _, store1 ) =
            substituteMonoTracked store monoType
    in
    ( result, store1 )


{-| Was the substituted type var-free? See [`substituteMonoTracked`](#substituteMonoTracked).
-}
resultIsGround : Flags -> Bool
resultIsGround =
    isGround


{-| Same as `substituteMono`, but also returns flags describing the result.

`isGround` falls out of the walk for free -- don't recompute it with
`Type.isParametricMono`, that would cost the same O(size) this is meant to
save. `isChanged` is what keeps structural sharing alive: rebuilding every
constructor even when nothing under it moved throws away the physical identity
that makes `Unify.unifyMono`'s `rawT1 == rawT2` fast path (a reference check
first) actually hit, and for records it rebuilds a whole `Dict` for nothing.

-}
substituteMonoTracked : SubstitutionMap -> MonoType -> ( MonoType, Flags, SubstitutionMap )
substituteMonoTracked store monoType =
    case monoType of
        -- The main interesting part
        TypeVar var ->
            let
                k : Key
                k =
                    key var
            in
            -- One dictionary lookup covers the cache, the unbound case and the
            -- directly-bound case; `find` is inlined so only a real link chain
            -- pays for a walk.
            case Dict.get k store.slots of
                Nothing ->
                    -- Unbound root.
                    ( monoType, noFlags, store )

                Just (Ground groundType) ->
                    ( groundType, groundAndChanged, store )

                Just (Bound bound) ->
                    resolveBound store k bound

                Just (Link next) ->
                    let
                        ( root, store1 ) =
                            findHelp store [ var ] next
                    in
                    case Dict.get (key root) store1.slots of
                        Just (Bound bound) ->
                            resolveBound store1 k bound

                        Just (Ground groundType) ->
                            ( groundType
                            , groundAndChanged
                            , { store1 | slots = Dict.insert k (Ground groundType) store1.slots }
                            )

                        _ ->
                            -- Unbound root: the best we can say is which var
                            -- this one has merged into.
                            ( TypeVar root, changedFlag, store1 )

        -- The rest are just recursion
        Function { from, to } ->
            let
                ( from_, f1, s1 ) =
                    substituteMonoTracked store from

                ( to_, f2, s2 ) =
                    substituteMonoTracked s1 to

                flags : Flags
                flags =
                    both f1 f2
            in
            if isChanged flags then
                ( Function { from = from_, to = to_ }, flags, s2 )

            else
                ( monoType, flags, s2 )

        Int ->
            ( monoType, groundFlag, store )

        Float ->
            ( monoType, groundFlag, store )

        Char ->
            ( monoType, groundFlag, store )

        String ->
            ( monoType, groundFlag, store )

        Bool ->
            ( monoType, groundFlag, store )

        List listItemType ->
            let
                ( listItemType_, flags, s1 ) =
                    substituteMonoTracked store listItemType
            in
            if isChanged flags then
                ( List listItemType_, flags, s1 )

            else
                ( monoType, flags, s1 )

        Unit ->
            ( monoType, groundFlag, store )

        Tuple2 t1 t2 ->
            let
                ( t1_, f1, s1 ) =
                    substituteMonoTracked store t1

                ( t2_, f2, s2 ) =
                    substituteMonoTracked s1 t2

                flags : Flags
                flags =
                    both f1 f2
            in
            if isChanged flags then
                ( Tuple2 t1_ t2_, flags, s2 )

            else
                ( monoType, flags, s2 )

        Tuple3 t1 t2 t3 ->
            let
                ( t1_, f1, s1 ) =
                    substituteMonoTracked store t1

                ( t2_, f2, s2 ) =
                    substituteMonoTracked s1 t2

                ( t3_, f3, s3 ) =
                    substituteMonoTracked s2 t3

                flags : Flags
                flags =
                    both f1 (both f2 f3)
            in
            if isChanged flags then
                ( Tuple3 t1_ t2_ t3_, flags, s3 )

            else
                ( monoType, flags, s3 )

        Record { fields } ->
            let
                ( fields_, flags, s1 ) =
                    substituteFieldsTracked store fields
            in
            if isChanged flags then
                ( Record { fields = fields_ }, flags, s1 )

            else
                ( monoType, flags, s1 )

        ExtensibleRecord r ->
            let
                ( extensionTypevar_, f1, s1 ) =
                    substituteMonoTracked store r.extensionTypevar

                ( fields_, f2, s2 ) =
                    substituteFieldsTracked s1 r.fields

                flags : Flags
                flags =
                    both f1 f2

                needsCollapse : Bool
                needsCollapse =
                    Dict.isEmpty fields_
                        || (case extensionTypevar_ of
                                Record _ ->
                                    True

                                ExtensibleRecord _ ->
                                    True

                                _ ->
                                    False
                           )
            in
            if needsCollapse then
                ( Type.collapseExtensible
                    (ExtensibleRecord
                        { extensionTypevar = extensionTypevar_
                        , fields = fields_
                        }
                    )
                , Bitwise.or flags changedFlag
                , s2
                )

            else if isChanged flags then
                ( ExtensibleRecord
                    { extensionTypevar = extensionTypevar_
                    , fields = fields_
                    }
                , flags
                , s2
                )

            else
                ( monoType, flags, s2 )

        UserDefinedType r ->
            let
                ( args_, flags, s1 ) =
                    substituteArgsTracked store r.args
            in
            if isChanged flags then
                ( UserDefinedType { r | args = args_ }, flags, s1 )

            else
                ( monoType, flags, s1 )

        WebGLShader r ->
            let
                ( attributes_, f1, s1 ) =
                    substituteFieldsTracked store r.attributes

                ( uniforms_, f2, s2 ) =
                    substituteFieldsTracked s1 r.uniforms

                ( varyings_, f3, s3 ) =
                    substituteFieldsTracked s2 r.varyings

                flags : Flags
                flags =
                    both f1 (both f2 f3)
            in
            if isChanged flags then
                ( WebGLShader
                    { attributes = attributes_
                    , uniforms = uniforms_
                    , varyings = varyings_
                    }
                , flags
                , s3
                )

            else
                ( monoType, flags, s3 )


{-| The bound type can itself mention unresolved vars, so resolve it, then
either cache the answer (ground: can never change) or path-compress the chain
straight to it (not ground: still has to be walked again later, but no longer
through the whole chain).
-}
resolveBound : SubstitutionMap -> Key -> MonoType -> ( MonoType, Flags, SubstitutionMap )
resolveBound store k bound =
    let
        ( resolved, flags, store1 ) =
            substituteMonoTracked store bound

        slot : Slot
        slot =
            if isGround flags then
                Ground resolved

            else
                Bound resolved
    in
    ( resolved
    , -- Resolving a var to what it's bound to is always a change.
      Bitwise.or flags changedFlag
    , { store1 | slots = Dict.insert k slot store1.slots }
    )


{-| `isGround` and `isChanged` packed into one `Int`.

This is returned once per AST node per substitution, on the hottest path in the
library. Elm has no 4-tuples, and a record here allocates more than the flags
save.

-}
type alias Flags =
    Int


noFlags : Flags
noFlags =
    0


groundFlag : Flags
groundFlag =
    1


changedFlag : Flags
changedFlag =
    2


groundAndChanged : Flags
groundAndChanged =
    3


isGround : Flags -> Bool
isGround flags =
    Bitwise.and flags groundFlag /= 0


isChanged : Flags -> Bool
isChanged flags =
    Bitwise.and flags changedFlag /= 0


{-| Flags for a node built from two children: ground only if both are, changed
if either is.
-}
both : Flags -> Flags -> Flags
both a b =
    Bitwise.or
        (Bitwise.and groundFlag (Bitwise.and a b))
        (Bitwise.and changedFlag (Bitwise.or a b))


substituteFieldsTracked : SubstitutionMap -> Dict VarName MonoType -> ( Dict VarName MonoType, Flags, SubstitutionMap )
substituteFieldsTracked store fields =
    let
        ( reversed, flags, store1 ) =
            Dict.foldl
                (\name type_ ( accList, accFlags, accSubst ) ->
                    let
                        ( type__, fieldFlags, accSubst1 ) =
                            substituteMonoTracked accSubst type_
                    in
                    ( ( name, type__ ) :: accList, both accFlags fieldFlags, accSubst1 )
                )
                ( [], groundFlag, store )
                fields
    in
    if isChanged flags then
        ( Dict.fromList reversed, flags, store1 )

    else
        -- Nothing moved: hand back the very same `Dict` instead of rebuilding
        -- an identical one.
        ( fields, flags, store1 )


substituteArgsTracked : SubstitutionMap -> List MonoType -> ( List MonoType, Flags, SubstitutionMap )
substituteArgsTracked store args =
    let
        ( args_, flags, store1 ) =
            List.foldr
                (\type_ ( accArgs, accFlags, accSubst ) ->
                    let
                        ( type__, argFlags, accSubst1 ) =
                            substituteMonoTracked accSubst type_
                    in
                    ( type__ :: accArgs, both accFlags argFlags, accSubst1 )
                )
                ( [], groundFlag, store )
                args
    in
    if isChanged flags then
        ( args_, flags, store1 )

    else
        ( args, flags, store1 )
