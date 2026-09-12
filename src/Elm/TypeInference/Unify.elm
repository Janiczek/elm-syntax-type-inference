module Elm.TypeInference.Unify exposing (TypeAlias, UnifyCfg, unifyMany)

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type as Type
    exposing
        ( MonoType(..)
        , PackageName
        )
import Elm.TypeInference.TypeVar as TypeVar
    exposing
        ( SuperType(..)
        , TypeVar
        , TypeVarStyle(..)
        )


type alias TypeAlias =
    { args : List VarName
    , type_ : MonoType
    }


type alias TypeAliases =
    Dict ( PackageName, FullModuleName, VarName ) TypeAlias


{-| `checks == False` (from `inferCorrectCode`) uses the "code already compiles"
invariant and skips some checks.

`checks == True` (from `inferAndCheck`) is used in tests. `internalChecks`
keeps the fast path for production inference while turning a violated
"already-correct code" invariant into a diagnostic instead of silently
returning an incorrect type.

-}
type alias UnifyCfg =
    { typeAliases : TypeAliases
    , checks : Bool
    , internalChecks : Bool
    }


{-| Solve the equations left to right, each under the solution the ones before
it produced.

There's no substitution to pass in or get back: the union-find store lives in
`TIState` and only ever grows, so every equation -- including the ones the
structural cases below recurse into -- is solved against everything learned so
far.

-}
unifyMany : UnifyCfg -> List ( MonoType, MonoType ) -> TIState ()
unifyMany cfg eqs =
    State.foldl
        (\( t1, t2 ) () ->
            State.do (State.substituteEquation t1 t2) <|
                \( ( st1, isGround1 ), ( st2, isGround2 ) ) ->
                    unifyMono cfg isGround1 st1 isGround2 st2
        )
        ()
        eqs


{-| Expand alias (substitute its args) recursively.

Valid Elm aliases can't form an infinite cycle, but `inferAndCheck` is by
definition for code that might not be valid, so the recursion is bounded by
`fuel` rather than trusted to terminate. Running out leaves the type
unexpanded, which at worst produces a type mismatch -- unlike a hang, which
would take down the whole elm-review run.

-}
expandAlias : TypeAliases -> MonoType -> MonoType
expandAlias typeAliases type_ =
    expandAliasHelp maxAliasDepth typeAliases type_


{-| Deeper than any real alias chain; a type nesting aliases 1000 deep would
already be unprintable.
-}
maxAliasDepth : Int
maxAliasDepth =
    1000


expandAliasHelp : Int -> TypeAliases -> MonoType -> MonoType
expandAliasHelp fuel typeAliases type_ =
    case type_ of
        UserDefinedType ut ->
            if fuel <= 0 then
                type_

            else
                case Dict.get ( ut.package, ut.moduleName, ut.name ) typeAliases of
                    Nothing ->
                        type_

                    Just alias_ ->
                        if List.length alias_.args /= List.length ut.args then
                            -- A partially applied or malformed alias. Expanding
                            -- anyway would `List.map2`-truncate and leave the
                            -- body's remaining parameters unsubstituted -- and
                            -- an unsubstituted name-keyed typeclass var then
                            -- leaks out and becomes indistinguishable from an
                            -- unrelated same-named var elsewhere (the bug class
                            -- `e2e/tests/histogram-force-comparable` exists
                            -- for). Leaving the alias opaque is strictly safer.
                            type_

                        else
                            let
                                argsByName : Dict VarName MonoType
                                argsByName =
                                    List.map2
                                        (\rawArgName argType -> ( aliasArgKey rawArgName, argType ))
                                        alias_.args
                                        ut.args
                                        |> Dict.fromList
                            in
                            expandAliasHelp (fuel - 1) typeAliases (substituteAliasArgs argsByName alias_.type_)

        _ ->
            type_


{-| An alias's declared generic (eg. `type alias Threshold comparable = ...`)
is stored verbatim (`"comparable"`), but `Type.fromTypeAnnotation` parses that
same name inside the alias's body through `TypeVar.parse`, which strips
the `number`/`comparable`/`appendable`/`compappend` typeclass prefix (so the
body's `comparable` becomes `TypeVar ( Named "", Comparable )`, not `Named
"comparable"`). Without re-parsing here, `substituteAliasArgs` looks up the
raw name and never finds it, silently leaving the body's typeclass var
unsubstituted -- which then leaks out of the alias and, being name-keyed, is
indistinguishable from any other same-named typeclass var elsewhere in the
project (see the `e2e/tests/histogram-force-comparable` regression fixture).
-}
aliasArgKey : VarName -> VarName
aliasArgKey rawArgName =
    case Tuple.first (TypeVar.parse rawArgName) of
        Named parsedName ->
            parsedName

        Generated _ ->
            rawArgName


{-| One-shot, non-chaining replacement of a type alias's own generic
parameters with the actual type arguments from its use site.

Deliberately not `SubstitutionMap.substituteMono`: that treats its input as a
triangular substitution and chain-follows `TypeVar -> TypeVar` links, which is
unsound here. The alias's parameter names live in a completely different
scope than the caller's own type variables, and can collide by pure spelling
coincidence -- e.g. `type alias Wrap acc = acc -> acc` applied at a call site
that also has its own, unrelated `acc`. Keying by that shared spelling in a
chain-following substitution map produces a var that (mis)resolves to itself,
looping forever. A plain one-shot rewrite instead replaces each hole exactly
once with the caller's argument verbatim, so a same-named-but-unrelated
variable in that argument is never mistaken for another parameter to expand.

-}
substituteAliasArgs : Dict VarName MonoType -> MonoType -> MonoType
substituteAliasArgs argsByName type_ =
    let
        go : MonoType -> MonoType
        go =
            substituteAliasArgs argsByName
    in
    case type_ of
        TypeVar ( Named name, _ ) ->
            Dict.get name argsByName
                |> Maybe.withDefault type_

        TypeVar ( Generated _, _ ) ->
            type_

        Function f ->
            Function { from = go f.from, to = go f.to }

        Int ->
            type_

        Float ->
            type_

        Char ->
            type_

        String ->
            type_

        Bool ->
            type_

        List listItemType ->
            List (go listItemType)

        Unit ->
            type_

        Tuple t1 t2 ->
            Tuple (go t1) (go t2)

        Tuple3 t1 t2 t3 ->
            Tuple3 (go t1) (go t2) (go t3)

        Record fields ->
            Record (Dict.map (\_ v -> go v) fields)

        ExtensibleRecord r ->
            ExtensibleRecord { type_ = go r.type_, fields = Dict.map (\_ v -> go v) r.fields }

        UserDefinedType r ->
            UserDefinedType { r | args = List.map go r.args }

        WebGLShader r ->
            WebGLShader
                { attributes = Dict.map (\_ v -> go v) r.attributes
                , uniforms = Dict.map (\_ v -> go v) r.uniforms
                , varyings = Dict.map (\_ v -> go v) r.varyings
                }


{-| Collapse a consecutive chain of `ExtensibleRecord`s. Bias towards outer fields.
-}
flattenExtensible :
    { type_ : MonoType, fields : Dict VarName MonoType }
    -> { type_ : MonoType, fields : Dict VarName MonoType }
flattenExtensible er =
    case er.type_ of
        ExtensibleRecord inner ->
            flattenExtensible { type_ = inner.type_, fields = Dict.union er.fields inner.fields }

        TypeVar _ ->
            er

        Function _ ->
            er

        Int ->
            er

        Float ->
            er

        Char ->
            er

        String ->
            er

        Bool ->
            er

        List _ ->
            er

        Unit ->
            er

        Tuple _ _ ->
            er

        Tuple3 _ _ _ ->
            er

        Record _ ->
            er

        UserDefinedType _ ->
            er

        WebGLShader _ ->
            er


{-| Pair the two field dicts up _by name_, or `Nothing` if the key sets differ
at all.

Zipping `Dict.values` by sorted position (what this used to do) pairs unrelated
fields whenever the key sets disagree, and truncates to the shorter list --
silently producing wrong types. Two `Record`s with different key sets can reach
here even on code that already compiles, via `collapseExtensible` /
`recordVsExtensible`, so this must be correct in both `checks` modes.

-}
zipRecordFields : Dict VarName MonoType -> Dict VarName MonoType -> Maybe (List ( MonoType, MonoType ))
zipRecordFields bindings1 bindings2 =
    Dict.merge
        (\_ _ _ -> Nothing)
        (\_ v1 v2 acc -> Maybe.map (\eqs -> ( v1, v2 ) :: eqs) acc)
        (\_ _ _ -> Nothing)
        bindings1
        bindings2
        (Just [])
        |> Maybe.map List.reverse


{-| `isGround1`/`isGround2` come for free from the caller's `substituteMono`
call (`optimizations-plan.md` Step 1's groundness tracking) -- don't recompute
them here with `Type.isParametricMono`, that would cost the same O(size) walk
this is meant to save.
-}
unifyMono : UnifyCfg -> Bool -> MonoType -> Bool -> MonoType -> TIState ()
unifyMono cfg isGround1 rawT1 isGround2 rawT2 =
    if rawT1 == rawT2 then
        -- Always on, sound without the invariant: `_Utils_eq` starts
        -- with a reference check, so this is O(1) whenever the two
        -- sides are physically shared (which the ground cache makes
        -- common).
        State.pure ()

    else if not cfg.checks && isGround1 && isGround2 then
        -- Cash in the "code already compiles" invariant: two ground
        -- types reached here only because upstream code already
        -- agreed they unify, so skip the full structural walk below
        -- (and even alias expansion: a ground alias arg can't expand
        -- to something non-ground, since an alias's only free vars
        -- are its args).
        if cfg.internalChecks then
            State.error (InternalInconsistency rawT1 rawT2)

        else
            State.pure ()

    else
        let
            t1 : MonoType
            t1 =
                expandAlias cfg.typeAliases rawT1

            t2 : MonoType
            t2 =
                expandAlias cfg.typeAliases rawT2

            noSubstitutionNeeded : TIState ()
            noSubstitutionNeeded =
                State.pure ()

            typeMismatch : TIState ()
            typeMismatch =
                State.error <| TypeMismatchMono t1 t2

            recordBindings : Dict VarName MonoType -> Dict VarName MonoType -> TIState ()
            recordBindings bindings1 bindings2 =
                if Dict.size bindings1 /= Dict.size bindings2 then
                    typeMismatch

                else
                    case zipRecordFields bindings1 bindings2 of
                        Nothing ->
                            typeMismatch

                        Just eqs ->
                            unifyMany cfg eqs

            recordVsExtensible :
                Dict VarName MonoType
                -> { type_ : MonoType, fields : Dict VarName MonoType }
                -> TIState ()
            recordVsExtensible recordFields rawEr =
                let
                    er : { type_ : MonoType, fields : Dict VarName MonoType }
                    er =
                        flattenExtensible rawEr
                in
                if not (List.all (\k -> Dict.member k recordFields) (Dict.keys er.fields)) then
                    typeMismatch

                else
                    let
                        residual : Dict VarName MonoType
                        residual =
                            Dict.filter (\k _ -> not (Dict.member k er.fields)) recordFields

                        matched : Dict VarName MonoType
                        matched =
                            Dict.filter (\k _ -> Dict.member k er.fields) recordFields
                    in
                    unifyMany cfg
                        (( er.type_, Record residual )
                            :: List.map2 Tuple.pair (Dict.values matched) (Dict.values er.fields)
                        )
        in
        case ( t1, t2 ) of
            ( TypeVar v, _ ) ->
                bind cfg v t2

            ( _, TypeVar v ) ->
                bind cfg v t1

            ( Int, Int ) ->
                noSubstitutionNeeded

            ( Int, _ ) ->
                typeMismatch

            ( Float, Float ) ->
                noSubstitutionNeeded

            ( Float, _ ) ->
                typeMismatch

            ( String, String ) ->
                noSubstitutionNeeded

            ( String, _ ) ->
                typeMismatch

            ( Char, Char ) ->
                noSubstitutionNeeded

            ( Char, _ ) ->
                typeMismatch

            ( Bool, Bool ) ->
                noSubstitutionNeeded

            ( Bool, _ ) ->
                typeMismatch

            ( Unit, Unit ) ->
                noSubstitutionNeeded

            ( Unit, _ ) ->
                typeMismatch

            ( Function a, Function b ) ->
                unifyMany
                    cfg
                    [ ( a.from, b.from )
                    , ( a.to, b.to )
                    ]

            ( Function _, _ ) ->
                typeMismatch

            ( List list1, List list2 ) ->
                unifyMany cfg [ ( list1, list2 ) ]

            ( List _, _ ) ->
                typeMismatch

            ( Tuple t1e1 t1e2, Tuple t2e1 t2e2 ) ->
                unifyMany
                    cfg
                    [ ( t1e1, t2e1 )
                    , ( t1e2, t2e2 )
                    ]

            ( Tuple _ _, _ ) ->
                typeMismatch

            ( Tuple3 t1e1 t1e2 t1e3, Tuple3 t2e1 t2e2 t2e3 ) ->
                unifyMany
                    cfg
                    [ ( t1e1, t2e1 )
                    , ( t1e2, t2e2 )
                    , ( t1e3, t2e3 )
                    ]

            ( Tuple3 _ _ _, _ ) ->
                typeMismatch

            ( Record bindings1, Record bindings2 ) ->
                recordBindings bindings1 bindings2

            ( Record r, ExtensibleRecord er ) ->
                recordVsExtensible r er

            ( Record _, _ ) ->
                typeMismatch

            ( ExtensibleRecord rawR1, ExtensibleRecord rawR2 ) ->
                {- Fields that only one side mentions must be added to the other
                   side's required fields.
                   Both sides' extensible record typevars (the r in { r | ... })
                   now need to be the same var.

                   ie.
                   - getX : { row1 | x : Float } -> Float
                   - getY : { row2 | y : Float } -> Float
                   - sum r = getX r + getY r
                   Use them both on the same record and you get
                   - sum : { commonVar | x : Float, y : Float } -> Float
                -}
                let
                    -- We flatten to be able to correctly find overlaps.
                    -- (There was a bug with an infinite loop in the past.)
                    r1 : { type_ : MonoType, fields : Dict VarName MonoType }
                    r1 =
                        flattenExtensible rawR1

                    r2 : { type_ : MonoType, fields : Dict VarName MonoType }
                    r2 =
                        flattenExtensible rawR2

                    onlyIn1 : Dict VarName MonoType
                    onlyIn1 =
                        Dict.filter (\k _ -> not (Dict.member k r2.fields)) r1.fields

                    onlyIn2 : Dict VarName MonoType
                    onlyIn2 =
                        Dict.filter (\k _ -> not (Dict.member k r1.fields)) r2.fields

                    sharedEqs : List ( MonoType, MonoType )
                    sharedEqs =
                        List.map2 Tuple.pair
                            (Dict.values (Dict.filter (\k _ -> Dict.member k r2.fields) r1.fields))
                            (Dict.values (Dict.filter (\k _ -> Dict.member k r1.fields) r2.fields))
                in
                if Dict.isEmpty onlyIn1 && Dict.isEmpty onlyIn2 then
                    {- Same field set on both sides -> the `r` in `{r | ...}`
                       must be the same for both sides.
                    -}
                    unifyMany cfg (( r1.type_, r2.type_ ) :: sharedEqs)

                else
                    State.do State.getNextIdAndTick <|
                        \tailId ->
                            let
                                tail : MonoType
                                tail =
                                    Type.id_ tailId
                            in
                            unifyMany
                                cfg
                                (( r1.type_, ExtensibleRecord { type_ = tail, fields = onlyIn2 } )
                                    :: ( r2.type_, ExtensibleRecord { type_ = tail, fields = onlyIn1 } )
                                    :: sharedEqs
                                )

            ( ExtensibleRecord er, Record r ) ->
                recordVsExtensible r er

            ( ExtensibleRecord _, _ ) ->
                typeMismatch

            ( UserDefinedType ut1, UserDefinedType ut2 ) ->
                if
                    (ut1.package /= ut2.package)
                        || (ut1.moduleName /= ut2.moduleName)
                        || (ut1.name /= ut2.name)
                        || (List.length ut1.args /= List.length ut2.args)
                then
                    typeMismatch

                else
                    List.map2 Tuple.pair ut1.args ut2.args
                        |> unifyMany cfg

            ( UserDefinedType _, _ ) ->
                typeMismatch

            ( WebGLShader webgl1, WebGLShader webgl2 ) ->
                unifyMany
                    cfg
                    [ ( Record webgl1.attributes, Record webgl2.attributes )
                    , ( Record webgl1.uniforms, Record webgl2.uniforms )
                    , ( Record webgl1.varyings, Record webgl2.varyings )
                    ]

            ( WebGLShader _, _ ) ->
                typeMismatch


{-| Merge `typeVar`'s equivalence class with `type_`.

Both sides arrive already substituted, so `typeVar` is the root of its class
and unbound -- exactly what union-find needs to link or bind without creating a
cycle.

-}
bind : UnifyCfg -> TypeVar -> MonoType -> TIState ()
bind cfg typeVar type_ =
    if type_ == TypeVar typeVar then
        State.pure ()

    else if occursCheck typeVar type_ then
        -- Unconditional, even when `cfg.checks` is off: a cyclic binding like
        -- `a := List a` is what keeps the store acyclic, and without it
        -- `substituteMonoTracked` would recurse into the binding forever.
        -- It's O(size) on a type we just substituted anyway.
        State.error <| InfiniteType typeVar type_

    else
        let
            ( _, super ) =
                typeVar
        in
        case type_ of
            TypeVar (( _, otherSuper ) as otherVar) ->
                case meet super otherSuper of
                    Nothing ->
                        State.error <| SuperTypeMismatch super type_

                    Just m ->
                        if m == super && m == otherSuper then
                            -- Same constraint, so either could represent the
                            -- class -- except that a `Named` var is not unique
                            -- the way a `Generated` one is: two unrelated
                            -- declarations can both write `a` in a signature,
                            -- and an alias body can leak one (see
                            -- `aliasArgKey`). Making such a var the
                            -- representative would bind it for _every_
                            -- same-named var in the project. Generated ids
                            -- can't collide, so they always win; between two
                            -- of a kind, rank decides.
                            State.modifySubst <|
                                case ( Tuple.first typeVar, Tuple.first otherVar ) of
                                    ( Named _, Generated _ ) ->
                                        SubstitutionMap.linkTo { child = typeVar, parent = otherVar }

                                    ( Generated _, Named _ ) ->
                                        SubstitutionMap.linkTo { child = otherVar, parent = typeVar }

                                    _ ->
                                        SubstitutionMap.union typeVar otherVar

                        else if m == otherSuper then
                            -- otherVar is strictly more constrained than
                            -- typeVar; it has to be the representative.
                            State.modifySubst (SubstitutionMap.linkTo { child = typeVar, parent = otherVar })

                        else if m == super then
                            State.modifySubst (SubstitutionMap.linkTo { child = otherVar, parent = typeVar })

                        else
                            -- eg. Comparable and Appendable
                            -- introduce fresh var with combined constraint
                            -- point both at it
                            State.do State.getNextIdAndTick <|
                                \freshId ->
                                    let
                                        fresh : TypeVar
                                        fresh =
                                            ( Generated freshId, m )
                                    in
                                    State.modifySubst
                                        (SubstitutionMap.linkTo { child = typeVar, parent = fresh }
                                            >> SubstitutionMap.linkTo { child = otherVar, parent = fresh }
                                        )

            _ ->
                if not cfg.checks || accepts cfg.typeAliases super type_ then
                    State.modifySubst (SubstitutionMap.bindRoot typeVar type_)

                else
                    State.error <| SuperTypeMismatch super type_


{-| The most specific supertype that satisfies both constraints, if any.
-}
meet : SuperType -> SuperType -> Maybe SuperType
meet a b =
    if a == b then
        Just a

    else
        case ( a, b ) of
            ( Normal, other ) ->
                Just other

            ( other, Normal ) ->
                Just other

            ( Number, Comparable ) ->
                Just Number

            ( Comparable, Number ) ->
                Just Number

            ( Comparable, Appendable ) ->
                Just CompAppend

            ( Appendable, Comparable ) ->
                Just CompAppend

            ( Comparable, CompAppend ) ->
                Just CompAppend

            ( CompAppend, Comparable ) ->
                Just CompAppend

            ( Appendable, CompAppend ) ->
                Just CompAppend

            ( CompAppend, Appendable ) ->
                Just CompAppend

            ( Number, CompAppend ) ->
                Nothing

            ( CompAppend, Number ) ->
                Nothing

            ( Number, Appendable ) ->
                Nothing

            ( Appendable, Number ) ->
                Nothing

            -- The a == b guard above makes these diagonal branches unreachable
            -- but let's not use wildcards anyways
            ( Number, Number ) ->
                Just a

            ( Comparable, Comparable ) ->
                Just a

            ( Appendable, Appendable ) ->
                Just a

            ( CompAppend, CompAppend ) ->
                Just a


accepts : TypeAliases -> SuperType -> MonoType -> Bool
accepts typeAliases super type_ =
    case super of
        Normal ->
            True

        Number ->
            case expandAlias typeAliases type_ of
                Int ->
                    True

                Float ->
                    True

                _ ->
                    False

        Comparable ->
            isComparable typeAliases type_

        Appendable ->
            isAppendable typeAliases type_

        CompAppend ->
            isComparable typeAliases type_ && isAppendable typeAliases type_


{-| `type_` may contain aliases anywhere in its structure (eg. a tuple element
that's a `type alias ModuleName = List String`), not just at the top: expand
before every pattern match, not just once on the way in.
-}
isComparable : TypeAliases -> MonoType -> Bool
isComparable typeAliases type_ =
    case expandAlias typeAliases type_ of
        Int ->
            True

        Float ->
            True

        Char ->
            True

        String ->
            True

        List inner ->
            isComparable typeAliases inner

        Tuple a b ->
            isComparable typeAliases a && isComparable typeAliases b

        Tuple3 a b c ->
            isComparable typeAliases a && isComparable typeAliases b && isComparable typeAliases c

        TypeVar _ ->
            True

        Function _ ->
            False

        Bool ->
            False

        Unit ->
            False

        Record _ ->
            False

        ExtensibleRecord _ ->
            False

        UserDefinedType _ ->
            False

        WebGLShader _ ->
            False


isAppendable : TypeAliases -> MonoType -> Bool
isAppendable typeAliases type_ =
    case expandAlias typeAliases type_ of
        String ->
            True

        List _ ->
            True

        TypeVar _ ->
            True

        Int ->
            False

        Float ->
            False

        Char ->
            False

        Tuple _ _ ->
            False

        Tuple3 _ _ _ ->
            False

        Function _ ->
            False

        Bool ->
            False

        Unit ->
            False

        Record _ ->
            False

        ExtensibleRecord _ ->
            False

        UserDefinedType _ ->
            False

        WebGLShader _ ->
            False


{-| Does `typeVar` occur anywhere in `type_`?
-}
occursCheck : TypeVar -> MonoType -> Bool
occursCheck typeVar type_ =
    let
        inFields : Dict VarName MonoType -> Bool
        inFields fields =
            List.any (occursCheck typeVar) (Dict.values fields)
    in
    case type_ of
        TypeVar var ->
            var == typeVar

        Function { from, to } ->
            occursCheck typeVar from || occursCheck typeVar to

        Int ->
            False

        Float ->
            False

        Char ->
            False

        String ->
            False

        Bool ->
            False

        List listItemType ->
            occursCheck typeVar listItemType

        Unit ->
            False

        Tuple t1 t2 ->
            occursCheck typeVar t1 || occursCheck typeVar t2

        Tuple3 t1 t2 t3 ->
            occursCheck typeVar t1
                || occursCheck typeVar t2
                || occursCheck typeVar t3

        Record fields ->
            inFields fields

        ExtensibleRecord r ->
            occursCheck typeVar r.type_ || inFields r.fields

        UserDefinedType r ->
            List.any (occursCheck typeVar) r.args

        WebGLShader r ->
            inFields r.attributes
                || inFields r.uniforms
                || inFields r.varyings
