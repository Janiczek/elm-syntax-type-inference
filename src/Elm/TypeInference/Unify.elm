module Elm.TypeInference.Unify exposing (TypeAlias, unifyMany)

import AssocList
import AssocSet
import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Elm.TypeInference.Type as Type
    exposing
        ( MonoType(..)
        , PackageName
        , SuperType(..)
        , TypeVar
        , TypeVarStyle(..)
        )
import Elm.TypeInference.VarName exposing (VarName)


type alias TypeAlias =
    { args : List VarName
    , type_ : MonoType
    }


type alias TypeAliases =
    Dict ( PackageName, FullModuleName, VarName ) TypeAlias


unifyMany : TypeAliases -> List ( MonoType, MonoType ) -> TIState SubstitutionMap
unifyMany typeAliases eqs =
    case eqs of
        [] ->
            State.pure AssocList.empty

        ( t1, t2 ) :: eqs_ ->
            State.do (unifyMono typeAliases t1 t2) <| \su1 ->
            State.do
                (unifyMany
                    typeAliases
                    (List.map
                        (Tuple.mapBoth
                            (SubstitutionMap.substituteMono su1)
                            (SubstitutionMap.substituteMono su1)
                        )
                        eqs_
                    )
                )
            <| \su2 ->
            State.pure (SubstitutionMap.compose su2 su1)


{-| Expand alias (substitute its args) recursively.
Elm aliases can't form infinite cycles.
-}
expandAlias : TypeAliases -> MonoType -> MonoType
expandAlias typeAliases type_ =
    case type_ of
        UserDefinedType ut ->
            case Dict.get ( ut.package, ut.moduleName, ut.name ) typeAliases of
                Nothing ->
                    type_

                Just alias_ ->
                    let
                        subst : SubstitutionMap
                        subst =
                            List.map2 (\argName actualArg -> ( ( Named argName, Normal ), actualArg ))
                                alias_.args
                                ut.args
                                |> AssocList.fromList
                    in
                    expandAlias typeAliases (SubstitutionMap.substituteMono subst alias_.type_)

        _ ->
            type_


unifyMono : TypeAliases -> MonoType -> MonoType -> TIState SubstitutionMap
unifyMono typeAliases rawT1 rawT2 =
    let
        t1 : MonoType
        t1 =
            expandAlias typeAliases rawT1

        t2 : MonoType
        t2 =
            expandAlias typeAliases rawT2

        noSubstitutionNeeded : TIState SubstitutionMap
        noSubstitutionNeeded =
            State.pure AssocList.empty

        typeMismatch : TIState SubstitutionMap
        typeMismatch =
            State.error <| TypeMismatchMono t1 t2

        recordBindings : Dict VarName MonoType -> Dict VarName MonoType -> TIState SubstitutionMap
        recordBindings bindings1 bindings2 =
            if Dict.keys bindings1 /= Dict.keys bindings2 then
                typeMismatch

            else
                unifyMany typeAliases
                    (List.map2 Tuple.pair
                        (Dict.values bindings1)
                        (Dict.values bindings2)
                    )

        recordVsExtensible :
            Dict VarName MonoType
            -> { type_ : MonoType, fields : Dict VarName MonoType }
            -> TIState SubstitutionMap
        recordVsExtensible recordFields er =
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
                unifyMany typeAliases
                    (( er.type_, Record residual )
                        :: List.map2 Tuple.pair (Dict.values matched) (Dict.values er.fields)
                    )
    in
    case ( t1, t2 ) of
        ( TypeVar v, _ ) ->
            bind v t2

        ( _, TypeVar v ) ->
            bind v t1

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
                typeAliases
                [ ( a.from, b.from )
                , ( a.to, b.to )
                ]

        ( Function _, _ ) ->
            typeMismatch

        ( List list1, List list2 ) ->
            unifyMono typeAliases list1 list2

        ( List _, _ ) ->
            typeMismatch

        ( Tuple t1e1 t1e2, Tuple t2e1 t2e2 ) ->
            unifyMany
                typeAliases
                [ ( t1e1, t2e1 )
                , ( t1e2, t2e2 )
                ]

        ( Tuple _ _, _ ) ->
            typeMismatch

        ( Tuple3 t1e1 t1e2 t1e3, Tuple3 t2e1 t2e2 t2e3 ) ->
            unifyMany
                typeAliases
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

        ( ExtensibleRecord r1, ExtensibleRecord r2 ) ->
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
            State.do State.getNextIdAndTick <| \tailId ->
            let
                tail : MonoType
                tail =
                    Type.id_ tailId
            in
            unifyMany
                typeAliases
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
                    |> unifyMany typeAliases

        ( UserDefinedType _, _ ) ->
            typeMismatch

        ( WebGLShader webgl1, WebGLShader webgl2 ) ->
            unifyMany
                typeAliases
                [ ( Record webgl1.attributes, Record webgl2.attributes )
                , ( Record webgl1.uniforms, Record webgl2.uniforms )
                , ( Record webgl1.varyings, Record webgl2.varyings )
                ]

        ( WebGLShader _, _ ) ->
            typeMismatch


bind : TypeVar -> MonoType -> TIState SubstitutionMap
bind typeVar type_ =
    if type_ == TypeVar typeVar then
        State.pure SubstitutionMap.empty

    else if occursCheck typeVar type_ then
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
                        if m == otherSuper then
                            -- otherVar is at least as constrained as typeVar
                            -- point typeVar at otherVar
                            State.pure <| SubstitutionMap.singleton typeVar (TypeVar otherVar)

                        else if m == super then
                            -- typeVar is at least as constrained as otherVar
                            -- point otherVar at typeVar
                            State.pure <| SubstitutionMap.singleton otherVar (TypeVar typeVar)

                        else
                            -- eg. Comparable and Appendable
                            -- introduce fresh var with combined constraint
                            -- point both at it
                            State.do State.getNextIdAndTick <| \freshId ->
                            let
                                fresh =
                                    Type.freshVar m freshId
                            in
                            State.pure <| SubstitutionMap.fromList [ ( typeVar, fresh ), ( otherVar, fresh ) ]

            _ ->
                if accepts super type_ then
                    State.pure <| SubstitutionMap.singleton typeVar type_

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


accepts : SuperType -> MonoType -> Bool
accepts super type_ =
    case super of
        Normal ->
            True

        Number ->
            case type_ of
                Int ->
                    True

                Float ->
                    True

                _ ->
                    False

        Comparable ->
            isComparable type_

        Appendable ->
            isAppendable type_

        CompAppend ->
            isComparable type_ && isAppendable type_


isComparable : MonoType -> Bool
isComparable type_ =
    case type_ of
        Int ->
            True

        Float ->
            True

        Char ->
            True

        String ->
            True

        List inner ->
            isComparable inner

        Tuple a b ->
            isComparable a && isComparable b

        Tuple3 a b c ->
            isComparable a && isComparable b && isComparable c

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


isAppendable : MonoType -> Bool
isAppendable type_ =
    case type_ of
        String ->
            True

        List _ ->
            True

        TypeVar _ ->
            True

        -- TODO expand, don't use wildcard
        _ ->
            False


occursCheck : TypeVar -> MonoType -> Bool
occursCheck typeVar type_ =
    AssocSet.member typeVar <| Type.freeVarsMono type_
