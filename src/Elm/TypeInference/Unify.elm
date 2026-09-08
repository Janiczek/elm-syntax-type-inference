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
        , SuperType(..)
        , Type(..)
        , TypeVar
        , TypeVarStyle(..)
        )
import Elm.TypeInference.VarName exposing (VarName)


type alias TypeAlias =
    { args : List VarName
    , type_ : MonoType
    }


unify : Dict ( FullModuleName, VarName ) TypeAlias -> Type -> Type -> TIState SubstitutionMap
unify typeAliases ((Forall boundVars1 mono1) as t1) ((Forall boundVars2 mono2) as t2) =
    if List.length boundVars1 /= List.length boundVars2 then
        State.error <| TypeMismatch t1 t2

    else
        -- TODO this is most likely wrong
        unifyMono typeAliases mono1 mono2


unifyMany : Dict ( FullModuleName, VarName ) TypeAlias -> List ( Type, Type ) -> TIState SubstitutionMap
unifyMany typeAliases equations =
    let
        go : SubstitutionMap -> List ( Type, Type ) -> TIState SubstitutionMap
        go subst equations_ =
            case equations_ of
                [] ->
                    State.pure subst

                ( t1, t2 ) :: restOfEquations ->
                    State.do (unify typeAliases t1 t2) <| \subst1 ->
                    go
                        (SubstitutionMap.compose subst1 subst)
                        (List.map (SubstitutionMap.substituteTypeEquation subst1) restOfEquations)
    in
    go SubstitutionMap.empty equations


unifyManyMono : Dict ( FullModuleName, VarName ) TypeAlias -> List ( MonoType, MonoType ) -> TIState SubstitutionMap
unifyManyMono typeAliases eqs =
    case eqs of
        [] ->
            State.pure AssocList.empty

        ( t1, t2 ) :: eqs_ ->
            State.do (unifyMono typeAliases t1 t2) <| \su1 ->
            State.do
                (unifyManyMono
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
expandAlias : Dict ( FullModuleName, VarName ) TypeAlias -> MonoType -> MonoType
expandAlias typeAliases type_ =
    case type_ of
        UserDefinedType ut ->
            case Dict.get ( ut.moduleName, ut.name ) typeAliases of
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


unifyMono : Dict ( FullModuleName, VarName ) TypeAlias -> MonoType -> MonoType -> TIState SubstitutionMap
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
                    (List.map2 (\b1 b2 -> ( Type.mono b1, Type.mono b2 ))
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
                unifyManyMono typeAliases
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
            unifyManyMono
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
            unifyManyMono
                typeAliases
                [ ( t1e1, t2e1 )
                , ( t1e2, t2e2 )
                ]

        ( Tuple _ _, _ ) ->
            typeMismatch

        ( Tuple3 t1e1 t1e2 t1e3, Tuple3 t2e1 t2e2 t2e3 ) ->
            unifyManyMono
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
            unifyManyMono
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
                (ut1.moduleName /= ut2.moduleName)
                    || (ut1.name /= ut2.name)
                    || (List.length ut1.args /= List.length ut2.args)
            then
                typeMismatch

            else
                List.map2 Tuple.pair ut1.args ut2.args
                    |> unifyManyMono typeAliases

        ( UserDefinedType _, _ ) ->
            typeMismatch

        ( WebGLShader webgl1, WebGLShader webgl2 ) ->
            unifyManyMono
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

            goAhead =
                State.pure <| SubstitutionMap.singleton typeVar type_
        in
        case super of
            Normal ->
                goAhead

            Number ->
                case type_ of
                    Int ->
                        goAhead

                    Float ->
                        goAhead

                    TypeVar (( _, Normal ) as otherVar) ->
                        -- go the other way, from less specific to more specific
                        State.pure <| SubstitutionMap.singleton otherVar (TypeVar typeVar)

                    TypeVar _ ->
                        goAhead

                    _ ->
                        State.error <| SuperTypeMismatch super type_


occursCheck : TypeVar -> MonoType -> Bool
occursCheck typeVar type_ =
    AssocSet.member typeVar <| Type.freeVarsMono type_
