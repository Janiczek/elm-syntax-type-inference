module Elm.TypeInference.Unify exposing (unifyMany)

import AssocList
import AssocSet
import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Elm.TypeInference.Type as Type
    exposing
        ( Id
        , MonoType(..)
        , SuperType(..)
        , Type(..)
        , TypeVar
        )
import Elm.TypeInference.TypeEquation exposing (TypeEquation)
import Elm.TypeInference.VarName exposing (VarName)
import Set exposing (Set)


unify : Dict ( FullModuleName, VarName ) MonoType -> Type -> Type -> TIState SubstitutionMap
unify typeAliases ((Forall boundVars1 mono1) as t1) ((Forall boundVars2 mono2) as t2) =
    if List.length boundVars1 /= List.length boundVars2 then
        State.error <| TypeMismatch t1 t2

    else
        -- TODO this is most likely wrong
        unifyMono typeAliases mono1 mono2


unifyMany : Dict ( FullModuleName, VarName ) MonoType -> List ( Type, Type ) -> TIState SubstitutionMap
unifyMany typeAliases equations =
    let
        go : SubstitutionMap -> List TypeEquation -> TIState SubstitutionMap
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


unifyManyMono : Dict ( FullModuleName, VarName ) MonoType -> List ( MonoType, MonoType ) -> TIState SubstitutionMap
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


unifyMono : Dict ( FullModuleName, VarName ) MonoType -> MonoType -> MonoType -> TIState SubstitutionMap
unifyMono typeAliases t1 t2 =
    let
        noSubstitutionNeeded : TIState SubstitutionMap
        noSubstitutionNeeded =
            State.pure AssocList.empty

        substitute : TypeVar -> MonoType -> TIState SubstitutionMap
        substitute var type_ =
            State.pure <| AssocList.singleton var type_

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

        -- TODO records vs extensible records
        ( Record bindings1, Record bindings2 ) ->
            recordBindings bindings1 bindings2

        ( Record _, _ ) ->
            typeMismatch

        ( ExtensibleRecord r1, ExtensibleRecord r2 ) ->
            unifyManyMono
                typeAliases
                [ ( r1.type_, r2.type_ )
                , ( Record r1.fields, Record r2.fields )
                ]

        ( ExtensibleRecord _, _ ) ->
            typeMismatch

        ( UserDefinedType ut1, UserDefinedType ut2 ) ->
            if ut1.name /= ut2.name || List.length ut1.args /= List.length ut2.args then
                typeMismatch

            else
                List.map2 Tuple.pair ut1.args ut2.args
                    |> unifyManyMono typeAliases

        ( UserDefinedType ut, _ ) ->
            case Dict.get ( ut.moduleName, ut.name ) typeAliases of
                Nothing ->
                    typeMismatch

                Just aliasedType ->
                    unifyMono typeAliases aliasedType t2

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
            ( style, super ) =
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

                    TypeVar otherVar ->
                        goAhead

                    _ ->
                        State.error <| SuperTypeMismatch super type_


occursCheck : TypeVar -> MonoType -> Bool
occursCheck typeVar type_ =
    AssocSet.member typeVar <| Type.freeVarsMono type_
