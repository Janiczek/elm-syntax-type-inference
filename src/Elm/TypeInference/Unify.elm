module Elm.TypeInference.Unify exposing
    ( unify
    , unifyMany
    )

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.Type exposing (Id, Type(..), TypeOrId(..))
import Elm.TypeInference.TypeEquation exposing (TypeEquation)
import Elm.TypeInference.VarName exposing (VarName)
import Set exposing (Set)


unifyMany : Dict ( FullModuleName, VarName ) Type -> List TypeEquation -> TIState ()
unifyMany typeAliases equations =
    equations
        |> State.traverse (\( t1, t2 ) -> unify typeAliases t1 t2)
        |> State.map (\_ -> ())


unify : Dict ( FullModuleName, VarName ) Type -> TypeOrId -> TypeOrId -> TIState ()
unify typeAliases t1 t2 =
    if t1 == t2 then
        State.pure ()

    else
        case ( t1, t2 ) of
            ( Id id, _ ) ->
                unifyVariable typeAliases id t2

            ( _, Id id ) ->
                unifyVariable typeAliases id t1

            ( Type t1_ maybeId1, Type t2_ maybeId2 ) ->
                unifyTypes typeAliases ( t1_, maybeId1 ) ( t2_, maybeId2 )


unifyTypes : Dict ( FullModuleName, VarName ) Type -> ( Type, Maybe Id ) -> ( Type, Maybe Id ) -> TIState ()
unifyTypes typeAliases ( t1, maybeId1 ) ( t2, maybeId2 ) =
    let
        noOp : TIState ()
        noOp =
            State.pure ()

        typeMismatch : TIState ()
        typeMismatch =
            State.typeMismatch (Type t1 maybeId1) (Type t2 maybeId2)

        coerceLeftTo : Type -> TIState ()
        coerceLeftTo type_ =
            maybeId1
                |> Maybe.map (\id -> State.insertTypeForId id (Type type_ maybeId2))
                |> Maybe.withDefault noOp

        coerceRightTo : Type -> TIState ()
        coerceRightTo type_ =
            maybeId2
                |> Maybe.map (\id -> State.insertTypeForId id (Type type_ maybeId1))
                |> Maybe.withDefault noOp

        recordBindings : Dict VarName TypeOrId -> Dict VarName TypeOrId -> TIState ()
        recordBindings bindings1 bindings2 =
            if Dict.keys bindings1 /= Dict.keys bindings2 then
                typeMismatch

            else
                let
                    fieldEquations : List TypeEquation
                    fieldEquations =
                        List.map2 Tuple.pair
                            (Dict.values bindings1)
                            (Dict.values bindings2)
                in
                unifyMany typeAliases fieldEquations

        recordVsExtensibleRecord :
            { record : Dict VarName TypeOrId
            , extensibleRecord : Dict VarName TypeOrId
            }
            -> TIState ()
        recordVsExtensibleRecord { record, extensibleRecord } =
            let
                fieldsToSet : Dict VarName TypeOrId -> Set VarName
                fieldsToSet dict =
                    dict
                        |> Dict.keys
                        |> Set.fromList

                neededButMissing : Set VarName
                neededButMissing =
                    Set.diff
                        (fieldsToSet extensibleRecord)
                        (fieldsToSet record)
            in
            if Set.isEmpty neededButMissing then
                noOp

            else
                typeMismatch
    in
    case ( t1, t2 ) of
        ( TypeVar name1, TypeVar name2 ) ->
            if name1 == name2 then
                noOp

            else
                -- TODO is this correct?
                typeMismatch

        ( TypeVar _, _ ) ->
            typeMismatch

        ( Int, Int ) ->
            noOp

        ( Int, Number ) ->
            coerceRightTo Int

        ( Int, _ ) ->
            typeMismatch

        ( Float, Float ) ->
            noOp

        ( Float, Number ) ->
            coerceRightTo Float

        ( Float, _ ) ->
            typeMismatch

        ( Number, Number ) ->
            noOp

        ( Number, Int ) ->
            coerceLeftTo Int

        ( Number, Float ) ->
            coerceLeftTo Float

        ( Number, _ ) ->
            typeMismatch

        ( String, String ) ->
            noOp

        ( String, _ ) ->
            typeMismatch

        ( Char, Char ) ->
            noOp

        ( Char, _ ) ->
            typeMismatch

        ( Bool, Bool ) ->
            noOp

        ( Bool, _ ) ->
            typeMismatch

        ( Unit, Unit ) ->
            noOp

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
            unify typeAliases list1 list2

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

        ( Record recBindings, ExtensibleRecord extRec ) ->
            recordVsExtensibleRecord
                { record = recBindings
                , extensibleRecord = extRec.fields
                }

        ( Record _, _ ) ->
            typeMismatch

        ( ExtensibleRecord r1, ExtensibleRecord r2 ) ->
            State.do
                (unify
                    typeAliases
                    r1.type_
                    r2.type_
                )
            <| \() ->
            unifyTypes
                typeAliases
                ( Record r1.fields, Nothing )
                ( Record r2.fields, Nothing )

        ( ExtensibleRecord extRec, Record recBindings ) ->
            recordVsExtensibleRecord
                { record = recBindings
                , extensibleRecord = extRec.fields
                }

        ( ExtensibleRecord _, _ ) ->
            typeMismatch

        ( UserDefinedType ut1, UserDefinedType ut2 ) ->
            if ut1.name /= ut2.name || List.length ut1.args /= List.length ut2.args then
                typeMismatch

            else
                List.map2 Tuple.pair ut1.args ut2.args
                    |> unifyMany typeAliases

        ( UserDefinedType ut, _ ) ->
            case Dict.get ( ut.moduleName, ut.name ) typeAliases of
                Nothing ->
                    typeMismatch

                Just aliasedType ->
                    unifyTypes typeAliases ( aliasedType, Nothing ) ( t2, Nothing )

        ( WebGLShader webgl1, WebGLShader webgl2 ) ->
            State.traverse (\( bindings1, bindings2 ) -> recordBindings bindings1 bindings2)
                [ ( webgl1.attributes, webgl2.attributes )
                , ( webgl1.uniforms, webgl2.uniforms )
                , ( webgl1.varyings, webgl2.varyings )
                ]
                |> State.map (\_ -> ())

        ( WebGLShader _, _ ) ->
            typeMismatch


unifyVariable : Dict ( FullModuleName, VarName ) Type -> Id -> TypeOrId -> TIState ()
unifyVariable typeAliases id otherTypeOrId =
    let
        other : TypeOrId
        other =
            case otherTypeOrId of
                Id _ ->
                    otherTypeOrId

                Type type_ (Just _) ->
                    otherTypeOrId

                Type type_ Nothing ->
                    Type type_ (Just id)

        occursCheck : TIState ()
        occursCheck =
            State.do (occurs id other) <| \doesOccur ->
            if doesOccur then
                State.occursCheckFailed id other

            else
                State.insertTypeForId id other
    in
    State.do (State.getTypeForId id) <| \maybeTypeOrId ->
    case maybeTypeOrId of
        Just typeOrId ->
            unify typeAliases typeOrId other

        Nothing ->
            case other of
                Id otherId ->
                    State.do (State.getTypeForId otherId) <| \maybeOtherType ->
                    case maybeOtherType of
                        Just otherType ->
                            unifyVariable typeAliases id otherType

                        Nothing ->
                            occursCheck

                Type _ _ ->
                    occursCheck


occurs : Id -> TypeOrId -> TIState Bool
occurs id typeOrId =
    let
        f : TypeOrId -> TIState Bool
        f typeOrId_ =
            occurs id typeOrId_

        or : List (TIState Bool) -> TIState Bool
        or list =
            list
                |> State.combine
                |> State.map (List.any identity)

        recordBindings : Dict VarName TypeOrId -> TIState Bool
        recordBindings bindings =
            bindings
                |> Dict.values
                |> List.map f
                |> or
    in
    case typeOrId of
        Id id_ ->
            State.pure <| id == id_

        Type type_ _ ->
            case type_ of
                TypeVar _ ->
                    State.pure False

                Function { from, to } ->
                    or
                        [ f from
                        , f to
                        ]

                Int ->
                    State.pure False

                Float ->
                    State.pure False

                Number ->
                    State.pure False

                Char ->
                    State.pure False

                String ->
                    State.pure False

                Bool ->
                    State.pure False

                List listType ->
                    f listType

                Unit ->
                    State.pure False

                Tuple t1 t2 ->
                    or
                        [ f t1
                        , f t2
                        ]

                Tuple3 t1 t2 t3 ->
                    or
                        [ f t1
                        , f t2
                        , f t3
                        ]

                Record fields ->
                    recordBindings fields

                ExtensibleRecord r ->
                    or
                        [ f r.type_
                        , f (Type (Record r.fields) Nothing)
                        ]

                UserDefinedType { args } ->
                    args
                        |> List.map f
                        |> or

                WebGLShader { attributes, uniforms, varyings } ->
                    or
                        [ recordBindings attributes
                        , recordBindings uniforms
                        , recordBindings varyings
                        ]
