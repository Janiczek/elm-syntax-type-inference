module Elm.TypeInference exposing (infer)

{-| TODO write docs

@docs infer

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.ExpressionV2 as ExpressionV2
    exposing
        ( LocatedExpr
        , TypedExpr
        )
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FileV2 exposing (TypedFile)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.AssignIds as AssignIds
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.GenerateEquations as GenerateEquations
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.Type as Type
    exposing
        ( Id
        , Type(..)
        , TypeOrId(..)
        )
import Elm.TypeInference.Unify as Unify
import List.ExtraExtra as List
import Maybe.Extra as Maybe


infer : Dict FullModuleName File -> Result (List Error) (Dict FullModuleName TypedFile)
infer files =
    gatherTypeAliases files
        |> State.map (\typeAliases -> Debug.todo "infer")


gatherTypeAliases : Dict FullModuleName File -> TIState (Dict ( FullModuleName, VarName ) Type)
gatherTypeAliases files =
    files
        |> Dict.toList
        |> List.map
            (\( moduleName, file ) ->
                file.declarations
                    |> List.map
                        (\declarationNode ->
                            case Node.value declarationNode of
                                Declaration.AliasDeclaration typeAlias ->
                                    let
                                        type_ : TIState Type
                                        type_ =
                                            typeAlias.typeAnnotation
                                                |> Node.value
                                                |> typeAnnotationToType
                                    in
                                    type_
                                        |> State.map
                                            (\type__ ->
                                                Just
                                                    ( ( moduleName, Node.value typeAlias.name )
                                                    , type__
                                                    )
                                            )

                                _ ->
                                    State.pure Nothing
                        )
                    |> State.combine
                    |> State.map Maybe.values
            )
        |> State.combine
        |> State.map (List.fastConcat >> Dict.fromList)


inferExpr : Dict ( FullModuleName, VarName ) Type -> LocatedExpr -> TIState TypedExpr
inferExpr typeAliases expr =
    -- TODO probably not do State.init here?
    let
        ( result, state ) =
            State.run (State.init typeAliases) (inferExpr_ expr)
    in
    State.fromTuple ( result, state )
        |> State.mapError (substituteTypesInError state.idTypes)


inferExpr_ : LocatedExpr -> TIState TypedExpr
inferExpr_ expr =
    State.do (AssignIds.assignIds expr) <| \exprWithIds ->
    State.do (GenerateEquations.generateLocalEquations exprWithIds) <| \exprEquations ->
    State.do GenerateEquations.generateVarEquations <| \varEquations ->
    State.do (Unify.unifyMany (exprEquations ++ varEquations)) <| \() ->
    State.do (substituteTypesInExpr exprWithIds) <| \betterExpr ->
    State.pure betterExpr


substituteTypesInExpr : TypedExpr -> TIState TypedExpr
substituteTypesInExpr expr =
    State.do State.getIdTypes <| \idTypes ->
    expr
        |> ExpressionV2.transformOnce (ExpressionV2.mapType (getBetterType idTypes))
        |> State.pure


substituteTypesInError : Dict Id TypeOrId -> Error -> Error
substituteTypesInError idTypes error =
    case error of
        TypeMismatch t1 t2 ->
            TypeMismatch
                (getBetterType idTypes t1)
                (getBetterType idTypes t2)

        OccursCheckFailed id type_ ->
            OccursCheckFailed id (getBetterType idTypes type_)

        ImpossibleAstPattern _ ->
            error


getBetterType : Dict Id TypeOrId -> TypeOrId -> TypeOrId
getBetterType idTypes typeOrId =
    if Dict.isEmpty idTypes then
        typeOrId

    else
        let
            f =
                getBetterType idTypes
        in
        case typeOrId of
            Id id ->
                Dict.get id idTypes
                    |> Maybe.map f
                    |> Maybe.withDefault typeOrId

            Type type_ ->
                case type_ of
                    Int ->
                        typeOrId

                    Float ->
                        typeOrId

                    Char ->
                        typeOrId

                    String ->
                        typeOrId

                    Bool ->
                        typeOrId

                    TypeVar _ ->
                        typeOrId

                    Function { from, to } ->
                        Type <|
                            Function
                                { from = f from
                                , to = f to
                                }

                    List param ->
                        Type <| List <| f param

                    Unit ->
                        typeOrId

                    Tuple e1 e2 ->
                        Type <| Tuple (f e1) (f e2)

                    Tuple3 e1 e2 e3 ->
                        Type <| Tuple3 (f e1) (f e2) (f e3)

                    Record bindings ->
                        Type <|
                            Record <|
                                Dict.map (always f) bindings

                    UserDefinedType ut ->
                        Type <|
                            UserDefinedType
                                { ut | args = List.map f ut.args }

                    WebGLShader { attributes, uniforms, varyings } ->
                        Type <|
                            WebGLShader
                                { attributes = Dict.map (always f) attributes
                                , uniforms = Dict.map (always f) uniforms
                                , varyings = Dict.map (always f) varyings
                                }


typeAnnotationToType : TypeAnnotation -> TIState Type
typeAnnotationToType typeAnnotation =
    let
        f : TypeAnnotation -> TIState TypeOrId
        f annotation =
            annotation
                |> typeAnnotationToType
                |> State.map Type

        recordBindings :
            List (Node ( Node String, Node TypeAnnotation ))
            -> TIState (Dict VarName TypeOrId)
        recordBindings fields =
            fields
                |> List.map
                    (\fieldNode ->
                        let
                            ( fieldNameNode, annotationNode ) =
                                Node.value fieldNode

                            type_ : TIState TypeOrId
                            type_ =
                                f (Node.value annotationNode)
                        in
                        type_
                            |> State.map (\type__ -> ( Node.value fieldNameNode, type__ ))
                    )
                |> State.combine
                |> State.map Dict.fromList
    in
    case typeAnnotation of
        TypeAnnotation.GenericType name ->
            State.pure <| TypeVar name

        TypeAnnotation.Typed name annotations ->
            let
                ( moduleName, typeName ) =
                    Node.value name

                fullModuleName : FullModuleName
                fullModuleName =
                    FullModuleName.fromModuleName_ moduleName

                args : TIState (List TypeOrId)
                args =
                    annotations
                        |> List.map (Node.value >> f)
                        |> State.combine
            in
            args
                |> State.map
                    (\args_ ->
                        UserDefinedType
                            { moduleName = fullModuleName
                            , name = typeName
                            , args = args_
                            }
                    )

        TypeAnnotation.Unit ->
            State.pure Unit

        TypeAnnotation.Tupled [ a, b ] ->
            State.map2 Tuple
                (f (Node.value a))
                (f (Node.value b))

        TypeAnnotation.Tupled [ a, b, c ] ->
            State.map3 Tuple3
                (f (Node.value a))
                (f (Node.value b))
                (f (Node.value c))

        TypeAnnotation.Tupled _ ->
            State.impossibleTypePattern typeAnnotation

        TypeAnnotation.Record fields ->
            recordBindings fields
                |> State.map Record

        TypeAnnotation.GenericRecord name fields ->
            recordBindings (Node.value fields)
                |> State.map
                    (\fields_ ->
                        ExtensibleRecord
                            { recordVar = Node.value name
                            , fields = fields_
                            }
                    )

        TypeAnnotation.FunctionTypeAnnotation from to ->
            State.map2
                (\from_ to_ ->
                    Function
                        { from = from_
                        , to = to_
                        }
                )
                (f (Node.value from))
                (f (Node.value to))
