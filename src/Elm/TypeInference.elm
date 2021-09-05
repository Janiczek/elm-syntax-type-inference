module Elm.TypeInference exposing (infer)

{-| TODO write docs

@docs infer

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.DeclarationV2 as DeclarationV2 exposing (DeclarationV2)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ExpressionV2 as ExpressionV2
    exposing
        ( FunctionV2
        , LocatedExpr
        , TypedExpr
        )
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FileV2 exposing (TypedFile)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.NodeV2 as NodeV2
    exposing
        ( LocatedMeta
        , LocatedNode
        , NodeV2(..)
        , TypedMeta
        )
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.PatternV2 as PatternV2
    exposing
        ( LocatedPattern
        , TypedPattern
        )
import Elm.Syntax.Range exposing (Range)
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


infer : Dict FullModuleName File -> Result Error (Dict FullModuleName TypedFile)
infer files =
    gatherTypeAliases files
        |> State.andThen (infer_ files)
        |> State.run State.init
        |> Tuple.first


infer_ :
    Dict FullModuleName File
    -> Dict ( FullModuleName, VarName ) Type
    -> TIState (Dict FullModuleName TypedFile)
infer_ files typeAliases =
    files
        |> Dict.toList
        |> State.traverse (inferFile files typeAliases)
        |> State.map Dict.fromList


inferFile :
    Dict FullModuleName File
    -> Dict ( FullModuleName, VarName ) Type
    -> ( FullModuleName, File )
    -> TIState ( FullModuleName, TypedFile )
inferFile files typeAliases ( moduleName, file ) =
    State.traverse (inferDeclaration files typeAliases) file.declarations
        |> State.map
            (\declarations ->
                ( moduleName
                , { moduleDefinition = NodeV2.fromNode file.moduleDefinition
                  , imports = List.map NodeV2.fromNode file.imports
                  , comments = List.map NodeV2.fromNode file.comments
                  , declarations = declarations
                  }
                )
            )


inferDeclaration :
    Dict FullModuleName File
    -> Dict ( FullModuleName, VarName ) Type
    -> Node Declaration
    -> TIState (LocatedNode (DeclarationV2 TypedMeta))
inferDeclaration files typeAliases declarationNode =
    let
        range : Range
        range =
            Node.range declarationNode

        declaration : Declaration
        declaration =
            Node.value declarationNode
    in
    (case declaration of
        Declaration.FunctionDeclaration fn ->
            inferFunction fn
                |> State.map DeclarationV2.FunctionDeclaration

        Declaration.AliasDeclaration typeAlias ->
            DeclarationV2.AliasDeclaration typeAlias
                |> State.pure

        Declaration.CustomTypeDeclaration customType ->
            DeclarationV2.CustomTypeDeclaration customType
                |> State.pure

        Declaration.PortDeclaration signature ->
            DeclarationV2.PortDeclaration signature
                |> State.pure

        Declaration.InfixDeclaration infix ->
            DeclarationV2.InfixDeclaration infix
                |> State.pure

        Declaration.Destructuring patternNode exprNode ->
            State.map2 DeclarationV2.Destructuring
                (inferPattern patternNode)
                (inferExpr typeAliases exprNode)
    )
        |> State.map (NodeV2 { range = range })


gatherTypeAliases :
    Dict FullModuleName File
    -> TIState (Dict ( FullModuleName, VarName ) Type)
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


inferExpr : Dict ( FullModuleName, VarName ) Type -> Node Expression -> TIState TypedExpr
inferExpr typeAliases exprNode =
    let
        expr_ : LocatedExpr
        expr_ =
            Debug.todo "inferExpr: located expr"
    in
    inferExpr_ typeAliases expr_
        |> State.mapError (\state err -> substituteTypesInError state.idTypes err)


inferExpr_ : Dict ( FullModuleName, VarName ) Type -> LocatedExpr -> TIState TypedExpr
inferExpr_ typeAliases expr =
    State.do (AssignIds.assignIds expr) <| \exprWithIds ->
    State.do (GenerateEquations.generateLocalEquations exprWithIds) <| \exprEquations ->
    State.do GenerateEquations.generateVarEquations <| \varEquations ->
    State.do (Unify.unifyMany typeAliases (exprEquations ++ varEquations)) <| \() ->
    State.do (substituteTypesInExpr exprWithIds) <| \betterExpr ->
    State.pure betterExpr


inferPattern : Node Pattern -> TIState TypedPattern
inferPattern patternNode =
    let
        pattern : LocatedPattern
        pattern =
            Debug.todo "inferPattern: located pattern"
    in
    Debug.todo "infer pattern"


inferFunction : Expression.Function -> TIState (FunctionV2 TypedMeta)
inferFunction function =
    let
        function_ : FunctionV2 LocatedMeta
        function_ =
            Debug.todo "inferFunction: located function"
    in
    Debug.todo "infer function"


substituteTypesInExpr : TypedExpr -> TIState TypedExpr
substituteTypesInExpr expr =
    State.do State.getIdTypes <| \idTypes ->
    expr
        |> ExpressionV2.transformOnce (ExpressionV2.mapType (getBetterType idTypes))
        |> State.pure


substituteTypesInError : Dict Id TypeOrId -> Error -> Error
substituteTypesInError idTypes error =
    case error of
        ImpossibleAstPattern _ ->
            error

        ImpossibleTypePattern _ ->
            error

        VarNotFound _ ->
            error

        AmbiguousName _ ->
            error

        TypeMismatch t1 t2 ->
            TypeMismatch
                (getBetterType idTypes t1)
                (getBetterType idTypes t2)

        OccursCheckFailed id type_ ->
            OccursCheckFailed id (getBetterType idTypes type_)


getBetterType : Dict Id TypeOrId -> TypeOrId -> TypeOrId
getBetterType idTypes typeOrId =
    if Dict.isEmpty idTypes then
        typeOrId

    else
        let
            f : TypeOrId -> TypeOrId
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

                    Record fields ->
                        Type <|
                            Record <|
                                Dict.map (always f) fields

                    ExtensibleRecord { typeVar, fields } ->
                        Type <|
                            ExtensibleRecord
                                { typeVar = typeVar
                                , fields = Dict.map (always f) fields
                                }

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
                            { typeVar = Node.value name
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
