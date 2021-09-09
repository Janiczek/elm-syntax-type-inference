module Elm.TypeInference exposing (infer)

{-| TODO write docs

@docs infer

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.DeclarationV2 as DeclarationV2 exposing (DeclarationV2)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.ExpressionV2 as ExpressionV2
    exposing
        ( FunctionImplementationV2
        , FunctionV2
        , LocatedExpr
        , TypedExpr
        )
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FileV2 as FileV2 exposing (TypedFile)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.NodeV2 as NodeV2
    exposing
        ( LocatedNode
        , NodeV2(..)
        , TypedMeta
        )
import Elm.Syntax.PatternV2 as PatternV2 exposing (LocatedPattern, TypedPattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.AssignIds as AssignIds
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.GenerateEquations as GenerateEquations
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.Type
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
    let
        ( result, state ) =
            gatherTypeAliases files
                |> State.andThen (infer_ files)
                |> State.run State.init
    in
    result
        |> Result.mapError (substituteTypesInError state.idTypes)
        |> Result.map (substituteTypesInFiles state.idTypes)


infer_ :
    Dict FullModuleName File
    -> Dict ( FullModuleName, VarName ) Type
    -> TIState (Dict FullModuleName TypedFile)
infer_ files typeAliases =
    State.do (State.traverse (inferFile files) (Dict.toList files)) <| \typedFiles ->
    State.do GenerateEquations.generateVarEquations <| \() ->
    State.do State.getTypeEquations <| \allEquations ->
    State.do (Unify.unifyMany typeAliases allEquations) <| \() ->
    State.pure (Dict.fromList typedFiles)


inferFile :
    Dict FullModuleName File
    -> ( FullModuleName, File )
    -> TIState ( FullModuleName, TypedFile )
inferFile files ( moduleName, thisFile ) =
    State.traverse (inferDeclaration files thisFile) thisFile.declarations
        |> State.map
            (\declarations ->
                ( moduleName
                , { moduleDefinition = NodeV2.fromNode thisFile.moduleDefinition
                  , imports = List.map NodeV2.fromNode thisFile.imports
                  , comments = List.map NodeV2.fromNode thisFile.comments
                  , declarations = declarations
                  }
                )
            )


inferDeclaration :
    Dict FullModuleName File
    -> File
    -> Node Declaration
    -> TIState (LocatedNode (DeclarationV2 TypedMeta))
inferDeclaration files thisFile declarationNode =
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
            inferFunction files thisFile fn
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
                (inferPattern files thisFile (PatternV2.fromNodePattern patternNode))
                (inferExpr files thisFile (ExpressionV2.fromNodeExpression exprNode))
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


inferExpr :
    Dict FullModuleName File
    -> File
    -> LocatedExpr
    -> TIState TypedExpr
inferExpr files thisFile expr =
    State.do (AssignIds.assignIds expr) <| \exprWithIds ->
    State.do (GenerateEquations.generateExprEquations files thisFile exprWithIds) <| \() ->
    State.pure exprWithIds


inferPattern :
    Dict FullModuleName File
    -> File
    -> LocatedPattern
    -> TIState TypedPattern
inferPattern files thisFile patternNode =
    State.do (AssignIds.assignIdsToPattern patternNode) <| \patternWithIds ->
    State.do (GenerateEquations.generatePatternEquations files thisFile patternWithIds) <| \() ->
    State.pure patternWithIds


inferFunction :
    Dict FullModuleName File
    -> File
    -> Expression.Function
    -> TIState (FunctionV2 TypedMeta)
inferFunction files thisFile function =
    let
        declarationRange : Range
        declarationRange =
            Node.range function.declaration

        oldDeclaration : Expression.FunctionImplementation
        oldDeclaration =
            Node.value function.declaration

        expr : TIState TypedExpr
        expr =
            inferExpr files thisFile (ExpressionV2.fromNodeExpression oldDeclaration.expression)

        arguments : TIState (List TypedPattern)
        arguments =
            oldDeclaration.arguments
                |> State.traverse
                    (PatternV2.fromNodePattern >> inferPattern files thisFile)
    in
    State.map2
        (\expr_ arguments_ ->
            let
                declaration : FunctionImplementationV2 TypedMeta
                declaration =
                    { name = NodeV2.fromNode oldDeclaration.name
                    , arguments = arguments_
                    , expression = expr_
                    }
            in
            { documentation = Maybe.map NodeV2.fromNode function.documentation
            , signature = Maybe.map NodeV2.fromNode function.signature
            , declaration = NodeV2 { range = declarationRange } declaration
            }
        )
        expr
        arguments



-- TYPE SUBSTITUTION


substituteTypesInFiles : Dict Id TypeOrId -> Dict FullModuleName TypedFile -> Dict FullModuleName TypedFile
substituteTypesInFiles idTypes files =
    files
        |> Dict.map
            (\_ file ->
                FileV2.map
                    (\meta -> { meta | type_ = getBetterType idTypes meta.type_ })
                    file
            )


substituteTypesInError : Dict Id TypeOrId -> Error -> Error
substituteTypesInError idTypes error =
    case error of
        ImpossibleExpr _ ->
            error

        ImpossiblePattern _ ->
            error

        ImpossibleType _ ->
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

                    Number ->
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

                    ExtensibleRecord r ->
                        Type <|
                            ExtensibleRecord
                                { type_ = f r.type_
                                , fields = Dict.map (always f) r.fields
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
            State.impossibleType typeAnnotation

        TypeAnnotation.Record fields ->
            recordBindings fields
                |> State.map Record

        TypeAnnotation.GenericRecord name fields ->
            recordBindings (Node.value fields)
                |> State.map
                    (\fields_ ->
                        ExtensibleRecord
                            { type_ = Type (TypeVar (Node.value name))
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
