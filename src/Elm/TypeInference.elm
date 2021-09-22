module Elm.TypeInference exposing (infer)

{-| TODO write docs

TODO check declarations against their type annotations

@docs infer

-}

import AssocList
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
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Elm.TypeInference.Type as Type
    exposing
        ( MonoType(..)
        , SuperType(..)
        , TypeVarStyle(..)
        )
import Elm.TypeInference.TypeEquation as TypeEquation exposing (TypeEquation)
import Elm.TypeInference.Unify as Unify
import List.ExtraExtra as List
import Maybe.Extra as Maybe
import Result.Extra as Result


{-| The entry-point you probably want to use.
-}
infer : Dict FullModuleName File -> Result Error (Dict FullModuleName TypedFile)
infer files =
    gatherTypeAliases files
        |> State.andThen (infer_ files)
        |> State.run (State.init Dict.empty)
        |> Tuple.first


infer_ :
    Dict FullModuleName File
    -> Dict ( FullModuleName, VarName ) MonoType
    -> TIState (Dict FullModuleName TypedFile)
infer_ files typeAliases =
    State.do (State.traverse (inferFile files) (Dict.toList files)) <| \typedFilesAndEquations ->
    let
        typedFiles =
            typedFilesAndEquations
                |> List.map Tuple.first

        fileEquations : List TypeEquation
        fileEquations =
            typedFilesAndEquations
                |> List.fastConcatMap Tuple.second
    in
    State.do GenerateEquations.generateVarEquations <| \varEquations ->
    let
        allEquations : List TypeEquation
        allEquations =
            fileEquations ++ varEquations

        _ =
            allEquations
                |> List.map (\eq -> "  " ++ TypeEquation.toString eq)
                |> List.sort
                |> String.join "\n"
                |> (\str -> "\nAll equations:\n" ++ str ++ "\n\n")
                |> (\str -> Debug.log str ())
    in
    State.do (Unify.unifyMany typeAliases (List.map TypeEquation.dropLabel allEquations)) <| \substitutionMap ->
    let
        _ =
            substitutionMap
                |> AssocList.toList
                |> List.map (\( var, type_ ) -> "  " ++ Type.varToString var ++ " ≡ " ++ Type.monoTypeToString type_)
                |> List.sort
                |> String.join "\n"
                |> (\str -> "\nSubstitution map:\n" ++ str ++ "\n\n")
                |> (\str -> Debug.log str ())
    in
    typedFiles
        |> Dict.fromList
        |> substituteTypesInFiles substitutionMap
        |> State.pure


inferFile :
    Dict FullModuleName File
    -> ( FullModuleName, File )
    -> TIState ( ( FullModuleName, TypedFile ), List TypeEquation )
inferFile files ( moduleName, thisFile ) =
    State.traverse (inferDeclaration files thisFile) thisFile.declarations
        |> State.map
            (\declarationsAndEqs ->
                let
                    declarations =
                        declarationsAndEqs
                            |> List.map Tuple.first

                    eqs =
                        declarationsAndEqs
                            |> List.fastConcatMap Tuple.second
                in
                ( ( moduleName
                  , { moduleDefinition = NodeV2.fromNode thisFile.moduleDefinition
                    , imports = List.map NodeV2.fromNode thisFile.imports
                    , comments = List.map NodeV2.fromNode thisFile.comments
                    , declarations = declarations
                    }
                  )
                , eqs
                )
            )


inferDeclaration :
    Dict FullModuleName File
    -> File
    -> Node Declaration
    -> TIState ( LocatedNode (DeclarationV2 TypedMeta), List TypeEquation )
inferDeclaration files thisFile declarationNode =
    let
        range : Range
        range =
            Node.range declarationNode

        declaration : Declaration
        declaration =
            Node.value declarationNode
    in
    -- TODO State.do (State.addVarType thisFile expr id) <| \() ->
    (case declaration of
        Declaration.FunctionDeclaration fn ->
            inferFunction files thisFile fn
                |> State.map (Tuple.mapFirst DeclarationV2.FunctionDeclaration)

        Declaration.AliasDeclaration typeAlias ->
            ( DeclarationV2.AliasDeclaration typeAlias, [] )
                |> State.pure

        Declaration.CustomTypeDeclaration customType ->
            ( DeclarationV2.CustomTypeDeclaration customType, [] )
                |> State.pure

        Declaration.PortDeclaration signature ->
            ( DeclarationV2.PortDeclaration signature, [] )
                |> State.pure

        Declaration.InfixDeclaration infix ->
            ( DeclarationV2.InfixDeclaration infix, [] )
                |> State.pure

        Declaration.Destructuring patternNode exprNode ->
            State.map2
                (\( pat, patEqs ) ( ex, exEqs ) ->
                    ( DeclarationV2.Destructuring pat ex
                    , patEqs ++ exEqs
                    )
                )
                (inferPattern files thisFile (PatternV2.fromNodePattern patternNode))
                (inferExpr files thisFile (ExpressionV2.fromNodeExpression exprNode))
    )
        |> State.map (Tuple.mapFirst (NodeV2 { range = range }))


gatherTypeAliases :
    Dict FullModuleName File
    -> TIState (Dict ( FullModuleName, VarName ) MonoType)
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
                                        type_ : TIState MonoType
                                        type_ =
                                            typeAlias.typeAnnotation
                                                |> Node.value
                                                |> Type.fromTypeAnnotation
                                                |> Result.mapError (State.error << ImpossibleType)
                                                |> Result.map State.pure
                                                |> Result.merge
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
    -> TIState ( TypedExpr, List TypeEquation )
inferExpr files thisFile expr =
    State.do (AssignIds.assignIds expr) <| \exprWithIds ->
    let
        _ =
            exprWithIds
                |> ExpressionV2.map (\{ type_ } -> Type.getDebugId type_)
                |> Debug.log "expr"
    in
    State.do (GenerateEquations.generateExprEquations files thisFile exprWithIds) <| \exprEquations ->
    State.pure ( exprWithIds, exprEquations )


inferPattern :
    Dict FullModuleName File
    -> File
    -> LocatedPattern
    -> TIState ( TypedPattern, List TypeEquation )
inferPattern files thisFile patternNode =
    State.do (AssignIds.assignIdsToPattern patternNode) <| \patternWithIds ->
    State.do (GenerateEquations.generatePatternEquations files thisFile patternWithIds) <| \patternEquations ->
    State.pure ( patternWithIds, patternEquations )


inferFunction :
    Dict FullModuleName File
    -> File
    -> Expression.Function
    -> TIState ( FunctionV2 TypedMeta, List TypeEquation )
inferFunction files thisFile function =
    -- TODO also bind the type annotation against the inferred type: function.signature
    let
        declarationRange : Range
        declarationRange =
            Node.range function.declaration

        oldDeclaration : Expression.FunctionImplementation
        oldDeclaration =
            Node.value function.declaration

        exprAndEquations : TIState ( TypedExpr, List TypeEquation )
        exprAndEquations =
            inferExpr files thisFile (ExpressionV2.fromNodeExpression oldDeclaration.expression)

        argumentsAndEquations : TIState (List ( TypedPattern, List TypeEquation ))
        argumentsAndEquations =
            oldDeclaration.arguments
                |> State.traverse (PatternV2.fromNodePattern >> inferPattern files thisFile)

        arguments : TIState (List TypedPattern)
        arguments =
            argumentsAndEquations
                |> State.map (List.map Tuple.first)

        patternEquations : TIState (List TypeEquation)
        patternEquations =
            argumentsAndEquations
                |> State.map (List.fastConcatMap Tuple.second)
    in
    State.map3
        (\( expr, exprEquations ) arguments_ patternEquations_ ->
            let
                allEquations =
                    exprEquations ++ patternEquations_

                declaration : FunctionImplementationV2 TypedMeta
                declaration =
                    { name = NodeV2.fromNode oldDeclaration.name
                    , arguments = arguments_
                    , expression = expr
                    }
            in
            ( { documentation = Maybe.map NodeV2.fromNode function.documentation
              , signature = Maybe.map NodeV2.fromNode function.signature
              , declaration = NodeV2 { range = declarationRange } declaration
              }
            , allEquations
            )
        )
        exprAndEquations
        arguments
        patternEquations



-- TYPE SUBSTITUTION


substituteTypesInFiles : SubstitutionMap -> Dict FullModuleName TypedFile -> Dict FullModuleName TypedFile
substituteTypesInFiles substitutionMap files =
    files
        |> Dict.map
            (\_ file ->
                FileV2.map
                    (\meta ->
                        { meta
                            | type_ =
                                SubstitutionMap.substitute substitutionMap meta.type_
                        }
                    )
                    file
            )
