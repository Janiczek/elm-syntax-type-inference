module Tests.Elm.TypeInference.Helpers exposing
    ( TestError(..)
    , buildMainModuleProject
    , buildProject
    , getDeclType
    , getDeclTypeAndProject
    , getDeclTypeWithDeps
    , getDeclTypeWithDirectAndDeps
    , getDeclTypeWithPackage
    , getExprType
    , getExprTypeWithDeps
    , parseModules
    )

import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.File.Extra as FileExtra
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference exposing (Dependency)
import Elm.TypeInference.InferError exposing (InferError, InferErrorDetails(..))
import Elm.TypeInference.ProjectError as ProjectError exposing (ProjectError)
import Elm.TypeInference.Type exposing (Type)
import List.Extra
import String.ExtraExtra


type TestError
    = CouldntParse
    | CouldntInit ProjectError
    | CouldntInfer InferError
    | MissingDependencySources (Dict String (List String))
    | CouldntFindMainModule
    | CouldntFindMainDeclaration


mainModule : ModuleName
mainModule =
    [ "Main" ]


buildMainModuleProject : String -> Result TestError ( File, Elm.TypeInference.Project )
buildMainModuleProject moduleCode =
    buildMainModuleProjectWithPackage Nothing moduleCode


buildMainModuleProjectWithPackage : Maybe String -> String -> Result TestError ( File, Elm.TypeInference.Project )
buildMainModuleProjectWithPackage currentPackage moduleCode =
    moduleCode
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Result.mapError (always CouldntParse)
        |> Result.andThen
            (\file ->
                buildProject
                    currentPackage
                    []
                    []
                    [ file ]
                    |> Result.map (Tuple.pair file)
            )


buildProject :
    Maybe String
    -> List String
    -> List Dependency
    -> List File
    -> Result TestError Elm.TypeInference.Project
buildProject currentPackage directDependencies allDependencies files =
    case
        Elm.TypeInference.init
            { directDependencies = directDependencies
            , allDependencies = allDependencies
            , sourcesToResolveAmbiguity = Dict.empty
            , projectPackageName = currentPackage
            , projectFiles = files
            }
    of
        Ok proj ->
            Ok proj

        Err err ->
            case err of
                ProjectError.NeedPackageSources needed ->
                    Err (MissingDependencySources needed)

                _ ->
                    Err (CouldntInit err)


{-| Look up a `Range` in a `Project`, mapping lookup failures to `TestError`.
-}
lookupType : ModuleName -> Range -> Elm.TypeInference.Project -> Result TestError Type
lookupType moduleName range proj =
    Elm.TypeInference.getType moduleName range proj
        |> Tuple.first
        |> Result.mapError mapLookupError


mapLookupError : InferError -> TestError
mapLookupError err =
    case err.details of
        ModuleNotFound ->
            CouldntFindMainModule

        RangeNotFound ->
            CouldntFindMainDeclaration

        _ ->
            CouldntInfer err


getExprType : String -> Result TestError Type
getExprType exprCode =
    getExprTypeWithDeps [] exprCode


getExprTypeWithDeps : List Dependency -> String -> Result TestError Type
getExprTypeWithDeps allDependencies exprCode =
    """
module Main exposing (main)

main =
{EXPR}
"""
        |> String.replace "{EXPR}" (String.ExtraExtra.indent 4 exprCode)
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Result.mapError (always CouldntParse)
        |> Result.andThen
            (\file ->
                buildProject Nothing (List.map .name allDependencies) allDependencies [ file ]
                    |> Result.andThen
                        (\proj ->
                            file.declarations
                                |> List.Extra.find (\declNode -> getFunctionName (Node.value declNode) == Just "main")
                                |> Result.fromMaybe CouldntFindMainDeclaration
                                |> Result.andThen
                                    (\mainNode ->
                                        lookupType mainModule (Node.range mainNode) proj
                                    )
                        )
            )


parseModules : Dict ModuleName String -> Result TestError (Dict ModuleName File)
parseModules modules =
    modules
        |> Dict.foldl
            (\_ code acc ->
                acc
                    |> Result.andThen
                        (\filesAcc ->
                            code
                                |> Elm.Parser.parse
                                |> Result.map (Elm.Processing.process Elm.Processing.init)
                                |> Result.mapError (always CouldntParse)
                                |> Result.map
                                    (\file ->
                                        Dict.insert (FileExtra.moduleName file) file filesAcc
                                    )
                        )
            )
            (Ok Dict.empty)


getDeclType : Dict ModuleName String -> ModuleName -> String -> Result TestError Type
getDeclType modules moduleName declName =
    getDeclTypeWithDeps [] modules moduleName declName


getDeclTypeWithDeps :
    List Dependency
    -> Dict ModuleName String
    -> ModuleName
    -> String
    -> Result TestError Type
getDeclTypeWithDeps dependencies modules moduleName declName =
    getDeclTypeWithDirectAndDeps (List.map .name dependencies) dependencies modules moduleName declName


getDeclTypeWithDirectAndDeps :
    List String
    -> List Dependency
    -> Dict ModuleName String
    -> ModuleName
    -> String
    -> Result TestError Type
getDeclTypeWithDirectAndDeps directDependencies dependencies modules moduleName declName =
    getDeclTypeWithPackage Nothing directDependencies dependencies modules moduleName declName


getDeclTypeWithPackage :
    Maybe String
    -> List String
    -> List Dependency
    -> Dict ModuleName String
    -> ModuleName
    -> String
    -> Result TestError Type
getDeclTypeWithPackage currentPackage directDependencies dependencies modules moduleName declName =
    getDeclTypeAndProject
        currentPackage
        directDependencies
        dependencies
        modules
        moduleName
        declName
        |> Result.map Tuple.first


{-| Like `getDeclTypeWithPackage`, but also gives the (post-inference) `Project`.
-}
getDeclTypeAndProject :
    Maybe String
    -> List String
    -> List Dependency
    -> Dict ModuleName String
    -> ModuleName
    -> String
    -> Result TestError ( Type, Elm.TypeInference.Project )
getDeclTypeAndProject currentPackage directDependencies dependencies modules moduleName declName =
    parseModules modules
        |> Result.andThen
            (\files ->
                buildProject currentPackage directDependencies dependencies (Dict.values files)
                    |> Result.andThen
                        (\proj ->
                            Dict.get moduleName files
                                |> Result.fromMaybe CouldntFindMainModule
                                |> Result.andThen
                                    (\file ->
                                        file.declarations
                                            |> List.Extra.find (\declNode -> getFunctionName (Node.value declNode) == Just declName)
                                            |> Result.fromMaybe CouldntFindMainDeclaration
                                            |> Result.andThen
                                                (\declNode ->
                                                    Elm.TypeInference.getType moduleName (Node.range declNode) proj
                                                        |> (\( result, newProj ) ->
                                                                result
                                                                    |> Result.mapError mapLookupError
                                                                    |> Result.map (\t -> ( t, newProj ))
                                                           )
                                                )
                                    )
                        )
            )


getFunctionName : Declaration -> Maybe String
getFunctionName decl =
    case decl of
        Declaration.FunctionDeclaration fn ->
            fn.declaration
                |> Node.value
                |> .name
                |> Node.value
                |> Just

        _ ->
            Nothing
