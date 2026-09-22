module Tests.Elm.TypeInference.Helpers exposing
    ( TestError(..)
    , buildDepEnv
    , getDeclType
    , getDeclTypeWithDeps
    , getDeclTypeWithDirectAndDeps
    , getDeclTypeWithPackage
    , getExprType
    , getExprTypeWithDeps
    , inferMainModule
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
import Elm.TypeInference exposing (Dependency)
import Elm.TypeInference.Error exposing (Error)
import Elm.TypeInference.Type exposing (Type)
import List.Extra
import String.ExtraExtra
import TypeLookupTable exposing (TypeLookupTable)
import TypeLookupTable.Internal


type TestError
    = CouldntParse
    | CouldntInfer Error
    | MissingDependencySources (Dict String (List String))
    | CouldntFindMainModule
    | CouldntFindMainDeclaration


mainModule : ModuleName
mainModule =
    [ "Main" ]


{-| Parse and infer a whole `Main` module, giving back its type lookup table.
-}
inferMainModule : String -> Result TestError ( File, TypeLookupTable )
inferMainModule moduleCode =
    inferMainModuleWithPackage Nothing moduleCode


inferMainModuleWithPackage : Maybe String -> String -> Result TestError ( File, TypeLookupTable )
inferMainModuleWithPackage currentPackage moduleCode =
    moduleCode
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Result.mapError (always CouldntParse)
        |> Result.andThen
            (\file ->
                runInferenceWithPackage currentPackage [] [] [ file ]
                    |> Result.andThen
                        (\lookupTables ->
                            Dict.get mainModule lookupTables
                                |> Result.fromMaybe CouldntFindMainModule
                                |> Result.map (Tuple.pair file)
                        )
            )


runInference :
    List String
    -> List Dependency
    -> List File
    -> Result TestError (Dict ModuleName TypeLookupTable)
runInference directDependencies allDependencies files =
    runInferenceWithPackage Nothing directDependencies allDependencies files


buildDepEnv : List String -> List Dependency -> Result TestError Elm.TypeInference.DependencyEnv
buildDepEnv directDependencies allDependencies =
    case
        Elm.TypeInference.dependencyEnv
            { directDependencies = directDependencies
            , allDependencies = allDependencies
            , sourcesToResolveAmbiguity = Dict.empty
            }
    of
        Elm.TypeInference.Failed err ->
            Err (CouldntInfer err)

        Elm.TypeInference.Ready depEnv ->
            Ok depEnv

        Elm.TypeInference.NeedPackageSources needed ->
            Err (MissingDependencySources needed)


runInferenceWithPackage :
    Maybe String
    -> List String
    -> List Dependency
    -> List File
    -> Result TestError (Dict ModuleName TypeLookupTable)
runInferenceWithPackage currentPackage directDependencies allDependencies files =
    case buildDepEnv directDependencies allDependencies of
        Err err ->
            Err err

        Ok depEnv ->
            case Elm.TypeInference.project currentPackage depEnv files of
                Err err ->
                    Err (CouldntInfer err)

                Ok proj0 ->
                    let
                        tablesAndErrors :
                            { tables : Dict ModuleName TypeLookupTable
                            , errors : Dict ModuleName Error
                            }
                        tablesAndErrors =
                            Elm.TypeInference.inferModules files proj0
                                |> Tuple.first
                    in
                    case Dict.values tablesAndErrors.errors of
                        [] ->
                            Ok tablesAndErrors.tables

                        err :: _ ->
                            Err (CouldntInfer err)


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
                runInference (List.map .name allDependencies) allDependencies [ file ]
                    |> Result.andThen
                        (\lookupTables ->
                            Dict.get mainModule lookupTables
                                |> Result.fromMaybe CouldntFindMainModule
                                |> Result.andThen
                                    (\lookupTable ->
                                        file.declarations
                                            |> List.Extra.find (\declNode -> getFunctionName (Node.value declNode) == Just "main")
                                            |> Maybe.andThen (\mainNode -> Tuple.first (TypeLookupTable.get (Node.range mainNode) lookupTable))
                                            |> Result.fromMaybe CouldntFindMainDeclaration
                                    )
                        )
            )


parseModules : Dict ModuleName String -> Result TestError (List File)
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
                                |> Result.map (\file -> file :: filesAcc)
                        )
            )
            (Ok [])


inferModulesWithPackage :
    Maybe String
    -> List String
    -> List Dependency
    -> Dict ModuleName String
    -> Result TestError (Dict ModuleName ( File, TypeLookupTable ))
inferModulesWithPackage currentPackage directDependencies allDependencies modules =
    parseModules modules
        |> Result.andThen
            (\files ->
                runInferenceWithPackage currentPackage directDependencies allDependencies files
                    |> Result.map
                        (\lookupTables ->
                            files
                                |> List.foldl
                                    (\file acc ->
                                        let
                                            moduleName : ModuleName
                                            moduleName =
                                                FileExtra.moduleName file
                                        in
                                        Dict.insert moduleName
                                            ( file
                                            , lookupTables
                                                |> Dict.get moduleName
                                                |> Maybe.withDefault TypeLookupTable.Internal.empty
                                            )
                                            acc
                                    )
                                    Dict.empty
                        )
            )


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
    inferModulesWithPackage currentPackage directDependencies dependencies modules
        |> Result.andThen
            (\inferred ->
                Dict.get moduleName inferred
                    |> Result.fromMaybe CouldntFindMainModule
                    |> Result.andThen
                        (\( file, lookupTable ) ->
                            file.declarations
                                |> List.Extra.find (\declNode -> getFunctionName (Node.value declNode) == Just declName)
                                |> Maybe.andThen (\declNode -> Tuple.first (TypeLookupTable.get (Node.range declNode) lookupTable))
                                |> Result.fromMaybe CouldntFindMainDeclaration
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
