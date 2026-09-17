module Tests.Elm.TypeInference.Helpers exposing
    ( TestError(..)
    , getDeclType
    , getDeclTypeWithDeps
    , getDeclTypeWithDirectAndDeps
    , getExprType
    , getExprTypeWithDeps
    , inferMainModule
    )

import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
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
    | CouldntFindMainModule
    | CouldntFindMainDeclaration


mainModule : ModuleName
mainModule =
    [ "Main" ]


{-| Parse and infer a whole `Main` module, giving back its type lookup table.
-}
inferMainModule : String -> Result TestError ( File, TypeLookupTable )
inferMainModule moduleCode =
    moduleCode
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Result.mapError (always CouldntParse)
        |> Result.andThen
            (\file ->
                runInference [] [] (Dict.singleton mainModule file)
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
    -> Dict ModuleName File
    -> Result TestError (Dict ModuleName TypeLookupTable)
runInference directDependencies allDependencies files =
    Elm.TypeInference.dependencyEnv
        { directDependencies = directDependencies
        , allDependencies = allDependencies
        }
        |> Result.mapError CouldntInfer
        |> Result.andThen
            (\depEnv ->
                let
                    project :
                        { tables : Dict ModuleName TypeLookupTable
                        , errors : Dict ModuleName Error
                        }
                    project =
                        Elm.TypeInference.inferProject
                            depEnv
                            files
                in
                case Dict.values project.errors of
                    [] ->
                        Ok project.tables

                    err :: _ ->
                        Err (CouldntInfer err)
            )


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
                runInference (List.map .name allDependencies) allDependencies (Dict.singleton mainModule file)
                    |> Result.andThen
                        (\lookupTables ->
                            Dict.get mainModule lookupTables
                                |> Result.fromMaybe CouldntFindMainModule
                                |> Result.andThen
                                    (\lookupTable ->
                                        file.declarations
                                            |> List.Extra.find (\declNode -> getFunctionName (Node.value declNode) == Just "main")
                                            |> Maybe.andThen (\mainNode -> TypeLookupTable.get (Node.range mainNode) lookupTable)
                                            |> Result.fromMaybe CouldntFindMainDeclaration
                                    )
                        )
            )


{-| Some tests need to know which deps are direct: user code can't import
modules from a non-direct dependency.
-}
inferModules :
    List String
    -> List Dependency
    -> Dict ModuleName String
    -> Result TestError (Dict ModuleName ( File, TypeLookupTable ))
inferModules directDependencies allDependencies modules =
    modules
        |> Dict.foldl
            (\moduleName code acc ->
                acc
                    |> Result.andThen
                        (\filesAcc ->
                            code
                                |> Elm.Parser.parse
                                |> Result.map (Elm.Processing.process Elm.Processing.init)
                                |> Result.mapError (always CouldntParse)
                                |> Result.map (\file -> Dict.insert moduleName file filesAcc)
                        )
            )
            (Ok Dict.empty)
        |> Result.andThen
            (\files ->
                runInference directDependencies allDependencies files
                    |> Result.map
                        (\lookupTables ->
                            files
                                |> Dict.map
                                    (\moduleName file ->
                                        ( file
                                        , lookupTables
                                            |> Dict.get moduleName
                                            |> Maybe.withDefault (TypeLookupTable.Internal.TLT Dict.empty)
                                        )
                                    )
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
    inferModules directDependencies dependencies modules
        |> Result.andThen
            (\inferred ->
                Dict.get moduleName inferred
                    |> Result.fromMaybe CouldntFindMainModule
                    |> Result.andThen
                        (\( file, lookupTable ) ->
                            file.declarations
                                |> List.Extra.find (\declNode -> getFunctionName (Node.value declNode) == Just declName)
                                |> Maybe.andThen (\declNode -> TypeLookupTable.get (Node.range declNode) lookupTable)
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
