module Tests.Elm.TypeInference.Helpers exposing
    ( TestError(..)
    , getDeclType
    , getDeclTypeWithDeps
    , getExprType
    , getExprTypeWithDeps
    , inferMainModule
    , inferModules
    , inferModulesWithDeps
    )

import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.TypeInference
import Elm.TypeInference.Dependencies exposing (DependencyPackage)
import Elm.TypeInference.Error exposing (Error)
import Elm.TypeInference.Type exposing (Type)
import List.Extra
import String.ExtraExtra
import TypeLookupTable exposing (TypeLookupTable)


type TestError
    = CouldntParse (List String)
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
        |> Result.mapError (List.map Debug.toString >> CouldntParse)
        |> Result.andThen
            (\file ->
                Elm.TypeInference.infer { dependencies = [], files = Dict.singleton mainModule file }
                    |> Result.mapError CouldntInfer
                    |> Result.andThen
                        (\lookupTables ->
                            Dict.get mainModule lookupTables
                                |> Result.fromMaybe CouldntFindMainModule
                                |> Result.map (Tuple.pair file)
                        )
            )


getExprType : String -> Result TestError Type
getExprType exprCode =
    getExprTypeWithDeps [] exprCode


getExprTypeWithDeps : List DependencyPackage -> String -> Result TestError Type
getExprTypeWithDeps dependencies exprCode =
    """
module Main exposing (main)

main =
{EXPR}
"""
        |> String.replace "{EXPR}" (String.ExtraExtra.indent 4 exprCode)
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Result.mapError (List.map Debug.toString >> CouldntParse)
        |> Result.andThen
            (\file ->
                Elm.TypeInference.infer { dependencies = dependencies, files = Dict.singleton mainModule file }
                    |> Result.mapError CouldntInfer
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


inferModules : Dict ModuleName String -> Result TestError (Dict ModuleName ( File, TypeLookupTable ))
inferModules modules =
    inferModulesWithDeps [] modules


inferModulesWithDeps :
    List DependencyPackage
    -> Dict ModuleName String
    -> Result TestError (Dict ModuleName ( File, TypeLookupTable ))
inferModulesWithDeps dependencies modules =
    modules
        |> Dict.foldl
            (\moduleName code acc ->
                acc
                    |> Result.andThen
                        (\filesAcc ->
                            code
                                |> Elm.Parser.parse
                                |> Result.map (Elm.Processing.process Elm.Processing.init)
                                |> Result.mapError (List.map Debug.toString >> CouldntParse)
                                |> Result.map (\file -> Dict.insert moduleName file filesAcc)
                        )
            )
            (Ok Dict.empty)
        |> Result.andThen
            (\files ->
                Elm.TypeInference.infer { dependencies = dependencies, files = files }
                    |> Result.mapError CouldntInfer
                    |> Result.map
                        (\lookupTables ->
                            files
                                |> Dict.map
                                    (\moduleName file ->
                                        ( file
                                        , Dict.get moduleName lookupTables
                                            |> Maybe.withDefault (TypeLookupTable.fromDict moduleName Dict.empty)
                                        )
                                    )
                        )
            )


getDeclType : Dict ModuleName String -> ModuleName -> String -> Result TestError Type
getDeclType modules moduleName declName =
    getDeclTypeWithDeps [] modules moduleName declName


getDeclTypeWithDeps :
    List DependencyPackage
    -> Dict ModuleName String
    -> ModuleName
    -> String
    -> Result TestError Type
getDeclTypeWithDeps dependencies modules moduleName declName =
    inferModulesWithDeps dependencies modules
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
