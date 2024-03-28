module Elm.TypeInference.Helpers exposing (TestError(..), getExprType)

import Dict
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.DeclarationV2 exposing (DeclarationV2(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.NodeV2 as NodeV2 exposing (NodeV2(..), TypedMeta)
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference
import Elm.TypeInference.Error exposing (Error)
import Elm.TypeInference.Type exposing (Type)
import List.Extra as List
import String.ExtraExtra as String
import TypeLookupTable exposing (TypeLookupTable)


type TestError
    = CouldntParse (List String)
    | CouldntInfer Error
    | CouldntFindMainModule
    | CouldntFindMainDeclaration


getExprType : String -> Result TestError Type
getExprType exprCode =
    let
        indentedExprCode =
            String.indent 4 exprCode

        moduleCode =
            """
module Main exposing (main)

main = 
{EXPR}
"""
                |> String.replace "{EXPR}" indentedExprCode

        mainModule : ModuleName
        mainModule =
            [ "Main" ]
    in
    moduleCode
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Result.mapError (List.map Debug.toString >> CouldntParse)
        |> Result.andThen
            (\file ->
                Elm.TypeInference.infer (Dict.singleton mainModule file)
                    |> Result.mapError CouldntInfer
                    |> Result.andThen
                        (\lookupTables ->
                            Dict.get mainModule lookupTables
                                |> Result.fromMaybe CouldntFindMainModule
                        )
                    |> Result.andThen
                        (\lookupTable ->
                            file.declarations
                                |> List.find (\declNode -> getFunctionName (Node.value declNode) == Just "main")
                                |> Maybe.andThen (\mainNode -> TypeLookupTable.get (Node.range mainNode) lookupTable)
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
