module Elm.TypeInference.Helpers exposing (TestError(..), getExprType)

import Dict
import Elm.Parser
import Elm.Processing
import Elm.Syntax.DeclarationV2 exposing (DeclarationV2(..))
import Elm.Syntax.NodeV2 as NodeV2 exposing (NodeV2(..), TypedMeta)
import Elm.TypeInference
import Elm.TypeInference.Error exposing (Error)
import Elm.TypeInference.Type exposing (Type)


type TestError
    = CouldntParse (List String)
    | CouldntInfer Error
    | CouldntFindMainModule
    | CouldntFindAnyDeclaration
    | FoundUnexpectedDeclaration (DeclarationV2 TypedMeta)


getExprType : String -> Result TestError Type
getExprType exprCode =
    let
        moduleCode =
            """
module Main exposing (main)
main = {EXPR}
"""
                |> String.replace "{EXPR}" exprCode

        mainModule =
            ( "Main", [] )
    in
    moduleCode
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Result.mapError (List.map Debug.toString >> CouldntParse)
        |> Result.andThen
            (\file ->
                Elm.TypeInference.infer (Dict.singleton mainModule file)
                    |> Result.mapError CouldntInfer
            )
        |> Result.andThen
            (\typedFiles ->
                Dict.get mainModule typedFiles
                    |> Result.fromMaybe CouldntFindMainModule
            )
        |> Result.andThen
            (\typedFile ->
                List.head typedFile.declarations
                    |> Result.fromMaybe CouldntFindAnyDeclaration
            )
        |> Result.andThen
            (\decl ->
                case decl of
                    NodeV2 _ (FunctionDeclaration { declaration }) ->
                        declaration
                            |> NodeV2.value
                            |> .expression
                            |> NodeV2.type_
                            |> Ok

                    NodeV2 _ d ->
                        Err <| FoundUnexpectedDeclaration d
            )
