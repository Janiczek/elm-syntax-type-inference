module Tests exposing (..)

import Dict
import Elm.Parser
import Elm.Processing
import Elm.Syntax.DeclarationV2 exposing (DeclarationV2(..))
import Elm.Syntax.NodeV2 as NodeV2 exposing (NodeV2(..))
import Elm.TypeInference
import Elm.TypeInference.Type as Type exposing (Type(..), TypeOrId(..))
import Expect
import Test exposing (Test)


testExpr : String -> String -> Type -> Test
testExpr label exprCode expectedType =
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
    Test.test label <| \() ->
    let
        fileResult =
            moduleCode
                |> Elm.Parser.parse
                |> Result.map (Elm.Processing.process Elm.Processing.init)
    in
    case fileResult of
        Err parseErrs ->
            Expect.fail <| "Couldn't parse: " ++ Debug.toString parseErrs

        Ok file ->
            let
                inferResult =
                    Elm.TypeInference.infer (Dict.singleton mainModule file)
            in
            case inferResult of
                Err inferErr ->
                    Expect.fail <| "Couldn't infer types: " ++ Debug.toString inferErr

                Ok typedFiles ->
                    case Dict.get mainModule typedFiles of
                        Nothing ->
                            Expect.fail "Couldn't find the Main module"

                        Just typedFile ->
                            case List.head typedFile.declarations of
                                Nothing ->
                                    Expect.fail "Couldn't find the main declaration"

                                Just (NodeV2 _ (FunctionDeclaration { declaration })) ->
                                    declaration
                                        |> NodeV2.value
                                        |> .expression
                                        |> NodeV2.type_
                                        |> Type.getType
                                        |> Expect.equal (Just expectedType)

                                Just (NodeV2 _ d) ->
                                    Expect.fail <| "Found an unexpected declaration: " ++ Debug.toString d


suite : Test
suite =
    Test.describe "Elm.TypeInference"
        [ Test.describe "infer"
            [ Test.describe "simple single expressions"
                [ testExpr "Unit" "()" Unit
                , testExpr "Integer = Number until proven otherwise" "123" Number
                , testExpr "Hex = Number until proven otherwise" "0x123" Number
                , testExpr "Float" "42.0" Float
                , testExpr "Negation" "-123" Number
                , testExpr "Negation of float" "-123.0" Float
                ]

            -- TODO number later used with an int -> coerced into an int
            -- TODO number later used with a float -> coerced into a float
            ]
        ]
