module Tests exposing (..)

import Dict
import Elm.Parser
import Elm.Processing
import Elm.Syntax.DeclarationV2 exposing (DeclarationV2(..))
import Elm.Syntax.NodeV2 as NodeV2 exposing (NodeV2(..))
import Elm.TypeInference
import Elm.TypeInference.Type as Type exposing (Type(..), TypeOrId(..))
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


getExprType : String -> Result String Type
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
        |> Result.mapError (\parseErrs -> "Couldn't parse: " ++ Debug.toString parseErrs)
        |> Result.andThen
            (\file ->
                Elm.TypeInference.infer (Dict.singleton mainModule file)
                    |> Result.mapError (\inferErr -> "Couldn't infer types: " ++ Debug.toString inferErr)
            )
        |> Result.andThen
            (\typedFiles ->
                Dict.get mainModule typedFiles
                    |> Result.fromMaybe "Couldn't find the Main module"
            )
        |> Result.andThen
            (\typedFile ->
                List.head typedFile.declarations
                    |> Result.fromMaybe "Couldn't find the main declaration"
            )
        |> Result.andThen
            (\decl ->
                case decl of
                    NodeV2 _ (FunctionDeclaration { declaration }) ->
                        declaration
                            |> NodeV2.value
                            |> .expression
                            |> NodeV2.type_
                            |> Type.getType
                            |> Result.fromMaybe "Couldn't get type of inferred declaration"

                    NodeV2 _ d ->
                        Err <| "Found an unexpected declaration: " ++ Debug.toString d
            )


testExpr : String -> String -> Type -> Test
testExpr label exprCode expectedType =
    Test.test label <| \() ->
    case getExprType exprCode of
        Err err ->
            Expect.fail err

        Ok type_ ->
            type_ |> Expect.equal expectedType


suite : Test
suite =
    let
        singleExprs : List ( String, String, Type )
        singleExprs =
            [ ( "Unit", "()", Unit )
            , ( "Integer = Number until proven otherwise", "123", Number )
            , ( "Hex = Number until proven otherwise", "0x123", Number )
            , ( "Float", "42.0", Float )
            , ( "Negation of int", "-123", Number )
            , ( "Negation of hex", "-0x123", Number )
            , ( "Negation of float", "-123.0", Float )
            , ( "Literal", "\"ABC\"", String )
            , ( "Char literal", "'A'", Char )
            , ( "Parenthesized float", "(42.0)", Float )
            , ( "List of floats", "[1.0, 2.0, 3.0]", List (Type Float) )
            , ( "List of ints and one float at end = floats", "[1, 2, 3.0]", List (Type Float) )
            , ( "List of ints and one float at beginning = floats", "[1.0, 2, 3]", List (Type Float) )
            , ( "List of ints = numbers", "[1, 2, 3]", List (Type Number) )

            -- TODO , ( "Heterogenous list isn't allowed", "[1, ()]", fails )
            -- TODO Application (List (ExprWith meta))
            -- TODO OperatorApplication String InfixDirection (ExprWith meta) (ExprWith meta)
            -- TODO FunctionOrValue ModuleName String
            -- TODO IfBlock (ExprWith meta) (ExprWith meta) (ExprWith meta)
            -- TODO PrefixOperator String
            -- TODO Operator String
            -- TODO TupledExpression (List (ExprWith meta))
            -- TODO LetExpression (LetBlock meta)
            -- TODO CaseExpression (CaseBlock meta)
            -- TODO LambdaExpression (Lambda meta)
            -- TODO RecordExpr (List (LocatedNode (RecordSetter meta)))
            -- TODO ListExpr (List (ExprWith meta))
            -- TODO RecordAccess (ExprWith meta) (LocatedNode String)
            -- TODO RecordAccessFunction String
            -- TODO RecordUpdateExpression (LocatedNode String) (List (LocatedNode (RecordSetter meta)))
            -- TODO GLSLExpression String
            ]

        singleExprFuzzer : Fuzzer String
        singleExprFuzzer =
            singleExprs
                |> List.map (\( _, expr, _ ) -> Fuzz.constant expr)
                |> Fuzz.oneOf
    in
    Test.describe "Elm.TypeInference"
        [ Test.describe "infer"
            [ Test.describe "simple single expressions"
                (singleExprs
                    |> List.map (\( label, input, output ) -> testExpr label input output)
                )
            , Test.fuzz singleExprFuzzer "e == (e)" <| \expr ->
            getExprType ("(" ++ expr ++ ")")
                |> Expect.equal (getExprType expr)
            ]

        -- TODO number later used with an int -> coerced into an int
        -- TODO number later used with a float -> coerced into a float
        ]
