module Tests exposing (..)

import Dict
import Elm.TypeInference.Error exposing (Error)
import Elm.TypeInference.Helpers exposing (TestError(..), getExprType)
import Elm.TypeInference.Type as Type
    exposing
        ( MonoType(..)
        , SuperType(..)
        , Type(..)
        )
import Expect
import String.ExtraExtra as String
import Test exposing (Test)


testExpr : ( String, Result Error Type -> Bool ) -> Test
testExpr ( exprCode, predicate ) =
    let
        trimmedExprCode =
            String.multilineInput exprCode
    in
    Test.test trimmedExprCode <|
        \() ->
            case getExprType trimmedExprCode of
                Err (CouldntInfer err) ->
                    predicate (Err err)
                        |> Expect.equal True
                        |> Expect.onFail ("Has failed in a bad way: " ++ Debug.toString err)

                Ok type_ ->
                    predicate (Ok type_)
                        |> Expect.equal True
                        |> Expect.onFail ("Has inferred a bad type: " ++ Type.toString type_)

                Err err ->
                    Expect.fail <| "Has failed (but shouldn't): " ++ Debug.toString err


is : MonoType -> Result Error Type -> Bool
is expected actual =
    Ok (Forall [] expected) == actual


fails : Result Error Type -> Bool
fails actual =
    case actual of
        Err _ ->
            True

        Ok _ ->
            False


isNumber : Result Error Type -> Bool
isNumber actual =
    case actual of
        Ok (Forall [] (TypeVar ( _, Number ))) ->
            True

        _ ->
            False


isList : (Result Error Type -> Bool) -> Result Error Type -> Bool
isList innerCheck actual =
    case actual of
        Ok (Forall [] (List inner)) ->
            innerCheck (Ok (Forall [] inner))

        _ ->
            False


isTuple : (Result Error Type -> Bool) -> (Result Error Type -> Bool) -> Result Error Type -> Bool
isTuple check1 check2 actual =
    case actual of
        Ok (Forall [] (Tuple t1 t2)) ->
            check1 (Ok (Forall [] t1))
                && check2 (Ok (Forall [] t2))

        _ ->
            False


isFunction : (Result Error Type -> Bool) -> (Result Error Type -> Bool) -> Result Error Type -> Bool
isFunction fromCheck toCheck actual =
    case actual of
        Ok (Forall [] (Function { from, to })) ->
            fromCheck (Ok (Forall [] from)) && toCheck (Ok (Forall [] to))

        _ ->
            False


isFunctionWithSignature : String -> Result Error Type -> Bool
isFunctionWithSignature signature actual =
    case actual of
        Ok ((Forall [] (Function { from, to })) as type_) ->
            let
                (Forall _ mono) =
                    Type.normalize type_

                actualSignature =
                    Type.monoTypeToString mono
            in
            signature == actualSignature

        _ ->
            False


isVar : Result Error Type -> Bool
isVar actual =
    case actual of
        Ok (Forall [] (TypeVar _)) ->
            True

        _ ->
            False


isRecord : List ( String, Result Error Type -> Bool ) -> Result Error Type -> Bool
isRecord fieldChecks actual =
    case actual of
        Ok (Forall [] (Record fields)) ->
            List.all
                (\( field, check ) ->
                    case Dict.get field fields of
                        Nothing ->
                            False

                        Just fieldType ->
                            check (Ok (Forall [] fieldType))
                )
                fieldChecks

        _ ->
            False


isExtensibleRecord : (Result Error Type -> Bool) -> List ( String, Result Error Type -> Bool ) -> Result Error Type -> Bool
isExtensibleRecord baseRecordCheck fieldChecks actual =
    case actual of
        Ok (Forall [] (ExtensibleRecord r)) ->
            baseRecordCheck (Ok (Forall [] r.type_))
                && List.all
                    (\( field, check ) ->
                        case Dict.get field r.fields of
                            Nothing ->
                                False

                            Just fieldType ->
                                check (Ok (Forall [] fieldType))
                    )
                    fieldChecks

        _ ->
            False


suite : Test
suite =
    let
        goodExprs : List ( String, Result Error Type -> Bool )
        goodExprs =
            [ ( "()", is Unit )
            , ( "123", isNumber )
            , ( "0x123", isNumber )
            , ( "42.0", is Float )
            , ( "-123", isNumber )
            , ( "-0x123", isNumber )
            , ( "-123.0", is Float )
            , ( "\"ABC\"", is String )
            , ( "'A'", is Char )
            , ( "(42.0)", is Float )
            , ( "('a', ())", is (Tuple Char Unit) )
            , ( "('a', (), 123.4)", is (Tuple3 Char Unit Float) )
            , ( "[1.0, 2.0, 3.0]", isList (is Float) )
            , ( "[1, 2, 3.0]", isList (is Float) )
            , ( "[1.0, 2, 3]", isList (is Float) )
            , ( "[1, 2, 3]", isList isNumber )
            , ( "[(1,'a'),(2,'b')]", isList (isTuple isNumber (is Char)) )
            , ( "\\x -> 1", isFunction isVar isNumber )
            , ( "\\x -> x", isFunctionWithSignature "#0 -> #0" )
            , ( "\\x y -> x", isFunctionWithSignature "#1 -> #0 -> #1" ) -- TODO weirdness
            , ( "\\x y -> y", isFunctionWithSignature "#0 -> #1 -> #1" )
            , ( "\\x y -> 1", isFunction isVar (isFunction isVar isNumber) )
            , ( "\\() -> 1", isFunction (is Unit) isNumber )
            , ( "\\x () -> 1", isFunction isVar (isFunction (is Unit) isNumber) )
            , ( "\\() x -> 1", isFunction (is Unit) (isFunction isVar isNumber) )
            , ( "{}", isRecord [] )
            , ( "{a = 1}", isRecord [ ( "a", isNumber ) ] )
            , ( "{a = 1, b = ()}", isRecord [ ( "a", isNumber ), ( "b", is Unit ) ] )
            , ( ".a", isFunction (isExtensibleRecord isVar [ ( "a", isVar ) ]) isVar )
            , ( "record.a", isVar ) -- in this example we can't say much more about the fact that `record` is `{ ? | a : ? }`
            , ( ".a {a = 1}", isNumber )
            , ( "(\\x -> x) 1", isNumber )
            , ( "(\\x y -> x) 1 2", isNumber )
            , ( "(\\x y -> x) 1", isFunction isVar isNumber )
            , ( "(\\x y -> y) 1", isFunction isVar isVar )
            , ( "let x = 1 in x", isNumber )
            , ( "let id x = x in id", isFunctionWithSignature "#0 -> #0" )

            --, ( """
            --    let
            --        x : Float
            --        x = 1
            --    in
            --    x
            --    """, is Float )
            -- , ( "let id x = x in (id 1, id ())", isTuple isNumber (is Unit) )
            -- , ( "if True then 1 else 2", isNumber ) -- TODO will need us to provide all the project deps as files
            -- , ( "let x = 1 in x + 1.0", is Float ) -- needs to know about `+`
            -- TODO check type annotations are checked in let
            -- TODO check type annotations are checked in top-level declarations
            -- TODO Application (List (ExprWith meta))
            -- TODO OperatorApplication String InfixDirection (ExprWith meta) (ExprWith meta)
            -- TODO FunctionOrValue ModuleName String
            -- TODO IfBlock (ExprWith meta) (ExprWith meta) (ExprWith meta)
            -- TODO PrefixOperator String
            -- TODO Operator String
            -- TODO LetExpression (LetBlock meta)
            -- TODO CaseExpression (CaseBlock meta)
            -- TODO RecordUpdateExpression (LocatedNode String) (List (LocatedNode (RecordSetter meta)))
            -- , ( "{ record | a = 123 }", isRecord [ ( "a", isNumber ) ] ) -- TODO needs `record` in scope
            -- TODO GLSLExpression String
            ]

        badExprs : List ( String, Result Error Type -> Bool )
        badExprs =
            [ ( "[1, ()]", fails )
            , ( "fn 1", fails )
            , ( "\\x -> y", fails )
            , ( "(\\x y -> x) 1 2 3", fails )
            , ( "let x = 1 in y", fails )
            ]
    in
    Test.describe "Elm.TypeInference"
        [ Test.describe "infer"
            [ Test.describe "good expressions" (List.map testExpr goodExprs)
            , Test.describe "bad expressions" (List.map testExpr badExprs)
            , Test.describe "e == (e)" <|
                List.map
                    (\( expr, _ ) ->
                        Test.test expr <|
                            \() ->
                                Result.map Type.normalize (getExprType ("(" ++ expr ++ ")"))
                                    |> Expect.equal (Result.map Type.normalize (getExprType expr))
                    )
                    goodExprs
            ]

        -- TODO number later used with an int -> coerced into an int
        -- TODO number later used with a float -> coerced into a float
        ]
