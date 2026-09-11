module Tests exposing (..)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.FullModuleName as FullModuleName
import Elm.Type
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.State as State
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type as Type
    exposing
        ( MonoType(..)
        , SuperType(..)
        , Type(..)
        , TypeVarStyle(..)
        )
import Elm.TypeInference.Type.External as ExternalType
import Elm.TypeInference.Unify as Unify
import Expect
import String.ExtraExtra
import Test exposing (Test)
import Tests.Elm.TypeInference.Fixture.ElmCore as CoreFixture
import Tests.Elm.TypeInference.Helpers
    exposing
        ( TestError(..)
        , getDeclType
        , getDeclTypeWithDeps
        , getExprType
        , getExprTypeWithDeps
        , inferMainModule
        )
import TypeLookupTable


testExpr : ( String, Result Error Type -> Bool ) -> Test
testExpr ( exprCode, predicate ) =
    let
        trimmedExprCode =
            String.ExtraExtra.multilineInput exprCode
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
                        |> Expect.onFail ("Has inferred a bad type: " ++ Type.toString (Type.normalize type_))

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


isMaybe : (Result Error Type -> Bool) -> Result Error Type -> Bool
isMaybe innerCheck actual =
    case actual of
        Ok (Forall [] (UserDefinedType { name, args })) ->
            case ( name, args ) of
                ( "Maybe", [ inner ] ) ->
                    innerCheck (Ok (Forall [] inner))

                _ ->
                    False

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


{-| Like `isExtensibleRecord`, but doesn't care about order of ext.record nesting
-}
isExtensibleRecordWithFields : List ( String, Result Error Type -> Bool ) -> Result Error Type -> Bool
isExtensibleRecordWithFields fieldChecks actual =
    let
        flatten : MonoType -> Dict String MonoType
        flatten mono =
            case mono of
                ExtensibleRecord r ->
                    Dict.union r.fields (flatten r.type_)

                _ ->
                    Dict.empty
    in
    case actual of
        Ok (Forall [] ((ExtensibleRecord _) as mono)) ->
            let
                allFields =
                    flatten mono
            in
            List.all
                (\( field, check ) ->
                    case Dict.get field allFields of
                        Nothing ->
                            False

                        Just fieldType ->
                            check (Ok (Forall [] fieldType))
                )
                fieldChecks

        _ ->
            False


type alias ShaderFields =
    { attributes : List ( String, MonoType )
    , uniforms : List ( String, MonoType )
    , varyings : List ( String, MonoType )
    }


emptyShader : ShaderFields
emptyShader =
    { attributes = []
    , uniforms = []
    , varyings = []
    }


isShader : ShaderFields -> Result Error Type -> Bool
isShader expected actual =
    actual
        == Ok
            (Forall []
                (WebGLShader
                    { attributes = Dict.fromList expected.attributes
                    , uniforms = Dict.fromList expected.uniforms
                    , varyings = Dict.fromList expected.varyings
                    }
                )
            )


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
            , ( "\\x y -> x", isFunctionWithSignature "#0 -> #1 -> #0" )
            , ( "\\x y -> y", isFunctionWithSignature "#0 -> #1 -> #1" )
            , ( "\\x y -> 1", isFunction isVar (isFunction isVar isNumber) )
            , ( "\\() -> 1", isFunction (is Unit) isNumber )
            , ( "\\x () -> 1", isFunction isVar (isFunction (is Unit) isNumber) )
            , ( "\\() x -> 1", isFunction (is Unit) (isFunction isVar isNumber) )
            , ( "\\(x, y) -> x", isFunction (isTuple isVar isVar) isVar )
            , ( "\\(x, y) -> y", isFunction (isTuple isVar isVar) isVar )
            , ( "{}", isRecord [] )
            , ( "{a = 1}", isRecord [ ( "a", isNumber ) ] )
            , ( "{a = 1, b = ()}", isRecord [ ( "a", isNumber ), ( "b", is Unit ) ] )
            , ( ".a", isFunction (isExtensibleRecord isVar [ ( "a", isVar ) ]) isVar )
            , ( "let record = { a = 1 } in record.a", isNumber )
            , ( "\\record -> record.a", isFunction (isExtensibleRecord isVar [ ( "a", isVar ) ]) isVar )
            , ( ".a {a = 1}", isNumber )
            , ( "(\\x -> x) 1", isNumber )
            , ( "(\\x y -> x) 1 2", isNumber )
            , ( "(\\x y -> x) 1", isFunction isVar isNumber )
            , ( "(\\x y -> y) 1", isFunction isVar isVar )
            , ( "let x = 1 in x", isNumber )
            , ( "let x = 1 in ()", is Unit )
            , ( "let id x = x in id", isFunctionWithSignature "#0 -> #0" )
            , ( "\\f x -> (f x).a", isFunction (isFunction isVar (isExtensibleRecord isVar [ ( "a", isVar ) ])) (isFunction isVar isVar) )
            , ( "case 1 of\n    1 -> 'a'\n    _ -> 'b'", is Char )
            , ( "case ('a', 'b') of\n    ( x, _ ) -> x", is Char )
            , -- Extensible record - two usages, final record must satisfy both
              ( "\\r -> (r.a, r.b)", isFunction (isExtensibleRecordWithFields [ ( "a", isVar ), ( "b", isVar ) ]) (isTuple isVar isVar) )
            , -- Extensible record works with more complex types
              ( "\\r -> ( r.a, [ r.a, 1.0 ] )", isFunction (isExtensibleRecordWithFields [ ( "a", is Float ) ]) (isTuple (is Float) (isList (is Float))) )
            , -- Let-polymorphism: `id` is used with two different types
              ( "let id x = x in (id 1, id ())", isTuple isNumber (is Unit) )
            , ( "let const x y = x in (const 1 'a', const () \"b\")", isTuple isNumber (is Unit) )
            , -- Nested generalization
              ( "let f x = x in let g y = f y in (g 1, g ())", isTuple isNumber (is Unit) )
            , -- Self-recursion doesn't block generalization
              ( "let loop x = loop x in (loop 1, loop ())", isTuple isVar isVar )
            , -- Annotations are trusted, not checked. We assume `elm make` passes.
              -- This allows having eg. `Float` instead of `number` literals.
              ( """
                let
                    x : Float
                    x = 1
                in
                x
                """
              , is Float
              )
            , -- Record update on a closed record stays closed (doesn't become extensible)
              ( """
                let
                    record = { a = 1, b = 'x' }
                in
                { record | a = 2.5 }
                """
              , isRecord [ ( "a", is Float ), ( "b", is Char ) ]
              )
            , ( """
                let
                    setA r = { r | a = 1.0 }
                in
                setA { a = 2, b = 'x' }
                """
              , isRecord [ ( "a", is Float ), ( "b", is Char ) ]
              )
            , ( "\\record -> { record | a = 1.0 }"
              , isFunction
                    (isExtensibleRecordWithFields [ ( "a", is Float ) ])
                    (isExtensibleRecordWithFields [ ( "a", is Float ) ])
              )

            -- Operator String is desugared away by Elm.Processing.process before we
            -- see it (Infer.elm maps it to impossibleExpr), so it's untestable here.
            ]

        badExprs : List ( String, Result Error Type -> Bool )
        badExprs =
            [ ( "[1, ()]", fails )
            , ( "fn 1", fails )
            , ( "\\x -> y", fails )
            , ( "(\\x y -> x) 1 2 3", fails )
            , ( "let x = 1 in y", fails )
            , ( "case 1 of\n    'a' -> 1\n    _ -> 2", fails ) -- pattern doesn't match scrutinee
            , ( "case 1 of\n    1 -> 'a'\n    _ -> 2", fails ) -- branch bodies disagree
            , ( "\\r -> ( [ r.a, 1.0 ], [ r.a, 'x' ] )", fails ) -- `r.a` forced to both Float and Char via row unification
            , ( "let a = (\\x -> x) 1 in x", fails ) -- inner scope must not leak
            , -- updating a field the record doesn't have
              ( """
                let
                    record = { a = 1 }
                in
                { record | b = 2 }
                """
              , fails
              )
            , -- updating a field with a different type
              ( """
                let
                    record = { a = 'x' }
                in
                { record | a = 1.0 }
                """
              , fails
              )
            , ( "(\\y -> (\\x -> x) y) x", fails )
            , ( "\\f -> (f 1, f ())", fails ) -- Lambdas don't generalize, only let-polymorphism does
            , ( "\\f -> let g = f in (g 1, g ())", fails )
            , ( "let f x = (f 1, f ()) in f", fails )
            ]
    in
    Test.describe "Elm.TypeInference"
        [ Test.describe "infer"
            [ Test.describe "good expressions" (List.map testExpr goodExprs)
            , Test.describe "bad expressions" (List.map testExpr badExprs)
            , Test.describe "subexpressions"
                [ Test.test "the `2` in `main = [1.0, 2]` is a Float" <|
                    \() ->
                        """module Main exposing (main)

main = [1.0, 2]
"""
                            |> inferMainModule
                            |> Result.map
                                (Tuple.second
                                    >> TypeLookupTable.get
                                        { start = { row = 3, column = 14 }
                                        , end = { row = 3, column = 15 }
                                        }
                                )
                            |> Expect.equal (Ok (Just (Forall [] Float)))
                , Test.test "a top-level function reports its function type, not its body's" <|
                    \() ->
                        """module Main exposing (main)

main x = x
"""
                            |> inferMainModule
                            |> Result.map
                                (Tuple.second
                                    >> TypeLookupTable.get
                                        { start = { row = 3, column = 1 }
                                        , end = { row = 3, column = 11 }
                                        }
                                    >> Maybe.map (Type.normalize >> Type.toString)
                                )
                            |> Expect.equal (Ok (Just "#0 -> #0"))
                ]
            , Test.describe "declarations other than functions"
                [ Test.test "a type alias and a custom type don't crash the inference" <|
                    \() ->
                        """module Main exposing (main)

type alias Foo =
    { a : Float }

type Bar
    = Baz
    | Qux Float

main = ()
"""
                            |> inferMainModule
                            |> Result.map (always ())
                            |> Expect.equal (Ok ())
                ]
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
        , unifyAliasSuite
        , bindingGroupSuite
        , dependenciesSuite
        , glslSuite
        ]


glslSuite : Test
glslSuite =
    let
        attr : List ( String, MonoType ) -> ShaderFields
        attr attributes =
            { emptyShader | attributes = attributes }
    in
    Test.describe "GLSL shaders"
        [ Test.describe "storage qualifiers"
            (List.map testExpr
                [ ( "[glsl|attribute vec3 a_position;|]"
                  , isShader { emptyShader | attributes = [ ( "a_position", ExternalType.vec3 ) ] }
                  )
                , ( "[glsl|uniform mat4 u_view;|]"
                  , isShader { emptyShader | uniforms = [ ( "u_view", ExternalType.mat4 ) ] }
                  )
                , ( "[glsl|varying vec2 v_texcoord;|]"
                  , isShader { emptyShader | varyings = [ ( "v_texcoord", ExternalType.vec2 ) ] }
                  )
                ]
            )
        , Test.describe "variable types"
            (List.map testExpr
                [ ( "[glsl|attribute vec2 x;|]", isShader (attr [ ( "x", ExternalType.vec2 ) ]) )
                , ( "[glsl|attribute vec3 x;|]", isShader (attr [ ( "x", ExternalType.vec3 ) ]) )
                , ( "[glsl|attribute vec4 x;|]", isShader (attr [ ( "x", ExternalType.vec4 ) ]) )
                , ( "[glsl|attribute mat4 x;|]", isShader (attr [ ( "x", ExternalType.mat4 ) ]) )
                , ( "[glsl|attribute sampler2d x;|]", isShader (attr [ ( "x", ExternalType.texture ) ]) )
                , ( "[glsl|attribute int x;|]", isShader (attr [ ( "x", Int ) ]) )
                , ( "[glsl|attribute float x;|]", isShader (attr [ ( "x", Float ) ]) )
                , -- we drop types we don't know:
                  ( "[glsl|attribute mat3 x;|]", isShader emptyShader )
                ]
            )
        , Test.describe "unification"
            (List.map testExpr
                [ ( "(\\x -> x) [glsl|attribute vec3 x;|]"
                  , isShader (attr [ ( "x", ExternalType.vec3 ) ])
                  )
                , -- Two shaders with the same declarations unify.
                  ( "[ [glsl|attribute vec3 x;|], [glsl|attribute vec3 x;|] ]"
                  , isList (isShader (attr [ ( "x", ExternalType.vec3 ) ]))
                  )
                , -- Shader records are closed
                  ( "[ [glsl|attribute vec3 x;|], [glsl|attribute vec3 y;|] ]", fails )
                , ( "[ [glsl|attribute vec3 x;|], [glsl|attribute vec2 x;|] ]", fails )
                , ( "[ [glsl|attribute vec3 x;|], [glsl|uniform vec3 x;|] ]", fails )
                , ( "[ [glsl|attribute vec3 x;|], 1 ]", fails )
                , ( "[glsl|attribute vec3 x;|] 1", fails )
                ]
            )
        ]


dependenciesSuite : Test
dependenciesSuite =
    let
        testWithCore : ( String, Result Error Type -> Bool ) -> Test
        testWithCore ( exprCode, predicate ) =
            Test.test exprCode <|
                \() ->
                    case getExprTypeWithDeps [ CoreFixture.core ] exprCode of
                        Err (CouldntInfer err) ->
                            predicate (Err err)
                                |> Expect.equal True
                                |> Expect.onFail ("Has failed in a bad way: " ++ Debug.toString err)

                        Ok type_ ->
                            predicate (Ok type_)
                                |> Expect.equal True
                                |> Expect.onFail ("Has inferred a bad type: " ++ Type.toString (Type.normalize type_))

                        Err err ->
                            Expect.fail <| "Has failed (but shouldn't): " ++ Debug.toString err
    in
    Test.describe "3rd party dependency types (using an elm/core fixture)"
        [ Test.describe "good expressions"
            (List.map testWithCore
                [ ( "if True then 1 else 2", isNumber )
                , ( "let x = 1 in x + 1.0", is Float )
                , ( "Just 1", isMaybe isNumber )
                , ( "List.map (\\x -> x + 1) [ 1, 2, 3 ]", isList isNumber )
                , ( "List.map (\\x -> x + 1.0) [ 1.0 ]", isList (is Float) )
                , ( "1 == 2", is Bool )
                , ( "1 :: [ 2 ]", isList isNumber ) -- (::) comes from the implicit `import List exposing (List, (::))`
                ]
            )
        , Test.describe "bad expressions"
            (List.map testWithCore
                [ ( "if 1 then 1 else 2", fails ) -- condition must be Bool
                , ( "True + 1", fails ) -- Bool isn't a number
                ]
            )
        , Test.test "a module name exposed by one package and shipped internally by another stays two distinct types" <|
            \() ->
                let
                    pkgA =
                        { name = "authorA/pkg-a"
                        , dependencies = []
                        , modules =
                            [ { name = "ModuleA"
                              , comment = ""
                              , unions = []
                              , aliases = []
                              , values =
                                    [ { name = "consume"
                                      , comment = ""
                                      , tipe =
                                            Elm.Type.Lambda
                                                (Elm.Type.Type "Char.Extra.Classification" [])
                                                (Elm.Type.Type "Basics.Int" [])
                                      }
                                    ]
                              , binops = []
                              }
                            ]
                        }

                    pkgB =
                        { name = "authorB/pkg-b"
                        , dependencies = []
                        , modules =
                            [ { name = "Char.Extra"
                              , comment = ""
                              , unions =
                                    [ { name = "Classification"
                                      , comment = ""
                                      , args = []
                                      , tags = [ ( "Alpha", [] ) ]
                                      }
                                    ]
                              , aliases = []
                              , values = []
                              , binops = []
                              }
                            ]
                        }

                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (main)

import ModuleA
import Char.Extra

main = ModuleA.consume Char.Extra.Alpha
"""
                in
                getDeclTypeWithDeps [ pkgA, pkgB ] modules [ "Main" ] "main"
                    |> Result.map (always ())
                    |> Expect.err
        , Test.test "ambiguity between two same-named exposed modules across packages (miniBill/elm-ui-with-context's Element) is an error" <|
            \() ->
                let
                    elmUi =
                        { name = "mdgriffith/elm-ui"
                        , dependencies = []
                        , modules =
                            [ { name = "Element"
                              , comment = ""
                              , unions = [ { name = "Element", comment = "", args = [ "msg" ], tags = [] } ]
                              , aliases = []
                              , values = []
                              , binops = []
                              }
                            ]
                        }

                    styleElements =
                        { name = "mdgriffith/style-elements"
                        , dependencies = []
                        , modules =
                            [ { name = "Element"
                              , comment = ""
                              , unions = [ { name = "Element", comment = "", args = [ "msg" ], tags = [] } ]
                              , aliases = []
                              , values = []
                              , binops = []
                              }
                            ]
                        }

                    elmUiWithContext =
                        { name = "miniBill/elm-ui-with-context"
                        , dependencies = [ "mdgriffith/elm-ui", "mdgriffith/style-elements" ]
                        , modules =
                            [ { name = "Element.WithContext"
                              , comment = ""
                              , unions = []
                              , aliases = []
                              , values =
                                    [ { name = "toElement"
                                      , comment = ""
                                      , tipe =
                                            Elm.Type.Lambda
                                                (Elm.Type.Type "Element.Element" [ Elm.Type.Var "msg" ])
                                                (Elm.Type.Type "Basics.Int" [])
                                      }
                                    ]
                              , binops = []
                              }
                            ]
                        }

                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (main)

main = 1
"""
                in
                case getDeclTypeWithDeps [ elmUi, styleElements, elmUiWithContext ] modules [ "Main" ] "main" of
                    Err (CouldntInfer (AmbiguousModuleOwner { moduleName, possiblePackages })) ->
                        Expect.all
                            [ \_ -> moduleName |> Expect.equal "Element"
                            , \_ -> possiblePackages |> Expect.equal [ "mdgriffith/elm-ui", "mdgriffith/style-elements" ]
                            ]
                            ()

                    other ->
                        Expect.fail ("Expected AmbiguousModuleOwner, got: " ++ Debug.toString other)
        , Test.test "ambiguity between two packages both defining Element.text is an error" <|
            \() ->
                let
                    elmUi =
                        { name = "mdgriffith/elm-ui"
                        , dependencies = []
                        , modules =
                            [ { name = "Element"
                              , comment = ""
                              , unions = []
                              , aliases = []
                              , values =
                                    [ { name = "text"
                                      , comment = ""
                                      , tipe =
                                            Elm.Type.Lambda
                                                (Elm.Type.Type "String.String" [])
                                                (Elm.Type.Type "Basics.Int" [])
                                      }
                                    ]
                              , binops = []
                              }
                            ]
                        }

                    styleElements =
                        { name = "mdgriffith/style-elements"
                        , dependencies = []
                        , modules =
                            [ { name = "Element"
                              , comment = ""
                              , unions = []
                              , aliases = []
                              , values =
                                    [ { name = "text"
                                      , comment = ""
                                      , tipe =
                                            Elm.Type.Lambda
                                                (Elm.Type.Type "String.String" [])
                                                (Elm.Type.Type "Basics.Int" [])
                                      }
                                    ]
                              , binops = []
                              }
                            ]
                        }

                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (main)

import Element

main = Element.text "hi"
"""
                in
                case getDeclTypeWithDeps [ elmUi, styleElements ] modules [ "Main" ] "main" of
                    Err (CouldntInfer (AmbiguousModuleOwner { moduleName, possiblePackages })) ->
                        Expect.all
                            [ \_ -> moduleName |> Expect.equal "Element"
                            , \_ -> possiblePackages |> Expect.equal [ "mdgriffith/elm-ui", "mdgriffith/style-elements" ]
                            ]
                            ()

                    other ->
                        Expect.fail ("Expected AmbiguousModuleOwner, got: " ++ Debug.toString other)
        , Test.test "ambiguity between two packages both defining an Element type is an error" <|
            \() ->
                let
                    elementUnion =
                        { name = "Element", comment = "", args = [ "msg" ], tags = [] }

                    elmUi =
                        { name = "mdgriffith/elm-ui"
                        , dependencies = []
                        , modules =
                            [ { name = "Element"
                              , comment = ""
                              , unions = [ elementUnion ]
                              , aliases = []
                              , values = []
                              , binops = []
                              }
                            ]
                        }

                    styleElements =
                        { name = "mdgriffith/style-elements"
                        , dependencies = []
                        , modules =
                            [ { name = "Element"
                              , comment = ""
                              , unions = [ elementUnion ]
                              , aliases = []
                              , values = []
                              , binops = []
                              }
                            ]
                        }

                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (main)

import Element

thing : Element.Element msg -> Int
thing _ = 1

main = 1
"""
                in
                case getDeclTypeWithDeps [ elmUi, styleElements ] modules [ "Main" ] "thing" of
                    Err (CouldntInfer (AmbiguousModuleOwner { moduleName, possiblePackages })) ->
                        Expect.all
                            [ \_ -> moduleName |> Expect.equal "Element"
                            , \_ -> possiblePackages |> Expect.equal [ "mdgriffith/elm-ui", "mdgriffith/style-elements" ]
                            ]
                            ()

                    other ->
                        Expect.fail ("Expected AmbiguousModuleOwner, got: " ++ Debug.toString other)
        , Test.test "`import Platform.Cmd as Cmd exposing (Cmd)` is implicit" <|
            \() ->
                let
                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (main)

main : Cmd msg
main = Cmd.none
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "main"
                    |> Result.map (Type.normalize >> Type.toString)
                    |> Expect.equal (Ok "Platform.Cmd.Cmd #0")
        , Test.test "custom operator" <|
            \() ->
                let
                    pkgCustomOps =
                        { name = "author/custom-ops"
                        , dependencies = [ "elm/core" ]
                        , modules =
                            [ { name = "CustomOps"
                              , comment = ""
                              , unions = []
                              , aliases = []
                              , values = []
                              , binops =
                                    [ { name = "|="
                                      , comment = ""
                                      , tipe =
                                            Elm.Type.Lambda
                                                (Elm.Type.Type "Basics.Int" [])
                                                (Elm.Type.Lambda
                                                    (Elm.Type.Type "Basics.Int" [])
                                                    (Elm.Type.Type "Basics.Int" [])
                                                )
                                      , associativity = Elm.Docs.Left
                                      , precedence = 5
                                      }
                                    ]
                              }
                            ]
                        }

                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (main)

import CustomOps exposing ((|=))

main = 1 |= 2
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core, pkgCustomOps ] modules [ "Main" ] "main"
                    |> Result.map (Ok >> is Int)
                    |> Expect.equal (Ok True)
        , Test.test "`Basics.Bool` unifies with `True` (qualified primitive)" <|
            \() ->
                let
                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (x)

x : Basics.Bool
x = True
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "x"
                    |> Result.map (Ok >> is Bool)
                    |> Expect.equal (Ok True)
        , Test.test "`x : B.Int` (aliased import) unifies with an Int literal" <|
            \() ->
                let
                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (x)

import Basics as B

x : B.Int
x = 1
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "x"
                    |> Result.map (Ok >> is Int)
                    |> Expect.equal (Ok True)
        , Test.test "`x : Char.Char` unifies with a Char literal" <|
            \() ->
                let
                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (x)

x : Char.Char
x = 'a'
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "x"
                    |> Result.map (Ok >> is Char)
                    |> Expect.equal (Ok True)
        ]


bindingGroupSuite : Test
bindingGroupSuite =
    Test.describe "binding groups"
        [ Test.test "top-level declaration order doesn't matter" <|
            \() ->
                let
                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (main)

main = helper 1
helper x = x
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map (Type.normalize >> Type.toString)
                    |> Expect.equal (Ok "number#0")

        {- , Test.todo """
           mutual recursion between two top-level declarations

           module Main exposing (main)

           isEven n = if n == 0 then True else isOdd n
           isOdd n = if n == 0 then False else isEven n
           main = isEven
           """
        -}
        , Test.test "a top-level var is usable, at its own inferred type, across modules" <|
            \() ->
                let
                    modules =
                        Dict.fromList
                            [ ( [ "Other" ]
                              , """
module Other exposing (identity)

identity x = x
"""
                              )
                            , ( [ "Main" ]
                              , """
module Main exposing (main)

import Other

main = Other.identity 1
"""
                              )
                            ]
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map (Ok >> isNumber)
                    |> Expect.equal (Ok True)
        , Test.test "a project's own custom-type constructor is usable in an expression" <|
            \() ->
                let
                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (main)

type Box a = Box a
main = Box 1
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map (Type.normalize >> Type.toString)
                    |> Expect.equal (Ok "Main.Box number#0")
        , Test.test "a custom operator declaration is type-checked (infix usage)" <|
            \() ->
                let
                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (main)

myAdd a b = a

infix left 6 (+) = myAdd

main = 1 + 2
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map (Ok >> isNumber)
                    |> Expect.equal (Ok True)
        , Test.test "a custom operator declaration is type-checked (prefix usage)" <|
            \() ->
                let
                    modules =
                        Dict.singleton [ "Main" ]
                            """
module Main exposing (main)

myAdd a b = a

infix left 6 (+) = myAdd

main = (+) 1 2
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map (Ok >> isNumber)
                    |> Expect.equal (Ok True)
        , Test.test "a custom operator declaration aliasing an imported function is type-checked" <|
            \() ->
                let
                    modules =
                        Dict.fromList
                            [ ( [ "Other" ]
                              , """
module Other exposing (myAdd)

myAdd a b = a
"""
                              )
                            , ( [ "Main" ]
                              , """
module Main exposing (main)

import Other exposing (myAdd)

infix left 6 (+) = myAdd

main = 1 + 2
"""
                              )
                            ]
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map (Ok >> isNumber)
                    |> Expect.equal (Ok True)
        ]


mainModule =
    FullModuleName.fromModuleName_ [ "Main" ]


var : Int -> MonoType
var n =
    TypeVar ( Generated n, Normal )


runUnify : Dict ( Type.PackageName, FullModuleName.FullModuleName, String ) Unify.TypeAlias -> List ( MonoType, MonoType ) -> Result Error SubstitutionMap.SubstitutionMap
runUnify typeAliases eqs =
    Unify.unifyMany typeAliases eqs
        |> State.run (State.init Dict.empty)
        |> Tuple.first


{-| Temporary suite before we refactor/fix `Type.fromTypeAnnotation`'s
unqualified-name resolution bug.
-}
unifyAliasSuite : Test
unifyAliasSuite =
    let
        pairAlias : Unify.TypeAlias
        pairAlias =
            { args = [ "a" ]
            , type_ = Tuple (TypeVar ( Named "a", Normal )) (TypeVar ( Named "a", Normal ))
            }

        typeAliases =
            Dict.singleton ( "", mainModule, "Pair" ) pairAlias

        pairOf : MonoType -> MonoType
        pairOf t =
            UserDefinedType { package = "", moduleName = mainModule, name = "Pair", args = [ t ] }

        run : List ( MonoType, MonoType ) -> Result Error SubstitutionMap.SubstitutionMap
        run =
            runUnify typeAliases
    in
    Test.describe "Unify: type alias expansion"
        [ Test.test "a Pair Float unifies with (Float, Float)" <|
            \() ->
                run [ ( pairOf Float, Tuple (var 0) (var 1) ) ]
                    |> Result.map (\subst -> SubstitutionMap.substituteMono subst (Tuple (var 0) (var 1)))
                    |> Expect.equal (Ok (Tuple Float Float))
        , Test.test "two different uses of the same alias don't leak into each other" <|
            \() ->
                run
                    [ ( pairOf Float, Tuple (var 0) (var 1) )
                    , ( pairOf Char, Tuple (var 2) (var 3) )
                    ]
                    |> Result.map
                        (\subst ->
                            ( SubstitutionMap.substituteMono subst (var 0)
                            , SubstitutionMap.substituteMono subst (var 2)
                            )
                        )
                    |> Expect.equal (Ok ( Float, Char ))
        , Test.test "a Pair Float does not unify with (Float, Char)" <|
            \() ->
                run [ ( pairOf Float, Tuple Float Char ) ]
                    |> Result.map (always ())
                    |> Expect.err
        ]


importedTypeReExposedFromDependencySuite : Test
importedTypeReExposedFromDependencySuite =
    let
        setModule : Elm.Docs.Module
        setModule =
            { name = "Set"
            , comment = ""
            , unions = [ { name = "Set", comment = "", args = [ "a" ], tags = [] } ]
            , aliases = []
            , values =
                [ { name = "fromList"
                  , comment = ""
                  , tipe =
                        Elm.Type.Lambda
                            (Elm.Type.Type "List.List" [ Elm.Type.Var "comparable" ])
                            (Elm.Type.Type "Set.Set" [ Elm.Type.Var "comparable" ])
                  }
                ]
            , binops = []
            }

        core =
            { name = "elm/core"
            , dependencies = []
            , modules = [ setModule ]
            }

        modules =
            Dict.singleton [ "Main" ] <|
                String.ExtraExtra.multilineInput """
                module Main exposing (allowedNames)

                import Set exposing (Set)

                allowedNames : Set String
                allowedNames =
                    Set.fromList []
                """
    in
    Test.test "import Set exposing (Set) then using Set, gets inferred correctly" <|
        \() ->
            getDeclTypeWithDeps [ core ] modules [ "Main" ] "allowedNames"
                |> Expect.equal
                    (Ok
                        (Forall []
                            (UserDefinedType
                                { package = "elm/core"
                                , moduleName = FullModuleName.fromModuleName_ [ "Set" ]
                                , name = "Set"
                                , args = [ String ]
                                }
                            )
                        )
                    )


infiniteLoopRegression : Test
infiniteLoopRegression =
    let
        modules =
            Dict.singleton [ "Main" ] <|
                String.ExtraExtra.multilineInput """
                module Main exposing (update)

                type alias Window =
                    { pid : Int
                    , position : Int
                    }

                update : List Window -> List Window
                update windows =
                    let
                        updatePosition window =
                            if window.pid == 1 then
                                { window | position = window.position + 1 }

                            else
                                window
                    in
                    List.map updatePosition windows
                """
    in
    Test.test "infinite loop for extensible records - regression test" <|
        \() ->
            getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "update"
                |> Result.map (Type.normalize >> Type.toString)
                |> Expect.equal (Ok "List Main.Window -> List Main.Window")
