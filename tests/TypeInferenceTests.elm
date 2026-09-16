module TypeInferenceTests exposing (suite)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Type
import Elm.TypeInference exposing (Dependency)
import Elm.TypeInference.Error exposing (Error, ErrorDetails(..))
import Elm.TypeInference.State as State
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type as Type exposing (PackageName, Type(..))
import Elm.TypeInference.Type.Internal as TypeI exposing (MonoType)
import Elm.TypeInference.TypeVar as TypeVar exposing (TypeVar)
import Elm.TypeInference.Unify as Unify exposing (TypeAlias)
import Elm.TypeInference.VarSet as VarSet
import Expect
import String.ExtraExtra
import Test exposing (Test)
import Tests.Elm.TypeInference.Fixture.ElmCore as CoreFixture
import Tests.Elm.TypeInference.Helpers
    exposing
        ( TestError(..)
        , getDeclType
        , getDeclTypeWithDeps
        , getDeclTypeWithDirectAndDeps
        , getExprType
        , getExprTypeWithDeps
        , inferMainModule
        )
import TypeLookupTable


suite : Test
suite =
    Test.describe "Elm.TypeInference"
        [ inferSuite
        , parenthesizedTest
        , unifyAliasSuite
        , comparableAliasedTupleRegression
        , substitutionMapCompressionSuite
        , linkToRankSuite
        , composeCycleRegression
        , instantiateIdCollisionRegression
        , bindingGroupSuite
        , dependenciesSuite
        , glslSuite
        , shaderAnnotationSuite
        , largeInputsSuite
        , importedTypeInferredProperly
        , infiniteLoopRegression
        , aliasParamNameCollisionRegression
        , recordConstructorFunctionRegression
        , unionConstructorReexposeRegression
        , recordConstructorReexposeRegression
        , unexposedUnionConstructorIsntFound
        , importWithSpecificExposes
        , duplicateImportAliasRegression
        , extensibleRecordRegression
        , annotationsCheckedAgainstBodiesSuite
        , rangeContractSuite
        , publicBoundarySuite
        , publicSurfaceLeakSuite
        ]


testExpr : ( String, Result Error Type -> Bool ) -> Test
testExpr ( exprCode, predicate ) =
    let
        trimmedExprCode : String
        trimmedExprCode =
            String.ExtraExtra.multilineInput exprCode
    in
    Test.test trimmedExprCode <| \() ->
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


is : Type -> Result Error Type -> Bool
is expected actual =
    Ok expected == actual


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
        Ok (Type.TypeVar name) ->
            name
                |> String.startsWith "number"

        _ ->
            False


isList :
    (Result Error Type -> Bool)
    -> Result Error Type
    -> Bool
isList innerCheck actual =
    case actual of
        Ok (Type.List inner) ->
            innerCheck (Ok inner)

        _ ->
            False


isMaybe :
    (Result Error Type -> Bool)
    -> Result Error Type
    -> Bool
isMaybe innerCheck actual =
    case actual of
        Ok (Type.Named { name, arguments }) ->
            case ( name, arguments ) of
                ( "Maybe", [ inner ] ) ->
                    innerCheck (Ok inner)

                _ ->
                    False

        _ ->
            False


isTuple :
    (Result Error Type -> Bool)
    -> (Result Error Type -> Bool)
    -> Result Error Type
    -> Bool
isTuple check1 check2 actual =
    case actual of
        Ok (Type.Tuple2 t1 t2) ->
            check1 (Ok t1)
                && check2 (Ok t2)

        _ ->
            False


isFunction :
    (Result Error Type -> Bool)
    -> (Result Error Type -> Bool)
    -> Result Error Type
    -> Bool
isFunction fromCheck toCheck actual =
    case actual of
        Ok (Type.Function { from, to }) ->
            fromCheck (Ok from) && toCheck (Ok to)

        _ ->
            False


isFunctionWithSignature : String -> Result Error Type -> Bool
isFunctionWithSignature signature actual =
    case actual of
        Ok ((Type.Function _) as type_) ->
            let
                actualSignature : String
                actualSignature =
                    Type.toString type_
            in
            signature == actualSignature

        _ ->
            False


isVar : Result Error Type -> Bool
isVar actual =
    case actual of
        Ok (Type.TypeVar _) ->
            True

        _ ->
            False


isRecord :
    List ( String, Result Error Type -> Bool )
    -> Result Error Type
    -> Bool
isRecord fieldChecks actual =
    case actual of
        Ok (Type.Record { fields }) ->
            List.all
                (\( field, check ) ->
                    case Dict.get field fields of
                        Nothing ->
                            False

                        Just fieldType ->
                            check (Ok fieldType)
                )
                fieldChecks

        _ ->
            False


isExtensibleRecord :
    List ( String, Result Error Type -> Bool )
    -> Result Error Type
    -> Bool
isExtensibleRecord fieldChecks actual =
    case actual of
        Ok (Type.ExtensibleRecord r) ->
            List.all
                (\( field, check ) ->
                    case Dict.get field r.fields of
                        Nothing ->
                            False

                        Just fieldType ->
                            check (Ok fieldType)
                )
                fieldChecks

        _ ->
            False


{-| Like `isExtensibleRecord`, but doesn't care about order of ext.record nesting
-}
isExtensibleRecordWithFields :
    List ( String, Result Error Type -> Bool )
    -> Result Error Type
    -> Bool
isExtensibleRecordWithFields fieldChecks actual =
    case actual of
        Ok (Type.ExtensibleRecord er) ->
            List.all
                (\( field, check ) ->
                    case Dict.get field er.fields of
                        Nothing ->
                            False

                        Just fieldType ->
                            check (Ok fieldType)
                )
                fieldChecks

        _ ->
            False


type alias ShaderFields =
    { attributes : List ( String, Type )
    , uniforms : List ( String, Type )
    , varyings : List ( String, Type )
    }


emptyShader : ShaderFields
emptyShader =
    { attributes = []
    , uniforms = []
    , varyings = []
    }


isShader : ShaderFields -> Result Error Type -> Bool
isShader expected actual =
    case actual of
        Ok (Type.WebGLShader shader) ->
            isShaderSet expected.attributes shader.attributesFields
                && isShaderSet expected.uniforms shader.uniformsFields
                && isShaderSet expected.varyings shader.varyingsFields

        _ ->
            False


isShaderSet : List ( String, Type ) -> Dict String Type -> Bool
isShaderSet expected actual =
    case expected of
        [] ->
            True

        _ ->
            actual == Dict.fromList expected


goodExprs : List ( String, Result Error Type.Type -> Bool )
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
    , ( "('a', ())", is (Type.Tuple2 Char Unit) )
    , ( "('a', (), 123.4)", is (Type.Tuple3 Char Unit Float) )
    , ( "[1.0, 2.0, 3.0]", isList (is Float) )
    , ( "[1, 2, 3.0]", isList (is Float) )
    , ( "[1.0, 2, 3]", isList (is Float) )
    , ( "[1, 2, 3]", isList isNumber )
    , ( "[(1,'a'),(2,'b')]", isList (isTuple isNumber (is Char)) )
    , ( "\\x -> 1", isFunction isVar isNumber )
    , ( "\\x -> x", isFunctionWithSignature "a -> a" )
    , ( "\\x y -> x", isFunctionWithSignature "a -> b -> a" )
    , ( "\\x y -> y", isFunctionWithSignature "a -> b -> b" )
    , ( "\\x y -> 1", isFunction isVar (isFunction isVar isNumber) )
    , ( "\\() -> 1", isFunction (is Unit) isNumber )
    , ( "\\x () -> 1", isFunction isVar (isFunction (is Unit) isNumber) )
    , ( "\\() x -> 1", isFunction (is Unit) (isFunction isVar isNumber) )
    , ( "\\(x, y) -> x", isFunction (isTuple isVar isVar) isVar )
    , ( "\\(x, y) -> y", isFunction (isTuple isVar isVar) isVar )
    , ( "{}", isRecord [] )
    , ( "{a = 1}", isRecord [ ( "a", isNumber ) ] )
    , ( "{a = 1, b = ()}", isRecord [ ( "a", isNumber ), ( "b", is Unit ) ] )
    , ( ".a", isFunction (isExtensibleRecord [ ( "a", isVar ) ]) isVar )
    , ( "let record = { a = 1 } in record.a", isNumber )
    , ( "\\record -> record.a", isFunction (isExtensibleRecord [ ( "a", isVar ) ]) isVar )
    , ( ".a {a = 1}", isNumber )
    , ( "(\\x -> x) 1", isNumber )
    , ( "(\\x y -> x) 1 2", isNumber )
    , ( "(\\x y -> x) 1", isFunction isVar isNumber )
    , ( "(\\x y -> y) 1", isFunction isVar isVar )
    , ( "let x = 1 in x", isNumber )
    , ( "let x = 1 in ()", is Unit )
    , ( "let id x = x in id", isFunctionWithSignature "a -> a" )
    , ( "\\f x -> (f x).a", isFunction (isFunction isVar (isExtensibleRecord [ ( "a", isVar ) ])) (isFunction isVar isVar) )
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
    , -- elm-visualization Force example
      ( "let ent = {x=1,y=2} in { ent | x = ent.x, y = ent.y }", isRecord [ ( "x", isNumber ), ( "y", isNumber ) ] )
    , ( "\\ent -> { ent | x = ent.x, y = ent.y }"
      , isFunction
            (isExtensibleRecordWithFields [ ( "x", isVar ), ( "y", isVar ) ])
            (isExtensibleRecordWithFields [ ( "x", isVar ), ( "y", isVar ) ])
      )
    , ( "(\\ent -> { ent | x = ent.x, y = ent.y }) {x = 1, y = 2}"
      , isRecord [ ( "x", isNumber ), ( "y", isNumber ) ]
      )
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
    ]


badExprs : List ( String, Result Error Type.Type -> Bool )
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


inferSuite : Test
inferSuite =
    Test.describe "infer"
        [ Test.describe "good expressions" (List.map testExpr goodExprs)
        , Test.describe "bad expressions" (List.map testExpr badExprs)
        , subexpressionsSuite
        ]


parenthesizedTest : Test
parenthesizedTest =
    Test.describe "e == (e)" <|
        List.map
            (\( expr, _ ) ->
                Test.test expr <| \() ->
                getExprType ("(" ++ expr ++ ")")
                    |> Expect.equal (getExprType expr)
            )
            goodExprs


subexpressionsSuite : Test
subexpressionsSuite =
    Test.describe "subexpressions"
        [ Test.test "the `2` in `main = [1.0, 2]` is a Float" <| \() ->
        """module Main exposing (main)

main = [1.0, 2]"""
                    |> inferMainModule
                    |> Result.map
                        (Tuple.second
                            >> TypeLookupTable.get
                                { start = { row = 3, column = 14 }
                                , end = { row = 3, column = 15 }
                                }
                        )
                    |> Expect.equal (Ok (Just Float))
        , Test.test "a top-level function reports its function type, not its body's" <| \() ->
        """module Main exposing (main)

main x = x"""
                    |> inferMainModule
                    |> Result.map
                        (Tuple.second
                            >> TypeLookupTable.get
                                { start = { row = 3, column = 1 }
                                , end = { row = 3, column = 11 }
                                }
                            >> Maybe.map Type.toString
                        )
                    |> Expect.equal (Ok (Just "a -> a"))
        ]


rangeContractSuite : Test
rangeContractSuite =
    Test.describe "TypeLookupTable ranges"
        [ Test.test "exact declaration range hits, shifted range misses" <| \() ->
        --     123456789
        --     main = 1
        --     ^^^^^^^^ exact declaration range (cols 1-9): hits
        --      ^^^^^^^ shifted by one column (cols 2-9): misses
        case inferMainModule """module Main exposing (main)

main = 1""" of
                    Err err ->
                        Expect.fail ("Should infer: " ++ Debug.toString err)

                    Ok ( _, table ) ->
                        Expect.all
                            [ \_ ->
                                TypeLookupTable.get
                                    { start = { row = 3, column = 1 }
                                    , end = { row = 3, column = 9 }
                                    }
                                    table
                                    |> Expect.notEqual Nothing
                            , \_ ->
                                TypeLookupTable.get
                                    { start = { row = 3, column = 2 }
                                    , end = { row = 3, column = 9 }
                                    }
                                    table
                                    |> Expect.equal Nothing
                            ]
                            ()
        , Test.test "record-literal field-name nodes carry the field value's type" <| \() ->
        --     00000000011111111112222222
        --     12345678901234567890123456
        --     main = { a = 1, b = 'x' }
        --              ^             field `a` (cols 10-11)
        --                     ^      field `b` (cols 17-18; value `'x'` is cols 21-24)
        case """module Main exposing (main)

main = { a = 1, b = 'x' }""" |> inferMainModule of
                    Err err ->
                        Expect.fail ("Should infer: " ++ Debug.toString err)

                    Ok ( _, table ) ->
                        Expect.all
                            [ \_ ->
                                TypeLookupTable.get
                                    { start = { row = 3, column = 10 }
                                    , end = { row = 3, column = 11 }
                                    }
                                    table
                                    |> isNumberLike
                                    |> Expect.equal True
                            , \_ ->
                                TypeLookupTable.get
                                    { start = { row = 3, column = 17 }
                                    , end = { row = 3, column = 18 }
                                    }
                                    table
                                    |> Expect.equal (Just Char)
                            ]
                            ()
        , Test.test "record-update field-name nodes carry the field value's type" <| \() ->
        --     000000000111111111122222222
        --     123456789012345678901234567
        --     main rec = { rec | a = 1 }
        --                        ^       field `a` (cols 20-21; value `1` is col 24)
        case """module Main exposing (main)

main rec = { rec | a = 1 }""" |> inferMainModule of
                    Err err ->
                        Expect.fail ("Should infer: " ++ Debug.toString err)

                    Ok ( _, table ) ->
                        TypeLookupTable.get
                            { start = { row = 3, column = 20 }
                            , end = { row = 3, column = 21 }
                            }
                            table
                            |> isNumberLike
                            |> Expect.equal True
        , Test.test "signature node and signature name node share the declared type" <| \() ->
        --     00000000011
        --     12345678901
        --     main : Int
        --     ^^^^^^^^^^ signature node (row 3, cols 1-11)
        --     ^^^^       signature name node (row 3, cols 1-5)
        --     main = 1
        case """module Main exposing (main)

main : Int
main = 1""" |> inferMainModule of
                    Err err ->
                        Expect.fail ("Should infer: " ++ Debug.toString err)

                    Ok ( _, table ) ->
                        Expect.all
                            [ \_ ->
                                TypeLookupTable.get
                                    { start = { row = 3, column = 1 }
                                    , end = { row = 3, column = 11 }
                                    }
                                    table
                                    |> Expect.equal (Just Int)
                            , \_ ->
                                TypeLookupTable.get
                                    { start = { row = 3, column = 1 }
                                    , end = { row = 3, column = 5 }
                                    }
                                    table
                                    |> Expect.equal (Just Int)
                            ]
                            ()
        , Test.test "custom-type declaration nodes have no entry" <| \() ->
        --     000000000111111
        --     123456789012345
        --     type Box = Box
        --     ^^^^^^^^^^^^^^ declaration node (row 3, cols 1-15): misses
        """module Main exposing (main)

type Box = Box

main = 1"""
                    |> inferMainModule
                    |> Result.map
                        (Tuple.second
                            >> TypeLookupTable.get
                                { start = { row = 3, column = 1 }
                                , end = { row = 3, column = 15 }
                                }
                        )
                    |> Expect.equal (Ok Nothing)
        , Test.test "type-alias declaration nodes have no entry" <| \() ->
        --     00000000011111111112222222222
        --     12345678901234567890123456789
        --     type alias Foo = { a : Int }
        --     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ declaration node (row 3, cols 1-29): misses
        """module Main exposing (main)

type alias Foo = { a : Int }

main = 1"""
                    |> inferMainModule
                    |> Result.map
                        (Tuple.second
                            >> TypeLookupTable.get
                                { start = { row = 3, column = 1 }
                                , end = { row = 3, column = 29 }
                                }
                        )
                    |> Expect.equal (Ok Nothing)
        ]


publicBoundarySuite : Test
publicBoundarySuite =
    Test.describe "Elm.TypeInference.Type.Internal.toPublicType"
        [ Test.test "an extensible record with a concrete closed tail collapses to a closed Record" <| \() ->
        TypeI.toPublicType
            { alreadyNormalized = False }
            (TypeI.ExtensibleRecord
                { extensionTypevar = TypeI.Record { fields = Dict.singleton "b" TypeI.Char }
                , fields = Dict.singleton "a" TypeI.Int
                }
            )
            |> Expect.equal
                (Type.Record { fields = Dict.fromList [ ( "a", Type.Int ), ( "b", Type.Char ) ] })
        , Test.test "a nested extensible chain flattens without losing fields" <| \() ->
        TypeI.toPublicType
            { alreadyNormalized = False }
            (TypeI.ExtensibleRecord
                { extensionTypevar =
                    TypeI.ExtensibleRecord
                        { extensionTypevar = TypeI.Record { fields = Dict.singleton "c" TypeI.Bool }
                        , fields = Dict.singleton "b" TypeI.Char
                        }
                , fields = Dict.singleton "a" TypeI.Int
                }
            )
            |> Expect.equal
                (Type.Record
                    { fields =
                        Dict.fromList
                            [ ( "a", Type.Int )
                            , ( "b", Type.Char )
                            , ( "c", Type.Bool )
                            ]
                    }
                )
        , Test.test "an open record with a type-variable tail stays open" <| \() ->
        TypeI.toPublicType
            { alreadyNormalized = False }
            (TypeI.ExtensibleRecord
                { extensionTypevar = TypeI.TypeVar ( TypeVar.Generated 0, TypeVar.Normal )
                , fields = Dict.singleton "a" TypeI.Int
                }
            )
            |> Expect.equal
                (Type.ExtensibleRecord
                    { extensionTypevar = "a"
                    , fields = Dict.singleton "a" Type.Int
                    }
                )
        ]


testExprLeakFree : String -> Test
testExprLeakFree exprCode =
    Test.test exprCode <| \() ->
    case getExprTypeWithDeps [ CoreFixture.core ] exprCode of
        Err err ->
            Expect.fail ("Has failed in a bad way: " ++ Debug.toString err)

        Ok type_ ->
            let
                str : String
                str =
                    Type.toString type_
            in
            Expect.all
                [ \_ -> str |> String.contains "#" |> Expect.equal False
                , \_ -> str |> String.contains "any type" |> Expect.equal False
                ]
                ()


publicSurfaceLeakSuite : Test
publicSurfaceLeakSuite =
    Test.describe "published surface never leaks internal typevar details"
        ([ "\\x -> x + 1"
         , "\\x y -> x == y"
         , "[1, 2, 3]"
         , "\\r -> r.a"
         , "{ a = 1, b = 'x' }"
         , "\\x y -> (x, y)"
         , "let id x = x in (id 1, id 'a')"
         ]
            |> List.map testExprLeakFree
        )


isNumberLike : Maybe Type -> Bool
isNumberLike maybeType =
    case maybeType of
        Just (Type.TypeVar name) ->
            String.startsWith "number" name

        _ ->
            False


largeInputsSuite : Test
largeInputsSuite =
    Test.describe "large inputs"
        [ Test.test "a large list literal doesn't blow the stack" <| \() ->
        -- One declaration's binding group holds ~2 type equations per
        -- list item; solving them used to recurse once per equation.
        ("""module Main exposing (main)

main =
    [ """
                    ++ String.join "\n    , " (List.repeat 3000 "\"a\"")
                    ++ """
    ]
"""
                )
                    |> inferMainModule
                    |> Result.map (always ())
                    |> Expect.equal (Ok ())
        , Test.test "a large list of large records doesn't blow the stack" <| \() ->
        let
            record : String
            record =
                "{ "
                    ++ (List.range 1 17
                            |> List.map (\i -> "field" ++ String.fromInt i ++ " = " ++ String.fromInt i)
                            |> String.join ", "
                       )
                    ++ " }"
        in
        ("""module Main exposing (main)

main =
    [ """
                    ++ String.join "\n    , " (List.repeat 500 record)
                    ++ """
    ]
"""
                )
                    |> inferMainModule
                    |> Result.map (always ())
                    |> Expect.equal (Ok ())
        ]


vec2 : Type
vec2 =
    Named
        { package = "elm-explorations/linear-algebra"
        , moduleName = [ "Math", "Vector2" ]
        , name = "Vec2"
        , arguments = []
        }


vec3 : Type
vec3 =
    Named
        { package = "elm-explorations/linear-algebra"
        , moduleName = [ "Math", "Vector3" ]
        , name = "Vec3"
        , arguments = []
        }


vec4 : Type
vec4 =
    Named
        { package = "elm-explorations/linear-algebra"
        , moduleName = [ "Math", "Vector4" ]
        , name = "Vec4"
        , arguments = []
        }


mat4 : Type
mat4 =
    Named
        { package = "elm-explorations/linear-algebra"
        , moduleName = [ "Math", "Matrix4" ]
        , name = "Mat4"
        , arguments = []
        }


texture : Type
texture =
    Named
        { package = "elm-explorations/webgl"
        , moduleName = [ "WebGL", "Texture" ]
        , name = "Texture"
        , arguments = []
        }


attr : List ( String, Type ) -> ShaderFields
attr attributes =
    { emptyShader | attributes = attributes }


glslSuite : Test
glslSuite =
    Test.describe "GLSL shaders"
        [ Test.describe "storage qualifiers"
            (List.map testExpr
                [ ( "[glsl|attribute vec3 a_position;|]"
                  , isShader { emptyShader | attributes = [ ( "a_position", vec3 ) ] }
                  )
                , ( "[glsl|uniform mat4 u_view;|]"
                  , isShader { emptyShader | uniforms = [ ( "u_view", mat4 ) ] }
                  )
                , ( "[glsl|varying vec2 v_texcoord;|]"
                  , isShader { emptyShader | varyings = [ ( "v_texcoord", vec2 ) ] }
                  )
                ]
            )
        , Test.describe "variable types"
            (List.map testExpr
                [ ( "[glsl|attribute vec2 x;|]", isShader (attr [ ( "x", vec2 ) ]) )
                , ( "[glsl|attribute vec3 x;|]", isShader (attr [ ( "x", vec3 ) ]) )
                , ( "[glsl|attribute vec4 x;|]", isShader (attr [ ( "x", vec4 ) ]) )
                , ( "[glsl|attribute mat4 x;|]", isShader (attr [ ( "x", mat4 ) ]) )
                , ( "[glsl|attribute sampler2D x;|]", isShader (attr [ ( "x", texture ) ]) )
                , ( "[glsl|attribute int x;|]", isShader (attr [ ( "x", Int ) ]) )
                , ( "[glsl|attribute float x;|]", isShader (attr [ ( "x", Float ) ]) )
                , -- we drop types we don't know:
                  ( "[glsl|attribute mat3 x;|]", isShader emptyShader )
                ]
            )
        , Test.describe "whitespace and layout"
            (List.map testExpr
                [ -- do we do multiline?
                  ( """
                    [glsl|
                    attribute vec3 a_position;
                    uniform mat4 u_view;
                    varying vec2 v_texcoord;
                    |]
                    """
                  , isShader
                        { attributes = [ ( "a_position", vec3 ) ]
                        , uniforms = [ ( "u_view", mat4 ) ]
                        , varyings = [ ( "v_texcoord", vec2 ) ]
                        }
                  )
                , ( """
                    [glsl|
                        attribute vec3 x;
                    |]
                    """
                  , isShader (attr [ ( "x", vec3 ) ])
                  )
                , ( """
                    [glsl|
                    attribute
                      vec4 a_position
                         ;
                    |]
                    """
                  , isShader (attr [ ( "a_position", vec4 ) ])
                  )
                , ( """
                    [glsl|
                    attribute vec3 x;
                    void main () {
                      gl_Position = vec4(x, 1.0);
                    }
                    |]
                    """
                  , isShader (attr [ ( "x", vec3 ) ])
                  )
                ]
            )
        , Test.describe "comments"
            (List.map testExpr
                [ ( "[glsl|uniform /* hello */ mat4 u_x;|]"
                  , isShader { emptyShader | uniforms = [ ( "u_x", mat4 ) ] }
                  )
                , ( """
                    [glsl|
                    // a comment; with a semicolon
                    attribute vec3 x;
                    |]
                    """
                  , isShader (attr [ ( "x", vec3 ) ])
                  )
                , ( """
                    [glsl|
                    /* multi
                       line; comment */
                    attribute vec3 x;
                    |]
                    """
                  , isShader (attr [ ( "x", vec3 ) ])
                  )
                , -- decl in comment doesn't count
                  ( """
                    [glsl|
                    // attribute vec3 x;
                    |]
                    """
                  , isShader emptyShader
                  )
                ]
            )
        , Test.describe "multiple declarators"
            (List.map testExpr
                [ ( "[glsl|uniform mat4 u_x, u_y, u_z;|]"
                  , isShader
                        { emptyShader
                            | uniforms =
                                [ ( "u_x", mat4 )
                                , ( "u_y", mat4 )
                                , ( "u_z", mat4 )
                                ]
                        }
                  )
                , ( "[glsl|uniform /* hello */ mat4 u_x, u_y, u_z;|]"
                  , isShader
                        { emptyShader
                            | uniforms =
                                [ ( "u_x", mat4 )
                                , ( "u_y", mat4 )
                                , ( "u_z", mat4 )
                                ]
                        }
                  )
                ]
            )
        , Test.describe "precision qualifiers"
            (List.map testExpr
                [ ( "[glsl|uniform lowp float x;|]"
                  , isShader { emptyShader | uniforms = [ ( "x", Float ) ] }
                  )
                , ( "[glsl|attribute mediump vec3 x;|]", isShader (attr [ ( "x", vec3 ) ]) )
                , ( "[glsl|uniform highp sampler2D x;|]"
                  , isShader { emptyShader | uniforms = [ ( "x", texture ) ] }
                  )
                ]
            )
        , Test.describe "unknown types are dropped"
            (List.map testExpr
                [ ( "[glsl|uniform vec3 u_lights[4];|]", isShader emptyShader ) -- array
                , ( "[glsl|attribute sampler2d x;|]", isShader emptyShader ) -- lowercase spelling
                ]
            )
        , Test.describe "unification"
            (List.map testExpr
                [ ( "(\\x -> x) [glsl|attribute vec3 x;|]"
                  , isShader (attr [ ( "x", vec3 ) ])
                  )
                , -- Two shaders with the same declarations unify.
                  ( "[ [glsl|attribute vec3 x;|], [glsl|attribute vec3 x;|] ]"
                  , isList (isShader (attr [ ( "x", vec3 ) ]))
                  )
                , -- The record sets are extensible, so shaders with different
                  -- fields unify too (their sets merge).
                  ( "[ [glsl|attribute vec3 x;|], [glsl|attribute vec3 y;|] ]"
                  , isList (isShader { emptyShader | attributes = [ ( "x", vec3 ), ( "y", vec3 ) ] })
                  )
                , ( "[ [glsl|attribute vec3 x;|], [glsl|attribute vec2 x;|] ]", fails )
                , ( "[ [glsl|attribute vec3 x;|], [glsl|uniform vec3 x;|] ]"
                  , isList
                        (isShader
                            { emptyShader
                                | attributes = [ ( "x", vec3 ) ]
                                , uniforms = [ ( "x", vec3 ) ]
                            }
                        )
                  )
                , ( "[ [glsl|attribute vec3 x;|], 1 ]", fails )
                , ( "[glsl|attribute vec3 x;|] 1", fails )
                ]
            )
        , Test.describe "the three sets are extensible records"
            (List.map testExpr
                [ ( "[glsl|attribute vec3 x;|]"
                  , isShader (attr [ ( "x", vec3 ) ])
                  )
                , ( "[glsl|uniform mat4 u;|]"
                  , isShader { emptyShader | uniforms = [ ( "u", mat4 ) ] }
                  )
                , ( "[glsl|varying vec2 v;|]"
                  , isShader { emptyShader | varyings = [ ( "v", vec2 ) ] }
                  )
                , ( """
                    [glsl|
                    attribute vec3 position;
                    attribute vec3 coord;
                    uniform mat4 view;
                    varying vec2 vcoord;
                    |]
                    """
                  , isShader
                        { attributes = [ ( "position", vec3 ), ( "coord", vec3 ) ]
                        , uniforms = [ ( "view", mat4 ) ]
                        , varyings = [ ( "vcoord", vec2 ) ]
                        }
                  )
                ]
            )
        ]


shaderAnnotationSuite : Test
shaderAnnotationSuite =
    let
        opacityMathModule : String -> String -> Elm.Docs.Module
        opacityMathModule moduleName typeName =
            { name = moduleName
            , comment = ""
            , unions = [ { name = typeName, comment = "", args = [], tags = [] } ]
            , aliases = []
            , values = []
            , binops = []
            }

        linearAlgebra : Dependency
        linearAlgebra =
            { name = "elm-explorations/linear-algebra"
            , dependencies = []
            , modules =
                [ opacityMathModule "Math.Vector2" "Vec2"
                , opacityMathModule "Math.Vector3" "Vec3"
                , opacityMathModule "Math.Matrix4" "Mat4"
                ]
            }

        webgl : Dependency
        webgl =
            { name = "elm-explorations/webgl"
            , dependencies = [ "elm-explorations/linear-algebra" ]
            , modules =
                [ { name = "WebGL"
                  , comment = ""
                  , unions =
                        [ { name = "Shader"
                          , comment = ""
                          , args = [ "attributes", "uniforms", "varyings" ]
                          , tags = []
                          }
                        ]
                  , aliases = []
                  , values = []
                  , binops = []
                  }
                ]
            }

        inferShader : String -> Result TestError Type
        inferShader code =
            getDeclTypeWithDeps [ webgl, linearAlgebra ]
                (Dict.singleton [ "Main" ] (String.ExtraExtra.multilineInput code))
                [ "Main" ]
                "shader"

        header : String
        header =
            """
            module Main exposing (shader)

            import Math.Matrix4 exposing (Mat4)
            import Math.Vector2 exposing (Vec2)
            import Math.Vector3 exposing (Vec3)
            import WebGL exposing (Shader)
            """
    in
    Test.describe "Shader type annotations unify with GLSL literals"
        [ Test.test "an annotation listing the same fields as the literal (extensible)" <| \() ->
        inferShader
            (header
                ++ """

            shader : Shader { a | position : Vec3 } { b | view : Mat4 } { c | vcoord : Vec2 }
            shader =
                [glsl|
                    attribute vec3 position;
                    uniform mat4 view;
                    varying vec2 vcoord;
                |]
            """
                    )
                    |> Result.map Type.toString
                    |> Expect.equal
                        (Ok "Shader { a | position : Math.Vector3.Vec3 } { b | view : Math.Matrix4.Mat4 } { c | vcoord : Math.Vector2.Vec2 }")
        , Test.test "an annotation listing the same fields as the literal (closed)" <| \() ->
        inferShader
            (header
                ++ """

            shader : Shader { position : Vec3 } { view : Mat4 } { vcoord : Vec2 }
            shader =
                [glsl|
                    attribute vec3 position;
                    uniform mat4 view;
                    varying vec2 vcoord;
                |]
            """
                    )
                    |> Result.map Type.toString
                    |> Expect.equal
                        (Ok "Shader {position : Math.Vector3.Vec3} {view : Math.Matrix4.Mat4} {vcoord : Math.Vector2.Vec2}")
        , Test.test "an annotation narrowing the literal's (open) sets" <| \() ->
        inferShader
            (header
                ++ """

            shader : Shader { position : Vec3 } { view : Mat4 } { vcoord : Vec2 }
            shader =
                [glsl|
                    attribute vec3 position;
                    attribute vec3 coord;
                    uniform mat4 view;
                    varying vec2 vcoord;
                |]
            """
                    )
                    |> Result.map Type.toString
                    |> Expect.equal
                        (Ok "Shader {position : Math.Vector3.Vec3} {view : Math.Matrix4.Mat4} {vcoord : Math.Vector2.Vec2}")
        , Test.test "an annotation with fully open sets (type variables)" <| \() ->
        inferShader
            (header
                ++ """

            shader : Shader a b c
            shader =
                [glsl|attribute vec3 position;|]
            """
                    )
                    |> Expect.err
        , Test.test "a field-type mismatch is still reported" <| \() ->
        inferShader
            (header
                ++ """

            shader : Shader { position : Vec4 } { view : Mat4 } { vcoord : Vec2 }
            shader =
                [glsl|
                    attribute vec3 position;
                    uniform mat4 view;
                    varying vec2 vcoord;
                |]
            """
                    )
                    |> Expect.err
        , Test.test "two different closed annotations don't unify" <| \() ->
        inferShader
            (header
                ++ """

            other : Shader { other : Vec3 } {} {}
            other =
                [glsl|attribute vec3 position;|]

            shader : Shader { position : Vec3 } {} {}
            shader = other
            """
                    )
                    |> Expect.err
        ]


dependenciesSuite : Test
dependenciesSuite =
    let
        testWithCore : ( String, Result Error Type.Type -> Bool ) -> Test
        testWithCore ( exprCode, predicate ) =
            Test.test exprCode <| \() ->
            case getExprTypeWithDeps [ CoreFixture.core ] exprCode of
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
        , Test.test "a module name exposed by one package and shipped internally by another stays two distinct types" <| \() ->
        let
            pkgA : Dependency
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

            pkgB : Dependency
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

            modules : Dict ModuleName String
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
        , Test.test "ambiguity between two same-named exposed modules across packages (miniBill/elm-ui-with-context's Element) is an error" <| \() ->
        let
            elmUi : Dependency
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

            styleElements : Dependency
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

            elmUiWithContext : Dependency
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

            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

main = 1
"""
                in
                case getDeclTypeWithDeps [ elmUi, styleElements, elmUiWithContext ] modules [ "Main" ] "main" of
                    Err (CouldntInfer err) ->
                        case err.details of
                            AmbiguousModuleOwner { moduleName, possiblePackages } ->
                                Expect.all
                                    [ \_ -> moduleName |> Expect.equal "Element"
                                    , \_ -> possiblePackages |> Expect.equal [ "mdgriffith/elm-ui", "mdgriffith/style-elements" ]
                                    ]
                                    ()

                            otherDetails ->
                                Expect.fail ("Expected AmbiguousModuleOwner, got: " ++ Debug.toString otherDetails)

                    other ->
                        Expect.fail ("Expected AmbiguousModuleOwner, got: " ++ Debug.toString other)
        , Test.test "ambiguity between two packages both defining Element.text is an error" <| \() ->
        let
            elmUi : Dependency
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

            styleElements : Dependency
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

            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

import Element

main = Element.text "hi"
"""
                in
                case getDeclTypeWithDeps [ elmUi, styleElements ] modules [ "Main" ] "main" of
                    Err (CouldntInfer err) ->
                        case err.details of
                            AmbiguousModuleOwner { moduleName, possiblePackages } ->
                                Expect.all
                                    [ \_ -> moduleName |> Expect.equal "Element"
                                    , \_ -> possiblePackages |> Expect.equal [ "mdgriffith/elm-ui", "mdgriffith/style-elements" ]
                                    ]
                                    ()

                            otherDetails ->
                                Expect.fail ("Expected AmbiguousModuleOwner, got: " ++ Debug.toString otherDetails)

                    other ->
                        Expect.fail ("Expected AmbiguousModuleOwner, got: " ++ Debug.toString other)
        , Test.test "ambiguity between two packages both defining an Element type is an error" <| \() ->
        let
            elementUnion : Elm.Docs.Union
            elementUnion =
                { name = "Element"
                , comment = ""
                , args = [ "msg" ]
                , tags = []
                }

            elmUi : Dependency
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

            styleElements : Dependency
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

            modules : Dict ModuleName String
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
                    Err (CouldntInfer err) ->
                        case err.details of
                            AmbiguousModuleOwner { moduleName, possiblePackages } ->
                                Expect.all
                                    [ \_ -> moduleName |> Expect.equal "Element"
                                    , \_ -> possiblePackages |> Expect.equal [ "mdgriffith/elm-ui", "mdgriffith/style-elements" ]
                                    ]
                                    ()

                            otherDetails ->
                                Expect.fail ("Expected AmbiguousModuleOwner, got: " ++ Debug.toString otherDetails)

                    other ->
                        Expect.fail ("Expected AmbiguousModuleOwner, got: " ++ Debug.toString other)
        , Test.test "a module exposed by both a direct dependency and a dependency-of-a-dependency isn't ambiguous (regression: gampleman/elm-visualization depends directly on elmcraft/core-extra, which exposes List.Extra; folkertdev/one-true-path-experiment -- itself only a direct dep -- separately depends on elm-community/list-extra, which *also* exposes a module named List.Extra, but that package isn't reachable/importable from our own source, exactly as with `elm make`)" <| \() ->
        let
            coreExtra : Dependency
            coreExtra =
                { name = "elmcraft/core-extra"
                , dependencies = []
                , modules =
                    [ { name = "List.Extra"
                      , comment = ""
                      , unions = [ { name = "Sentinel", comment = "", args = [], tags = [ ( "Sentinel", [] ) ] } ]
                      , aliases = []
                      , values =
                            [ { name = "last"
                              , comment = ""
                              , tipe = Elm.Type.Type "List.Extra.Sentinel" []
                              }
                            ]
                      , binops = []
                      }
                    ]
                }

            listExtra : Dependency
            listExtra =
                { name = "elm-community/list-extra"
                , dependencies = []
                , modules =
                    [ { name = "List.Extra"
                      , comment = ""
                      , unions = [ { name = "Sentinel", comment = "", args = [], tags = [ ( "Sentinel", [] ) ] } ]
                      , aliases = []
                      , values =
                            [ { name = "last"
                              , comment = ""
                              , tipe = Elm.Type.Type "List.Extra.Sentinel" []
                              }
                            ]
                      , binops = []
                      }
                    ]
                }

            onePathExperiment : Dependency
            onePathExperiment =
                { name = "folkertdev/one-true-path-experiment"
                , dependencies = [ "elm-community/list-extra" ]
                , modules = []
                }

            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

import List.Extra

main = List.Extra.last
"""
                in
                getDeclTypeWithDirectAndDeps
                    [ "elmcraft/core-extra", "folkertdev/one-true-path-experiment" ]
                    [ coreExtra, listExtra, onePathExperiment ]
                    modules
                    [ "Main" ]
                    "main"
                    |> Result.map (always ())
                    |> Expect.equal (Ok ())
        , Test.test "`import Platform.Cmd as Cmd exposing (Cmd)` is implicit" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

main : Cmd msg
main = Cmd.none
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Platform.Cmd.Cmd a")
        , Test.test "custom operator" <| \() ->
        let
            pkgCustomOps : Dependency
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

            modules : Dict ModuleName String
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
        , Test.test "`Basics.Bool` unifies with `True` (qualified primitive)" <| \() ->
        let
            modules : Dict ModuleName String
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
        , Test.test "`x : B.Int` (aliased import) unifies with an Int literal" <| \() ->
        let
            modules : Dict ModuleName String
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
        , Test.test "`x : Char.Char` unifies with a Char literal" <| \() ->
        let
            modules : Dict ModuleName String
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
        [ Test.test "top-level declaration order doesn't matter" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

main = helper 1
helper x = x
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "number")

        , Test.test "mutual recursion between two top-level declarations" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

isEven n = if n == 0 then True else isOdd n
isOdd n = if n == 0 then False else isEven n
main = isEven
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "number -> Bool")
        , Test.test "a top-level var is usable, at its own inferred type, across modules" <| \() ->
        let
            modules : Dict ModuleName String
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
        , Test.test "a project's own custom-type constructor is usable in an expression" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

type Box a = Box a
main = Box 1
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Main.Box number")
        , Test.test "a custom operator declaration is type-checked (infix usage)" <| \() ->
        let
            modules : Dict ModuleName String
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
        , Test.test "a custom operator declaration is type-checked (prefix usage)" <| \() ->
        let
            modules : Dict ModuleName String
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
        , Test.test "a custom operator declaration aliasing an imported function is type-checked" <| \() ->
        let
            modules : Dict ModuleName String
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
        , Test.test "a let destructuring can use a let function defined above it" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

main =
    let
        pair = ( 1, 'a' )
        ( n, c ) = pair
    in
    c
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Char")
        , Test.test "a let destructuring can use a let function defined below it" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

main =
    let
        ( n, c ) = pair
        pair = ( 1, 'a' )
    in
    c
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Char")
        , Test.test "a let function can use a name bound by a let destructuring below it" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

main =
    let
        useIt = c
        ( n, c ) = ( 1, 'a' )
    in
    useIt
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Char")
        , Test.test "a let record destructuring can use a let function defined above it" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

main =
    let
        record : { a : Char, b : Int }
        record = { a = 'x', b = 1 }

        { a, b } = record
    in
    a
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Char")
        , Test.test "a chain of let destructurings resolves in dependency order" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

main =
    let
        ( c, d ) = ( b, 'y' )
        ( a, b ) = start
        start = ( 'x', 'z' )
    in
    c
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Char")
        , Test.test "a case-pattern variable shadows an implicitly imported name" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

type Box a = Box a

main =
    case Box 'x' of
        Box identity ->
            identity
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Char")
        , Test.test "a let binding shadows an implicitly imported name" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

main =
    let
        identity = 'x'
    in
    identity
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Char")
        , Test.test "a lambda argument shadows an implicitly imported name" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

main =
    (\\identity -> identity) 'x'
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Char")
        , Test.test "a function argument shadows an implicitly imported name" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

useIt identity = identity

main = useIt 'x'
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Char")
        , Test.test "a local binding shadowing an implicitly imported name keeps the annotated type of its declaration" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

type MyResult e a = MyOk a | MyErr e

main : MyResult Char Int
main =
    let
        change = MyErr 'x'
    in
    case change of
        MyOk c -> MyOk c
        MyErr e -> MyErr e
"""
                in
                getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Main.MyResult Char Int")
        , Test.test "a qualifier shared by an alias and a real module resolves a type to whichever declares it" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.fromList
                    [ ( [ "Parser" ]
                      , """
module Parser exposing (Problem(..))

type Problem = Oops
"""
                              )
                            , ( [ "Elm", "Parser" ]
                              , """
module Elm.Parser exposing (parse)

parse x = x
"""
                              )
                            , ( [ "Main" ]
                              , """
module Main exposing (main)

import Elm.Parser as Parser
import Parser

describe : Parser.Problem -> Char
describe problem = 'x'

main = describe Parser.Oops
"""
                              )
                            ]
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Char")
        , Test.test "a qualifier shared by an alias and a real module prefers the aliased module when it declares the type" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.fromList
                    [ ( [ "Parser" ]
                      , """
module Parser exposing (Problem(..))

type Problem = TheWrongOne
"""
                              )
                            , ( [ "Elm", "Parser" ]
                              , """
module Elm.Parser exposing (Problem(..))

type Problem = TheRightOne
"""
                              )
                            , ( [ "Main" ]
                              , """
module Main exposing (main)

import Elm.Parser as Parser
import Parser

main : Parser.Problem
main = Parser.TheRightOne
"""
                              )
                            ]
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Elm.Parser.Problem")
        , Test.test "an aliased qualifier still resolves when no module of the alias's own name is imported" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.fromList
                    [ ( [ "Elm", "Parser" ]
                      , """
module Elm.Parser exposing (Problem(..))

type Problem = Oops
"""
                              )
                            , ( [ "Main" ]
                              , """
module Main exposing (main)

import Elm.Parser as Parser

main : Parser.Problem
main = Parser.Oops
"""
                              )
                            ]
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Elm.Parser.Problem")
        , Test.test "an `as` destructuring binds both the alias and the inner names" <| \() ->
        let
            modules : Dict ModuleName String
            modules =
                Dict.singleton [ "Main" ]
                    """
module Main exposing (main)

main =
    let
        useAlias = whole
        ({ a } as whole) = record
        record : { a : Char }
        record = { a = 'x' }
    in
    ( useAlias, a )
"""
                in
                getDeclType modules [ "Main" ] "main"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "( {a : Char}, Char )")
        ]


mainModule : FullModuleName
mainModule =
    FullModuleName.fromModuleName_ [ "Main" ]


generatedVar : Int -> MonoType
generatedVar n =
    TypeI.TypeVar ( TypeVar.Generated n, TypeVar.Normal )


runUnify : Dict ( PackageName, FullModuleName, String ) TypeAlias -> List ( MonoType, MonoType ) -> Result Error SubstitutionMap.SubstitutionMap
runUnify typeAliases eqs =
    (State.do
        (Unify.unifyMany
            { typeAliases = typeAliases
            , canSkipChecks = False
            , moduleName = mainModule
            , declarationNames = []
            }
            eqs
        )
     <| \() ->
     State.getSubst
    )
        |> State.run
            (State.test_initFull
                { lexicalEnv = Dict.empty
                , globalEnv = Dict.empty
                }
            )
        |> Tuple.first


{-| Temporary suite before we refactor/fix `Type.fromTypeAnnotation`'s
unqualified-name resolution bug.
-}
unifyAliasSuite : Test
unifyAliasSuite =
    let
        pairAlias : Unify.TypeAlias
        pairAlias =
            { args = [ ( TypeVar.Named "a", TypeVar.Normal ) ]
            , type_ =
                TypeI.Tuple2
                    (TypeI.TypeVar ( TypeVar.Named "a", TypeVar.Normal ))
                    (TypeI.TypeVar ( TypeVar.Named "a", TypeVar.Normal ))
            }

        typeAliases : Dict ( PackageName, FullModuleName, String ) TypeAlias
        typeAliases =
            Dict.singleton ( "", mainModule, "Pair" ) pairAlias

        pairOf : MonoType -> MonoType
        pairOf t =
            TypeI.UserDefinedType
                { package = ""
                , moduleName = mainModule
                , name = "Pair"
                , args = [ t ]
                }

        run : List ( MonoType, MonoType ) -> Result Error SubstitutionMap.SubstitutionMap
        run =
            runUnify typeAliases
    in
    Test.describe "Unify: type alias expansion"
        [ Test.test "a Pair Float unifies with (Float, Float)" <| \() ->
        run
            [ ( pairOf TypeI.Float
              , TypeI.Tuple2 (generatedVar 0) (generatedVar 1)
              )
            ]
            |> Result.map
                (\subst ->
                    let
                        ( res, _, _ ) =
                            SubstitutionMap.substituteMono
                                subst
                                (TypeI.Tuple2 (generatedVar 0) (generatedVar 1))
                    in
                    res
                )
            |> Expect.equal (Ok (TypeI.Tuple2 TypeI.Float TypeI.Float))
        , Test.test "two different uses of the same alias don't leak into each other" <| \() ->
        run
            [ ( pairOf TypeI.Float
              , TypeI.Tuple2 (generatedVar 0) (generatedVar 1)
              )
            , ( pairOf TypeI.Char
              , TypeI.Tuple2 (generatedVar 2) (generatedVar 3)
              )
            ]
            |> Result.map
                (\subst ->
                    let
                        ( res0, _, _ ) =
                            SubstitutionMap.substituteMono subst (generatedVar 0)

                        ( res2, _, _ ) =
                            SubstitutionMap.substituteMono subst (generatedVar 2)
                    in
                    ( res0, res2 )
                )
            |> Expect.equal (Ok ( TypeI.Float, TypeI.Char ))
        , Test.test "a Pair Float does not unify with (Float, Char)" <| \() ->
        run
            [ ( pairOf TypeI.Float
              , TypeI.Tuple2 TypeI.Float TypeI.Char
              )
            ]
            |> Result.map (always ())
            |> Expect.err
        ]


{-| `elm-review-unused` regression: `comparable` check must expand aliases inside a structural type
-}
comparableAliasedTupleRegression : Test
comparableAliasedTupleRegression =
    let
        moduleNameAlias : Unify.TypeAlias
        moduleNameAlias =
            { args = []
            , type_ = TypeI.List TypeI.String
            }

        typeAliases : Dict ( PackageName, FullModuleName, String ) TypeAlias
        typeAliases =
            Dict.singleton ( "", mainModule, "ModuleName" ) moduleNameAlias

        moduleNameType : MonoType
        moduleNameType =
            TypeI.UserDefinedType
                { package = ""
                , moduleName = mainModule
                , name = "ModuleName"
                , args = []
                }

        comparableVar : MonoType
        comparableVar =
            TypeI.TypeVar ( TypeVar.Generated 0, TypeVar.Comparable )
    in
    Test.test "a Tuple2 with an aliased (List String) element unifies with a `comparable` var" <| \() ->
    runUnify typeAliases
        [ ( comparableVar
          , TypeI.Tuple2 moduleNameType TypeI.String
          )
        ]
        |> Result.map (always ())
        |> Expect.equal (Ok ())


{-| `optimizations-plan.md` Step 1: path compression must not jump _over_ a
quantified var. `SubstitutionMap.substitute` removes the scheme's bound ids
from the map's domain before substituting the body, precisely so a chain
resolution stops there instead of continuing on to whatever the bound var
would otherwise point at outside the map. This assumes a quantified var is
never itself in the substitution's domain (see `optimizations-plan.md` Step 1
"One invariant to verify, not assume") -- this test pins that behavior down.
-}
substitutionMapCompressionSuite : Test
substitutionMapCompressionSuite =
    let
        a : TypeVar
        a =
            ( TypeVar.Generated 0, TypeVar.Normal )

        b : TypeVar
        b =
            ( TypeVar.Generated 1, TypeVar.Normal )

        -- a -> b -> Int, a chain that would otherwise compress straight to Int.
        subst : SubstitutionMap.SubstitutionMap
        subst =
            SubstitutionMap.test_fromList
                [ ( a, TypeI.TypeVar b )
                , ( b, TypeI.Int )
                ]
    in
    Test.describe "SubstitutionMap: path compression stops at quantified vars"
        [ Test.test "substituting outside any scheme resolves the whole chain" <| \() ->
        let
            ( res, _, _ ) =
                SubstitutionMap.substituteMono subst (TypeI.TypeVar a)
        in
        res
            |> Expect.equal TypeI.Int
        , Test.test "substituting a scheme quantified over `b` stops the chain at `b`" <| \() ->
        SubstitutionMap.substitute subst (TypeI.Forall [ b ] (TypeI.TypeVar a))
            |> Tuple.first
            |> Expect.equal (TypeI.Forall [ b ] (TypeI.TypeVar b))
        ]


{-| `linkTo` deliberately skips union-find rank bookkeeping (perf heuristic only)
but must still maintain let-ranks (generalization correctness).

Naming convention below: `letRank*` = generalization scope depth,
`unionFindRank*` = tree-height heuristic. They are unrelated.

-}
linkToRankSuite : Test
linkToRankSuite =
    let
        childVar : TypeVar
        childVar =
            ( TypeVar.Generated 10, TypeVar.Normal )

        parentVar : TypeVar
        parentVar =
            ( TypeVar.Generated 11, TypeVar.Normal )

        resolve : SubstitutionMap.SubstitutionMap -> TypeVar -> MonoType
        resolve subst var =
            SubstitutionMap.substituteMono subst (TypeI.TypeVar var)
                |> (\( res, _, _ ) -> res)

        unionFindRankOfVar : SubstitutionMap.SubstitutionMap -> TypeVar -> Int
        unionFindRankOfVar subst var =
            SubstitutionMap.unionFindRankOf subst (VarSet.varKey var)
    in
    Test.describe "SubstitutionMap: linkTo rank handling"
        [ Test.test "linkTo resolves child to parent" <| \() ->
        SubstitutionMap.empty
            |> SubstitutionMap.linkTo { child = childVar, parent = parentVar }
            |> (\subst -> resolve subst childVar)
            |> Expect.equal (TypeI.TypeVar parentVar)
        , Test.test "linkTo leaves union-find ranks at 0 (intentionally skipped)" <| \() ->
        let
            subst : SubstitutionMap.SubstitutionMap
            subst =
                SubstitutionMap.empty
                    |> SubstitutionMap.linkTo { child = childVar, parent = parentVar }
        in
        ( unionFindRankOfVar subst childVar
        , unionFindRankOfVar subst parentVar
        )
            |> Expect.equal ( 0, 0 )
        , Test.test "linkTo lowers parent let-rank to min when child is outer" <| \() ->
        let
            childLetRank : Int
            childLetRank =
                0

            parentLetRank : Int
            parentLetRank =
                5

            subst : SubstitutionMap.SubstitutionMap
            subst =
                SubstitutionMap.empty
                    |> SubstitutionMap.stampIdAtLetRank 10 childLetRank
                    |> SubstitutionMap.stampIdAtLetRank 11 parentLetRank
                    |> SubstitutionMap.linkTo { child = childVar, parent = parentVar }
        in
        SubstitutionMap.letRankOf parentVar subst
            |> Expect.equal (min childLetRank parentLetRank)
        , Test.test "linkTo lowers parent let-rank to min when parent is outer" <| \() ->
        let
            childLetRank : Int
            childLetRank =
                7

            parentLetRank : Int
            parentLetRank =
                2

            subst : SubstitutionMap.SubstitutionMap
            subst =
                SubstitutionMap.empty
                    |> SubstitutionMap.stampIdAtLetRank 10 childLetRank
                    |> SubstitutionMap.stampIdAtLetRank 11 parentLetRank
                    |> SubstitutionMap.linkTo { child = childVar, parent = parentVar }
        in
        SubstitutionMap.letRankOf parentVar subst
            |> Expect.equal (min childLetRank parentLetRank)
        , Test.test "linkTo chain resolves to ultimate parent despite union-find ranks staying 0" <| \() ->
        let
            middleVar : TypeVar
            middleVar =
                ( TypeVar.Generated 11, TypeVar.Normal )

            outerVar : TypeVar
            outerVar =
                ( TypeVar.Generated 12, TypeVar.Normal )

            subst : SubstitutionMap.SubstitutionMap
            subst =
                SubstitutionMap.empty
                    |> SubstitutionMap.linkTo { child = childVar, parent = middleVar }
                    |> SubstitutionMap.linkTo { child = middleVar, parent = outerVar }
        in
        ( resolve subst childVar
        , ( unionFindRankOfVar subst childVar
          , unionFindRankOfVar subst middleVar
          , unionFindRankOfVar subst outerVar
          )
        )
            |> Expect.equal
                ( TypeI.TypeVar outerVar
                , ( 0, 0, 0 )
                )
        , Test.test "union bumps union-find rank on equal merge and merges let-ranks to min" <| \() ->
        let
            aVar : TypeVar
            aVar =
                ( TypeVar.Generated 20, TypeVar.Normal )

            bVar : TypeVar
            bVar =
                ( TypeVar.Generated 21, TypeVar.Normal )

            subst : SubstitutionMap.SubstitutionMap
            subst =
                SubstitutionMap.empty
                    |> SubstitutionMap.stampIdAtLetRank 20 3
                    |> SubstitutionMap.stampIdAtLetRank 21 7
                    |> SubstitutionMap.union aVar bVar
        in
        -- Equal union-find ranks: `union a b` links b -> a and bumps a to 1.
        ( resolve subst bVar
        , SubstitutionMap.letRankOf aVar subst
        , unionFindRankOfVar subst aVar
        )
            |> Expect.equal ( TypeI.TypeVar aVar, 3, 1 )
        , Test.test "union lets taller union-find tree win (contrast: linkTo direction is forced)" <| \() ->
        let
            aVar : TypeVar
            aVar =
                ( TypeVar.Generated 20, TypeVar.Normal )

            bVar : TypeVar
            bVar =
                ( TypeVar.Generated 21, TypeVar.Normal )

            cVar : TypeVar
            cVar =
                ( TypeVar.Generated 22, TypeVar.Normal )

            subst : SubstitutionMap.SubstitutionMap
            subst =
                SubstitutionMap.empty
                    |> SubstitutionMap.union aVar bVar
                    |> SubstitutionMap.union aVar cVar
        in
        -- After the first union a has union-find rank 1, c has 0,
        -- so the second union must attach c under a.
        resolve subst cVar
            |> Expect.equal (TypeI.TypeVar aVar)
        , Test.test "unify Normal with Number keeps the more constrained parent" <| \() ->
        let
            normalMono : MonoType
            normalMono =
                TypeI.TypeVar ( TypeVar.Generated 30, TypeVar.Normal )

            numberMono : MonoType
            numberMono =
                TypeI.TypeVar ( TypeVar.Generated 31, TypeVar.Number )
        in
        runUnify Dict.empty [ ( normalMono, numberMono ) ]
            |> Result.map
                (\subst ->
                    ( resolve subst ( TypeVar.Generated 30, TypeVar.Normal )
                    , resolve subst ( TypeVar.Generated 31, TypeVar.Number )
                    )
                )
            |> Expect.equal (Ok ( numberMono, numberMono ))
        , Test.test "unify Comparable with Appendable resolves both to one fresh CompAppend var" <| \() ->
        let
            comparableMono : MonoType
            comparableMono =
                TypeI.TypeVar ( TypeVar.Generated 30, TypeVar.Comparable )

            appendableMono : MonoType
            appendableMono =
                TypeI.TypeVar ( TypeVar.Generated 31, TypeVar.Appendable )
        in
        runUnify Dict.empty [ ( comparableMono, appendableMono ) ]
            |> Result.map
                (\subst ->
                    let
                        resA : MonoType
                        resA =
                            resolve subst ( TypeVar.Generated 30, TypeVar.Comparable )

                        resB : MonoType
                        resB =
                            resolve subst ( TypeVar.Generated 31, TypeVar.Appendable )
                    in
                    case ( resA, resB ) of
                        ( TypeI.TypeVar ( TypeVar.Generated freshIdA, superA ), TypeI.TypeVar ( TypeVar.Generated freshIdB, superB ) ) ->
                            ( freshIdA == freshIdB, superA, superB )

                        _ ->
                            ( False, TypeVar.Normal, TypeVar.Normal )
                )
            |> Expect.equal (Ok ( True, TypeVar.CompAppend, TypeVar.CompAppend ))
        ]


{-| Instantiation must replace vars all at the same time, not one after another.

Here if we did it one after another, we'd get #5 -> #0 -> #1 and #0 -> #1.
We want to get #5 -> #0 and #0 -> #1 and end up with (#0, #1).

-}
instantiateIdCollisionRegression : Test
instantiateIdCollisionRegression =
    Test.test "instantiate doesn't chain when a fresh id collides with another bound id" <| \() ->
    -- Counter starts at 0, the fresh ids will be #0 and #1
    -- Collision with #0 being already bound.
    State.instantiate
        (TypeI.Forall
            [ ( TypeVar.Generated 5, TypeVar.Normal )
            , ( TypeVar.Generated 0, TypeVar.Normal )
            ]
            (TypeI.Tuple2 (generatedVar 5) (generatedVar 0))
        )
        |> State.run State.empty
        |> Tuple.first
        |> Expect.equal
            (Ok
                (TypeI.Tuple2
                    (generatedVar 0)
                    (generatedVar 1)
                )
            )


{-|

    type #0 == type #1
    type #1 == type #0
    ----------------
    both end up being #0 (`a` or such)

-}
composeCycleRegression : Test
composeCycleRegression =
    let
        a : MonoType
        a =
            TypeI.TypeVar ( TypeVar.Generated 0, TypeVar.Normal )

        b : MonoType
        b =
            TypeI.TypeVar ( TypeVar.Generated 1, TypeVar.Normal )
    in
    Test.test "unifying two vars in both directions works" <| \() ->
    runUnify Dict.empty [ ( a, b ), ( b, a ) ]
        |> Result.map
            (\subst ->
                let
                    ( resA, _, _ ) =
                        SubstitutionMap.substituteMono subst a

                    ( resB, _, _ ) =
                        SubstitutionMap.substituteMono subst b
                in
                ( resA, resB )
            )
        |> Expect.equal (Ok ( a, a ))


importedTypeInferredProperly : Test
importedTypeInferredProperly =
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

        core : Dependency
        core =
            { name = "elm/core"
            , dependencies = []
            , modules = [ setModule ]
            }

        modules : Dict ModuleName String
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
    Test.test "import Set exposing (Set) then using Set, gets inferred correctly" <| \() ->
    getDeclTypeWithDeps [ core ] modules [ "Main" ] "allowedNames"
        |> Expect.equal
            (Ok
                (Named
                    { package = "elm/core"
                    , moduleName = [ "Set" ]
                    , name = "Set"
                    , arguments = [ String ]
                    }
                )
            )


infiniteLoopRegression : Test
infiniteLoopRegression =
    let
        modules : Dict ModuleName String
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
    Test.test "infinite loop for extensible records - regression test" <| \() ->
    getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "update"
        |> Result.map Type.toString
        |> Expect.equal (Ok "List Main.Window -> List Main.Window")


aliasParamNameCollisionRegression : Test
aliasParamNameCollisionRegression =
    let
        modules : Dict ModuleName String
        modules =
            Dict.singleton [ "Main" ] <|
                String.ExtraExtra.multilineInput """
                module Main exposing (apply)

                type alias Wrap acc =
                    acc -> acc

                apply : Wrap acc -> acc -> acc
                apply f x =
                    f x
                """
    in
    Test.test "type alias whose own generic param name collides with the caller's generic name (regression test)" <| \() ->
    getDeclType modules [ "Main" ] "apply"
        |> Result.map Type.toString
        |> Expect.equal (Ok "Main.Wrap a -> a -> a")


recordConstructorFunctionRegression : Test
recordConstructorFunctionRegression =
    Test.test "a record type alias's own module can call it as a constructor function" <| \() ->
    let
        modules : Dict ModuleName String
        modules =
            Dict.singleton [ "Main" ] <|
                String.ExtraExtra.multilineInput """
    module Main exposing (main)

    type alias Foo =
        { a : Int
        , b : String
        }

    main : Foo
    main =
        Foo 1 "x"
    """
            in
            getDeclType modules [ "Main" ] "main"
                |> Result.map Type.toString
                |> Expect.equal (Ok "{a : Int, b : String}")


unionConstructorReexposeRegression : Test
unionConstructorReexposeRegression =
    Test.test "a union constructor re-exported via `exposing (Foo(..))` resolves unqualified in an importing module" <| \() ->
    let
        modules : Dict ModuleName String
        modules =
            Dict.fromList
                [ ( [ "A" ]
                  , String.ExtraExtra.multilineInput """
        module A exposing (Foo(..))

        type Foo
            = Foo Int
        """
                          )
                        , ( [ "Main" ]
                          , String.ExtraExtra.multilineInput """
        module Main exposing (main)

        import A exposing (Foo(..))

        main : Foo
        main =
            Foo 1
        """
                          )
                        ]
            in
            getDeclType modules [ "Main" ] "main"
                |> Result.map Type.toString
                |> Expect.equal (Ok "A.Foo")


recordConstructorReexposeRegression : Test
recordConstructorReexposeRegression =
    Test.test "a record type alias's implicit constructor re-exported via `exposing (Bar)` resolves unqualified in an importing module" <| \() ->
    let
        modules : Dict ModuleName String
        modules =
            Dict.fromList
                [ ( [ "A" ]
                  , String.ExtraExtra.multilineInput """
        module A exposing (Bar)

        type alias Bar =
            { x : Int }
        """
                          )
                        , ( [ "Main" ]
                          , String.ExtraExtra.multilineInput """
        module Main exposing (main)

        import A exposing (Bar)

        main : Bar
        main =
            Bar 1
        """
                          )
                        ]
            in
            getDeclType modules [ "Main" ] "main"
                |> Result.map Type.toString
                |> Expect.equal (Ok "{x : Int}")


unexposedUnionConstructorIsntFound : Test
unexposedUnionConstructorIsntFound =
    Test.test "union type exposed without `(..)` does not let an importing module use its constructor unqualified" <| \() ->
    let
        modules : Dict ModuleName String
        modules =
            Dict.fromList
                [ ( [ "A" ]
                  , String.ExtraExtra.multilineInput """
        module A exposing (Foo)

        type Foo
            = Foo Int
        """
                          )
                        , ( [ "Main" ]
                          , String.ExtraExtra.multilineInput """
        module Main exposing (main)

        import A exposing (Foo)

        main =
            Foo 1
        """
                          )
                        ]
            in
            getDeclType modules [ "Main" ] "main"
                |> Result.map Type.toString
                |> Expect.equal
                    (Err
                        (CouldntInfer
                            { moduleName = [ "Main" ]
                            , declarationNames = []
                            , details =
                                VarNotFound
                                    { usedIn = [ "Main" ]
                                    , varName = "Foo"
                                    }
                            }
                        )
                    )


duplicateImportAliasRegression : Test
duplicateImportAliasRegression =
    Test.test "two different modules imported under the same alias resolve a qualified type and constructor to whichever one actually declares it (regression test)" <| \() ->
    let
        modules : Dict ModuleName String
        modules =
            Dict.fromList
                [ ( [ "A" ]
                  , String.ExtraExtra.multilineInput """
        module A exposing (Placeholder)

        type alias Placeholder =
            Int
        """
                          )
                        , ( [ "B" ]
                          , String.ExtraExtra.multilineInput """
        module B exposing (Bar(..), Baz)

        type alias Baz =
            { x : Int }

        type Bar
            = Only
        """
                          )
                        , ( [ "Main" ]
                          , String.ExtraExtra.multilineInput """
        module Main exposing (value, useBaz)

        import A as M
        import B as M

        useBaz : M.Baz -> Int
        useBaz r =
            r.x

        value : M.Bar
        value =
            M.Only
        """
                          )
                        ]
            in
            Expect.all
                [ \() ->
                    getDeclType modules [ "Main" ] "useBaz"
                        |> Result.map Type.toString
                        |> Expect.equal (Ok "B.Baz -> Int")
                , \() ->
                    getDeclType modules [ "Main" ] "value"
                        |> Result.map Type.toString
                        |> Expect.equal (Ok "B.Bar")
                ]
                ()


importWithSpecificExposes : Test
importWithSpecificExposes =
    Test.test "a lambda parameter isn't confused with an unrelated value from a dependency imported only for its type" <| \() ->
    let
        decoderModule : Elm.Docs.Module
        decoderModule =
            { name = "Json.Decode"
            , comment = ""
            , unions = [ { name = "Decoder", comment = "", args = [ "a" ], tags = [] } ]
            , aliases = []
            , values =
                [ { name = "value"
                  , comment = ""
                  , tipe = Elm.Type.Type "Json.Decode.Decoder" [ Elm.Type.Type "Basics.Int" [] ]
                  }
                ]
            , binops = []
            }

        pkg : Dependency
        pkg =
            { name = "elm/json"
            , dependencies = []
            , modules = [ decoderModule ]
            }

        modules : Dict ModuleName String
        modules =
            Dict.singleton [ "Main" ] <|
                String.ExtraExtra.multilineInput """
    module Main exposing (attributeToString)

    import Json.Decode exposing (Decoder)

    attributeToString : ( String, String ) -> String
    attributeToString ( name, value ) =
        value
    """
            in
            getDeclTypeWithDeps [ pkg ] modules [ "Main" ] "attributeToString"
                |> Result.map Type.toString
                |> Expect.equal (Ok "( String, String ) -> String")


extensibleRecordRegression : Test
extensibleRecordRegression =
    Test.test "extensible record nesting" <| \() ->
    let
        modules : Dict ModuleName String
        modules =
            Dict.singleton [ "Main" ] <|
                String.ExtraExtra.multilineInput """
        module Main exposing ( Entity, applyForce )

        import Dict exposing (Dict)

        type alias Entity comparable a =
            { a
                | x : Float
                , y : Float
                , id : comparable
            }

        applyForce : Dict comparable (Entity comparable a) -> Dict comparable (Entity comparable a)
        applyForce entities =
            Dict.map (\\_ ent -> { ent | x = ent.x, y = ent.y }) entities
        """
            in
            getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] "applyForce"
                |> Result.map Type.toString
                |> Expect.equal (Ok "Dict.Dict comparable (Main.Entity comparable a) -> Dict.Dict comparable (Main.Entity comparable a)")


annotationsCheckedAgainstBodiesSuite : Test
annotationsCheckedAgainstBodiesSuite =
    let
        checkError : String -> Test
        checkError code =
            Test.test code <| \() ->
            getDeclType
                (Dict.singleton [ "Main" ] (String.ExtraExtra.multilineInput code))
                [ "Main" ]
                "x"
                |> Expect.err
    in
    Test.describe "annotations are checked against their expressions"
        [ checkError """
            module Main exposing (x)

            x : Int
            x = "nope"
            """
        , checkError """
            module Main exposing (x)

            x : Int -> Int
            x = \\n -> "oops"
            """
        , checkError """
            module Main exposing (x)

            x : Int -> Int
            x = 1
            """
        , checkError """
            module Main exposing (x)

            x : { a : Int }
            x = { a = "oops" }
            """
        , checkError """
            module Main exposing (x)

            x : { a : Int }
            x = { b = 1 }
            """
        , checkError """
            module Main exposing (x)

            x : List Int
            x = [ 1, "oops" ]
            """
        , checkError """
            module Main exposing (x)

            x : Int
            x = ( 1, 2 )
            """
        , checkError """
            module Main exposing (x)

            x : comparable
            x = { a = 1 }
            """
        , -- This one has some history: https://github.com/intellij-elm/intellij-elm/issues/482
          Test.test "extensible-record type param over a closed record loses its tail field" <| \() ->
          getDeclType
              (Dict.singleton [ "Main" ]
                  (String.ExtraExtra.multilineInput """
          module Main exposing (..)

          type Foo a = Foo (Outer a)
          type alias Outer a = { a | f1 : () }
          type alias Record = { f1 : () }

          foo : Foo Record
          foo =
              Foo { f1 = () }
          """)
                    )
                    [ "Main" ]
                    "foo"
                    |> Expect.err
        , Test.test "a let-bound function's annotation is checked against its body too" <| \() ->
        getDeclType
            (Dict.singleton [ "Main" ] (String.ExtraExtra.multilineInput """
            module Main exposing (main)

            main =
                let
                    x : Int
                    x = "nope"
                in
                x
            """))
                    [ "Main" ]
                    "main"
                    |> Expect.err
        , checkError """
            module Main exposing (x)

            f : Int -> Int
            f n = n

            x = f "hi"
            """
        , checkError """
            module Main exposing (x)

            f : Int -> Int
            f n = n

            g : String -> String
            g s = f s

            x = 1
            """
        , checkError """
            module Main exposing (x)

            f : Int -> Int
            f n = n

            x : String
            x = f 1
            """
        , checkError """
            module Main exposing (x)

            x : number
            x = 1.0
            """
        , Test.test "an annotation that the body satisfies is fine (identity)" <| \() ->
        getDeclType
            (Dict.singleton [ "Main" ] (String.ExtraExtra.multilineInput """
            module Main exposing (x)

            x : a -> a
            x = \\y -> y
            """))
                    [ "Main" ]
                    "x"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "a -> a")
        , Test.test "a polymorphic literal can satisfy a more specific annotation (Float = 1)" <| \() ->
        getDeclType
            (Dict.singleton [ "Main" ] (String.ExtraExtra.multilineInput """
            module Main exposing (x)

            x : Float
            x = 1
            """))
                    [ "Main" ]
                    "x"
                    |> Result.map Type.toString
                    |> Expect.equal (Ok "Float")
        ]
