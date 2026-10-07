module RecursiveAliasTests exposing (suite)

{-| Elm rejects type aliases that refer to themselves ("ALIAS PROBLEM").
-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.TypeInference
import Elm.TypeInference.InferError exposing (InferErrorDetails(..))
import Elm.TypeInference.Type as Type exposing (Type)
import Expect
import String.ExtraExtra
import Test exposing (Test)
import Tests.Elm.TypeInference.Fixture.ElmCore as CoreFixture
import Tests.Elm.TypeInference.Helpers exposing (TestError(..), buildProject, getDeclTypeWithDeps, parseModules)


suite : Test
suite =
    Test.describe "Recursive type aliases"
        [ Test.describe "are rejected" (List.map rejected rejectedCases)
        , Test.describe "recursion through a custom type is accepted" (List.map accepted acceptedCases)
        , Test.describe "across modules" crossModuleTests
        , expandDoesntCrash
        ]


rejectedCases : List ( String, String, List String )
rejectedCases =
    [ ( "c", """
        type alias Comment =
            { message : String, responses : List Comment }

        c : Comment
        c = { message = "a", responses = [] }
        """, [ "Comment" ] )
    , ( "x", """
        type alias T = Maybe T

        x : T
        x = Nothing
        """, [ "T" ] )
    , ( "f", """
        type alias F = Int -> F

        f : F
        f n = f
        """, [ "F" ] )
    , ( "x", """
        type alias T = ( Int, T )

        x : Int
        x = 1
        """, [ "T" ] )
    , ( "x", """
        type alias T a = { child : T a, value : a }

        x : Int
        x = 1
        """, [ "T" ] )
    , ( "x", """
        type alias R r = { r | x : Int }
        type alias Ext = { x : Int, next : R Ext }

        x : Int
        x = 1
        """, [ "Ext" ] )
    , ( "x", """
        type alias Comment =
            { message : String
            , upvotes : Int
            , downvotes : Int
            , responses : Responses
            }

        type alias Responses =
            { sortBy : SortBy
            , responses : List Comment
            }

        type SortBy = Time | Score | MostResponses

        x : Int
        x = 1
        """, [ "Comment", "Responses" ] )
    , ( "x", """
        type alias A = List B
        type alias B = Maybe C
        type alias C = { a : A }

        x : Int
        x = 1
        """, [ "A", "B", "C" ] )
    , ( "x", """
        type alias Fine = { x : Int }
        type alias Loop = { fine : Fine, loop : Loop }

        x : Int
        x = 1
        """, [ "Loop" ] )
    ]


acceptedCases : List ( String, String, String )
acceptedCases =
    [ ( "c", """
        type alias Comment =
            { message : String, responses : Responses }

        type Responses = Responses (List Comment)

        c : Comment
        c = { message = "a", responses = Responses [] }
        """, "Main.Comment" )
    , ( "t", """
        type Tree a = Node a (Forest a)

        type alias Forest a = List (Tree a)

        t : Forest Int
        t = [ Node 1 [] ]
        """, "Main.Forest Int" )
    , ( "x", """
        type alias Shared = { x : Int }
        type alias A = { s : Shared, b : B }
        type alias B = { s : Shared }

        x : A
        x = { s = { x = 1 }, b = { s = { x = 2 } } }
        """, "Main.A" )
    , ( "x", """
        type alias Pair a b = ( a, b )
        type alias IntPair = Pair Int Int

        x : IntPair
        x = ( 1, 2 )
        """, "Main.IntPair" )
    ]


crossModuleTests : List Test
crossModuleTests =
    [ Test.test "aliases chained through imports are fine" <|
        \() ->
            getDeclTypeWithDeps [ CoreFixture.core ]
                (Dict.fromList
                    [ ( [ "A" ], module_ "A" """
                        import B

                        type alias Outer = { inner : B.Inner }

                        x : Outer
                        x = { inner = { n = 1 } }
                        """ )
                    , ( [ "B" ], module_ "B" """
                        type alias Inner = { n : Int }
                        """ )
                    ]
                )
                [ "A" ]
                "x"
                |> Result.map Type.toString
                |> Expect.equal (Ok "A.Outer")
    , Test.test "a recursive alias in an imported module is reported on that module" <|
        \() ->
            getDeclTypeWithDeps [ CoreFixture.core ] twoModulesWithRecursiveB [ "B" ] "inner"
                |> expectRecursiveAlias [ ( [ "B" ], "Inner" ) ]
    , Test.test "and the importer can't use its values" <|
        \() ->
            case getDeclTypeWithDeps [ CoreFixture.core ] twoModulesWithRecursiveB [ "A" ] "x" of
                Err (CouldntInfer _) ->
                    Expect.pass

                other ->
                    Expect.fail ("Expected an inference error, got: " ++ Debug.toString other)
    , Test.test "in an import cycle, the module inferred first can't see the other's types" <|
        \() ->
            case getDeclTypeWithDeps [ CoreFixture.core ] importCycleWithAliasCycle [ "B" ] "b" of
                Err (CouldntInfer err) ->
                    err.details
                        |> Expect.equal
                            (TypeNotFound
                                { usedIn = [ "B" ]
                                , qualifier = [ "A" ]
                                , typeName = "Outer"
                                }
                            )

                other ->
                    Expect.fail ("Expected an inference error, got: " ++ Debug.toString other)
    ]


importCycleWithAliasCycle : Dict ModuleName String
importCycleWithAliasCycle =
    Dict.fromList
        [ ( [ "A" ], module_ "A" """
            import B

            type alias Outer = { inner : B.Inner }

            a : Int
            a = 1
            """ )
        , ( [ "B" ], module_ "B" """
            import A

            type alias Inner = { outer : A.Outer }

            b : Int
            b = 1
            """ )
        ]


twoModulesWithRecursiveB : Dict ModuleName String
twoModulesWithRecursiveB =
    Dict.fromList
        [ ( [ "A" ], module_ "A" """
            import B

            x : B.Inner
            x = B.inner
            """ )
        , ( [ "B" ], module_ "B" """
            type alias Inner = { next : Inner }

            inner : Inner
            inner = inner
            """ )
        ]


expandDoesntCrash : Test
expandDoesntCrash =
    Test.test "`expand` on a type mentioning a recursive alias doesn't loop" <|
        \() ->
            let
                files : Dict ModuleName String
                files =
                    Dict.singleton [ "Main" ]
                        (module_ "Main" """
                            type alias Comment =
                                { message : String, responses : List Comment }

                            c : Comment
                            c = { message = "a", responses = [] }
                            """)
            in
            case parseModules files |> Result.andThen (Dict.values >> buildProject Nothing [ "elm/core" ] [ CoreFixture.core ]) of
                Ok proj ->
                    let
                        type_ : Type
                        type_ =
                            Type.Named
                                { package = ""
                                , moduleName = [ "Main" ]
                                , name = "Comment"
                                , arguments = []
                                }
                    in
                    Elm.TypeInference.expand proj type_
                        |> Type.toString
                        |> Expect.equal "Main.Comment"

                Err err ->
                    Expect.fail ("Couldn't build the project: " ++ Debug.toString err)



-- HELPERS


module_ : String -> String -> String
module_ name body =
    "module " ++ name ++ " exposing (..)\n\n" ++ String.ExtraExtra.multilineInput body


infer : String -> String -> Result TestError Type
infer declName body =
    getDeclTypeWithDeps [ CoreFixture.core ] (Dict.singleton [ "Main" ] (module_ "Main" body)) [ "Main" ] declName


expectRecursiveAlias : List ( ModuleName, String ) -> Result TestError a -> Expect.Expectation
expectRecursiveAlias cycle result =
    case result of
        Err (CouldntInfer err) ->
            case err.details of
                RecursiveAlias r ->
                    let
                        ( expectedModule, expectedDeclarations ) =
                            case cycle of
                                ( firstModule, firstName ) :: _ ->
                                    ( firstModule, [ firstName ] )

                                [] ->
                                    ( [], [] )
                    in
                    { errorIn = err.moduleName
                    , declarationNames = err.declarationNames
                    , cycle = r.aliases
                    }
                        |> Expect.equal
                            { errorIn = expectedModule
                            , declarationNames = expectedDeclarations
                            , cycle = List.map (\( aliasModule, name ) -> { moduleName = aliasModule, name = name }) cycle
                            }

                _ ->
                    Expect.fail ("Failed for the wrong reason: " ++ Debug.toString err)

        Err other ->
            Expect.fail ("Failed for the wrong reason: " ++ Debug.toString other)

        Ok _ ->
            Expect.fail "Should have been rejected"


rejected : ( String, String, List String ) -> Test
rejected ( declName, body, cycle ) =
    Test.test (String.trim body) <|
        \() ->
            infer declName body
                |> expectRecursiveAlias (List.map (Tuple.pair [ "Main" ]) cycle)


accepted : ( String, String, String ) -> Test
accepted ( declName, body, expected ) =
    Test.test (String.trim body) <|
        \() ->
            infer declName body
                |> Result.map Type.toString
                |> Expect.equal (Ok expected)
