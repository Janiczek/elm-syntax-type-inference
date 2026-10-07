module TypeNameTests exposing (suite)

{-| Type names in annotations and declarations:

  - have to exist
  - have to be visible
  - have to be applied to the right number of arguments

`type` / `type alias` declarations have to declare the type variables they use.

-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.TypeInference.InferError exposing (InferErrorDetails(..))
import Elm.TypeInference.Type as Type exposing (Type)
import Expect
import String.ExtraExtra
import Test exposing (Test)
import Tests.Elm.TypeInference.Fixture.ElmCore as CoreFixture
import Tests.Elm.TypeInference.Helpers exposing (TestError(..), getDeclType, getDeclTypeWithDeps)


suite : Test
suite =
    Test.describe "Type names"
        [ Test.describe "unknown types are rejected" (List.map rejected notFoundCases)
        , Test.describe "wrong number of type arguments is rejected" (List.map rejected wrongArityCases)
        , Test.describe "undeclared type variables are rejected" (List.map rejected unboundCases)
        , Test.describe "known types are accepted" (List.map accepted acceptedCases)
        , Test.describe "across modules" crossModuleTests
        , Test.describe "without elm/core docs" implicitTypeTests
        ]


notFoundCases : List ( String, String, InferErrorDetails )
notFoundCases =
    [ ( "f", """
        f : Nonexistent -> Int
        f _ = 1
        """, typeNotFound [] "Nonexistent" )
    , ( "f", """
        f : Strng -> Int
        f _ = 1
        """, typeNotFound [] "Strng" )
    , ( "f", """
        f : Dict.Dict String Int -> Int
        f _ = 1
        """, typeNotFound [ "Dict" ] "Dict" )
    , ( "f", """
        import Dict

        f : Dict.Dikt String Int -> Int
        f _ = 1
        """, typeNotFound [ "Dict" ] "Dikt" )
    , ( "f", """
        import Dict exposing (Dict)

        f : Dikt String Int -> Int
        f _ = 1
        """, typeNotFound [] "Dikt" )
    , ( "x", """
        import Math.Vector3 exposing (Vec3)

        type alias V = { position : Vec3 }

        x : Int
        x = 1
        """, typeNotFound [] "Vec3" )
    , ( "x", """
        type T = T Nonexistent

        x : Int
        x = 1
        """, typeNotFound [] "Nonexistent" )
    , ( "x", """
        type alias R = { field : Nope }

        x : Int
        x = 1
        """, typeNotFound [] "Nope" )
    , ( "f", """
        f : { r | x : Nope } -> Int
        f _ = 1
        """, typeNotFound [] "Nope" )
    , ( "f", """
        f : List (Maybe Nope) -> Int
        f _ = 1
        """, typeNotFound [] "Nope" )
    ]


wrongArityCases : List ( String, String, InferErrorDetails )
wrongArityCases =
    [ ( "f", """
        f : Maybe Int Int -> Int
        f _ = 1
        """, wrongArity [ "Maybe" ] "Maybe" 1 2 )
    , ( "f", """
        f : Maybe -> Int
        f _ = 1
        """, wrongArity [ "Maybe" ] "Maybe" 1 0 )
    , ( "f", """
        f : Int Int -> Int
        f _ = 1
        """, wrongArity [ "Basics" ] "Int" 0 1 )
    , ( "f", """
        f : List -> Int
        f _ = 1
        """, wrongArity [ "List" ] "List" 1 0 )
    , ( "f", """
        f : Cmd -> Int
        f _ = 1
        """, wrongArity [ "Platform", "Cmd" ] "Cmd" 1 0 )
    , ( "f", """
        import Dict exposing (Dict)

        f : Dict String -> Int
        f _ = 1
        """, wrongArity [ "Dict" ] "Dict" 2 1 )
    , ( "f", """
        type alias Pair a b = ( a, b )

        f : Pair Int -> Int
        f _ = 1
        """, wrongArity [ "Main" ] "Pair" 2 1 )
    , ( "f", """
        type Box a = Box a

        f : Box -> Int
        f _ = 1
        """, wrongArity [ "Main" ] "Box" 1 0 )
    , ( "x", """
        type alias Id = Int
        type alias User = { id : Id Int }

        x : Int
        x = 1
        """, wrongArity [ "Main" ] "Id" 0 1 )
    ]


unboundCases : List ( String, String, InferErrorDetails )
unboundCases =
    [ ( "x", """
        type alias A = { x : a }

        x : Int
        x = 1
        """, unbound "A" "a" )
    , ( "x", """
        type T = T a

        x : Int
        x = 1
        """, unbound "T" "a" )
    , ( "x", """
        type alias R = { r | x : Int }

        x : Int
        x = 1
        """, unbound "R" "r" )
    , ( "x", """
        type alias P a = ( a, b )

        x : Int
        x = 1
        """, unbound "P" "b" )
    , ( "x", """
        type T a = A a | B (List b)

        x : Int
        x = 1
        """, unbound "T" "b" )
    ]


acceptedCases : List ( String, String, String )
acceptedCases =
    [ ( "f", """
        type List = Nil

        f : List -> Int
        f _ = 1
        """, "Main.List -> Int" )
    , ( "f", """
        import Dict as D

        f : D.Dict String Int -> Int
        f _ = 1
        """, "Dict.Dict String Int -> Int" )
    , ( "f", """
        import Dict exposing (Dict)

        f : Dict String Int -> Int
        f _ = 1
        """, "Dict.Dict String Int -> Int" )
    , ( "f", """
        import Dict exposing (..)

        f : Dict String Int -> Int
        f _ = 1
        """, "Dict.Dict String Int -> Int" )
    , ( "f", """
        type alias Pair a b = ( a, b )

        f : Pair Int String -> Int
        f _ = 1
        """, "Main.Pair Int String -> Int" )
    , ( "f", """
        type alias R r = { r | x : Int }

        f : R { y : Int } -> Int
        f r = r.x
        """, "Main.R { y : Int } -> Int" )
    , ( "f", """
        type Phantom a = Phantom

        f : Phantom Int -> Int
        f _ = 1
        """, "Main.Phantom Int -> Int" )
    , ( "f", """
        f : Maybe (List Int) -> Result String Char -> Cmd msg -> Int
        f _ _ _ = 1
        """, "Maybe.Maybe (List Int) -> Result.Result String Char -> Platform.Cmd.Cmd msg -> Int" )
    ]


crossModuleTests : List Test
crossModuleTests =
    let
        withA : String -> Dict ModuleName String
        withA mainBody =
            Dict.fromList
                [ ( [ "Main" ], module_ "Main" mainBody )
                , ( [ "A" ], """
                    module A exposing (Exposed, Alias, mk)

                    type Exposed a = Exposed a

                    type Hidden = Hidden

                    type alias Alias a = { n : a }

                    mk : Int
                    mk = 1
                    """ |> String.ExtraExtra.multilineInput )
                ]
    in
    [ Test.test "exposed custom type, qualified" <|
        \() ->
            inferModules (withA """
                import A

                f : A.Exposed Int -> Int
                f _ = 1
                """) "f"
                |> Result.map Type.toString
                |> Expect.equal (Ok "A.Exposed Int -> Int")
    , Test.test "exposed alias, via import exposing" <|
        \() ->
            inferModules (withA """
                import A exposing (Alias)

                f : Alias Int -> Int
                f r = r.n
                """) "f"
                |> Result.map Type.toString
                |> Expect.equal (Ok "A.Alias Int -> Int")
    , Test.test "type the module doesn't expose, qualified" <|
        \() ->
            inferModules (withA """
                import A

                f : A.Hidden -> Int
                f _ = 1
                """) "f"
                |> expectDetails (typeNotFound [ "A" ] "Hidden")
    , Test.test "type the module doesn't expose, via import exposing" <|
        \() ->
            inferModules (withA """
                import A exposing (Hidden)

                f : Hidden -> Int
                f _ = 1
                """) "f"
                |> expectDetails (typeNotFound [] "Hidden")
    , Test.test "module that isn't imported" <|
        \() ->
            inferModules (withA """
                f : A.Exposed Int -> Int
                f _ = 1
                """) "f"
                |> expectDetails (typeNotFound [ "A" ] "Exposed")
    , Test.test "wrong number of arguments for an imported type" <|
        \() ->
            inferModules (withA """
                import A

                f : A.Alias -> Int
                f _ = 1
                """) "f"
                |> expectDetails (wrongArity [ "A" ] "Alias" 1 0)
    ]


implicitTypeTests : List Test
implicitTypeTests =
    [ Test.test "implicit types resolve with their hardcoded arity" <|
        \() ->
            getDeclType
                (Dict.singleton [ "Main" ] (module_ "Main" """
                    f : Maybe (List Int) -> Result String Char -> Program () model msg -> Int
                    f _ _ _ = 1
                    """))
                [ "Main" ]
                "f"
                |> Result.map Type.toString
                |> Expect.equal (Ok "Maybe.Maybe (List Int) -> Result.Result String Char -> Platform.Program () model msg -> Int")
    , Test.test "and are still arity-checked" <|
        \() ->
            getDeclType
                (Dict.singleton [ "Main" ] (module_ "Main" """
                    f : Result String -> Int
                    f _ = 1
                    """))
                [ "Main" ]
                "f"
                |> expectDetails (wrongArity [ "Result" ] "Result" 2 1)
    ]



-- HELPERS


module_ : String -> String -> String
module_ name body =
    "module " ++ name ++ " exposing (..)\n\n" ++ String.ExtraExtra.multilineInput body


infer : String -> String -> Result TestError Type
infer declName body =
    inferModules (Dict.singleton [ "Main" ] (module_ "Main" body)) declName


inferModules : Dict ModuleName String -> String -> Result TestError Type
inferModules modules declName =
    getDeclTypeWithDeps [ CoreFixture.core ] modules [ "Main" ] declName


typeNotFound : ModuleName -> String -> InferErrorDetails
typeNotFound qualifier typeName =
    TypeNotFound { usedIn = [ "Main" ], qualifier = qualifier, typeName = typeName }


wrongArity : ModuleName -> String -> Int -> Int -> InferErrorDetails
wrongArity moduleName typeName expected actual =
    WrongTypeArity { usedIn = [ "Main" ], moduleName = moduleName, typeName = typeName, expected = expected, actual = actual }


unbound : String -> String -> InferErrorDetails
unbound typeName typeVar =
    UnboundTypeVariable { typeName = typeName, typeVar = typeVar }


expectDetails : InferErrorDetails -> Result TestError a -> Expect.Expectation
expectDetails expected result =
    case result of
        Err (CouldntInfer err) ->
            Expect.equal expected err.details

        Err other ->
            Expect.fail ("Failed for the wrong reason: " ++ Debug.toString other)

        Ok _ ->
            Expect.fail "Should have been rejected"


rejected : ( String, String, InferErrorDetails ) -> Test
rejected ( declName, body, expected ) =
    Test.test (String.trim body) <|
        \() ->
            infer declName body
                |> expectDetails expected


accepted : ( String, String, String ) -> Test
accepted ( declName, body, expected ) =
    Test.test (String.trim body) <|
        \() ->
            infer declName body
                |> Result.map Type.toString
                |> Expect.equal (Ok expected)
