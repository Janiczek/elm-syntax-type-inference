module ErrorTests exposing (suite)

import Dict
import Elm.Docs
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Type
import Elm.TypeInference
import Elm.TypeInference.Error as Error
import Expect
import String.ExtraExtra
import Test exposing (Test)
import Tests.Elm.TypeInference.Fixture.ElmCore as CoreFixture
import Tests.Elm.TypeInference.Helpers
    exposing
        ( TestError(..)
        , buildDepEnv
        , getDeclType
        , getDeclTypeWithDeps
        , getDeclTypeWithPackage
        , getExprTypeWithDeps
        , parseModules
        )


suite : Test
suite =
    Test.describe "Elm.TypeInference.Error (end-to-end)"
        [ varNotFoundTest
        , ambiguousNameTest
        , ambiguousModuleOwnerTest
        , impossibleDocsTypeTest
        , missingModuleNameTest
        , moduleNotFoundTest
        , typeMismatchTest
        , infiniteTypeTest
        , constraintMismatchTest
        ]


expectError : String -> Result TestError a -> Test
expectError expected result =
    Test.test expected <|
        \() ->
            case result of
                Err (CouldntInfer err) ->
                    Error.toString err
                        |> Expect.equal expected

                other ->
                    Expect.fail ("Expected a type inference error, got: " ++ Debug.toString other)


varNotFoundTest : Test
varNotFoundTest =
    getDeclType
        (Dict.singleton [ "Main" ]
            (String.ExtraExtra.multilineInput
                """
                module Main exposing (foo)

                foo =
                    bar
                """
            )
        )
        [ "Main" ]
        "foo"
        |> expectError "Var not found { usedIn = Main, varName = bar } (in Main)"


ambiguousNameTest : Test
ambiguousNameTest =
    getDeclType
        (Dict.fromList
            [ ( [ "A" ]
              , String.ExtraExtra.multilineInput
                    """
                    module A exposing (thing)

                    thing =
                        1
                    """
              )
            , ( [ "B" ]
              , String.ExtraExtra.multilineInput
                    """
                    module B exposing (thing)

                    thing =
                        2
                    """
              )
            , ( [ "Main" ]
              , String.ExtraExtra.multilineInput
                    """
                    module Main exposing (foo)

                    import A exposing (thing)
                    import B exposing (thing)

                    foo =
                        thing
                    """
              )
            ]
        )
        [ "Main" ]
        "foo"
        |> expectError "Ambiguous name { usedIn = Main, varName = thing, possibleModules = [A, B] } (in Main)"


ambiguousModuleOwnerTest : Test
ambiguousModuleOwnerTest =
    let
        elementModule : Elm.Docs.Module
        elementModule =
            { name = "Element"
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

        elmUi : Elm.TypeInference.Dependency
        elmUi =
            { name = "mdgriffith/elm-ui"
            , dependencies = []
            , modules = [ elementModule ]
            }

        styleElements : Elm.TypeInference.Dependency
        styleElements =
            { name = "mdgriffith/style-elements"
            , dependencies = []
            , modules = [ elementModule ]
            }
    in
    getDeclTypeWithPackage
        Nothing
        [ elmUi.name, styleElements.name ]
        [ elmUi, styleElements ]
        (Dict.singleton [ "Main" ]
            (String.ExtraExtra.multilineInput
                """
                module Main exposing (foo)

                import Element

                foo _ =
                    Element.text "hi"
                """
            )
        )
        [ "Main" ]
        "foo"
        |> expectError "Ambiguous module owner { moduleName = Element, possiblePackages = [mdgriffith/elm-ui, mdgriffith/style-elements] } (in Main)"


impossibleDocsTypeTest : Test
impossibleDocsTypeTest =
    let
        weirdModule : Elm.Docs.Module
        weirdModule =
            { name = "Weird"
            , comment = ""
            , unions = []
            , aliases = []
            , values =
                [ { name = "weird"
                  , comment = ""
                  , tipe =
                        Elm.Type.Tuple
                            [ Elm.Type.Var "a"
                            , Elm.Type.Var "b"
                            , Elm.Type.Var "c"
                            , Elm.Type.Var "d"
                            ]
                  }
                ]
            , binops = []
            }

        weird : Elm.TypeInference.Dependency
        weird =
            { name = "author/weird"
            , dependencies = []
            , modules = [ weirdModule ]
            }
    in
    getExprTypeWithDeps [ weird ]
        "Weird.weird"
        |> expectError "Impossible docs type ( a, b, c, d ) (in Weird)"


missingModuleNameTest : Test
missingModuleNameTest =
    Test.test "Missing module name" <|
        \() ->
            case
                parseModules
                    (Dict.singleton [ "Main" ]
                        (String.ExtraExtra.multilineInput
                            """
                module Main exposing (main)

                main =
                    1
                """
                        )
                    )
            of
                Err err ->
                    Expect.fail ("Couldn't parse fixture: " ++ Debug.toString err)

                Ok files ->
                    case buildDepEnv [] [] of
                        Err err ->
                            Expect.fail ("Couldn't build dependency env: " ++ Debug.toString err)

                        Ok depEnv ->
                            let
                                -- A file with an empty module name.
                                filesWithMissingName : List File
                                filesWithMissingName =
                                    files
                                        |> List.head
                                        |> Maybe.map withEmptyModuleName
                                        |> Maybe.map List.singleton
                                        |> Maybe.withDefault []
                            in
                            case Elm.TypeInference.project Nothing depEnv filesWithMissingName of
                                Err err ->
                                    Error.toString err
                                        |> Expect.equal "Missing module name (in <Missing>)"

                                Ok _ ->
                                    Expect.fail "Expected a MissingModuleName error"


withEmptyModuleName : File -> File
withEmptyModuleName file =
    let
        oldModule : Module.Module
        oldModule =
            Node.value file.moduleDefinition

        range : Range
        range =
            Node.range file.moduleDefinition

        emptyName : Node ModuleName
        emptyName =
            Node.empty []
    in
    { file
        | moduleDefinition =
            Node.Node range
                (case oldModule of
                    Module.NormalModule data ->
                        Module.NormalModule { data | moduleName = emptyName }

                    Module.PortModule data ->
                        Module.PortModule { data | moduleName = emptyName }

                    Module.EffectModule data ->
                        Module.EffectModule { data | moduleName = emptyName }
                )
    }


moduleNotFoundTest : Test
moduleNotFoundTest =
    Test.test "Module not found" <|
        \() ->
            case
                parseModules
                    (Dict.singleton [ "Main" ]
                        (String.ExtraExtra.multilineInput
                            """
                module Main exposing (main)

                main =
                    1
                """
                        )
                    )
            of
                Err err ->
                    Expect.fail ("Couldn't parse fixture: " ++ Debug.toString err)

                Ok files ->
                    case buildDepEnv [] [] of
                        Err err ->
                            Expect.fail ("Couldn't build dependency env: " ++ Debug.toString err)

                        Ok depEnv ->
                            case Elm.TypeInference.project Nothing depEnv files of
                                Err err ->
                                    Expect.fail ("Couldn't build project: " ++ Debug.toString err)

                                Ok proj ->
                                    case Elm.TypeInference.inferModule [ "DoesNotExist" ] proj |> Tuple.first of
                                        Err err ->
                                            Error.toString err
                                                |> Expect.equal "Module not found (in DoesNotExist)"

                                        Ok _ ->
                                            Expect.fail "Expected a ModuleNotFound error"


typeMismatchTest : Test
typeMismatchTest =
    getDeclType
        (Dict.singleton [ "Main" ]
            (String.ExtraExtra.multilineInput
                """
                module Main exposing (foo)

                foo : Int
                foo =
                    "abc"
                """
            )
        )
        [ "Main" ]
        "foo"
        |> expectError "Type mismatch Int String (in Main.foo)"


infiniteTypeTest : Test
infiniteTypeTest =
    getDeclType
        (Dict.singleton [ "Main" ]
            (String.ExtraExtra.multilineInput
                """
                module Main exposing (foo)

                foo x =
                    x x
                """
            )
        )
        [ "Main" ]
        "foo"
        |> expectError "Infinite type a (a -> b) (in Main.foo)"


constraintMismatchTest : Test
constraintMismatchTest =
    getDeclTypeWithDeps [ CoreFixture.core ]
        (Dict.singleton [ "Main" ]
            (String.ExtraExtra.multilineInput
                """
                module Main exposing (foo)

                foo x =
                    x + "a"
                """
            )
        )
        [ "Main" ]
        "foo"
        |> expectError "Constraint mismatch number String (in Main.foo)"
