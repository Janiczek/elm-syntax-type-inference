module DependencySourceTests exposing (suite)

import Dict exposing (Dict)
import Elm.Parser
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node
import Elm.Type
import Elm.TypeInference exposing (Dependency)
import Elm.TypeInference.ProjectError exposing (ProjectError(..))
import Expect
import Test exposing (Test)
import Tests.Elm.TypeInference.Fixture.ElmCore as CoreFixture


{-| `Main` infers without an error inside a fully-built `Project`.
-}
mainHasNoErrors : Elm.TypeInference.Project -> List File -> Expect.Expectation
mainHasNoErrors proj files =
    case List.head files of
        Nothing ->
            Expect.fail "Couldn't find Main file"

        Just mainFile ->
            case
                mainFile.declarations
                    |> List.filterMap
                        (\(Node.Node range decl) ->
                            case decl of
                                Declaration.FunctionDeclaration fn ->
                                    if (fn.declaration |> Node.value |> .name |> Node.value) == "main" then
                                        Just range

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                        )
                    |> List.head
            of
                Nothing ->
                    Expect.fail "Couldn't find main declaration"

                Just range ->
                    Elm.TypeInference.getType [ "Main" ] range proj
                        |> Tuple.first
                        |> Expect.ok


projectWith :
    List String
    -> List Dependency
    -> Dict String (List File)
    -> List File
    -> Result ProjectError Elm.TypeInference.Project
projectWith directDependencies allDependencies sources files =
    Elm.TypeInference.init
        { directDependencies = directDependencies
        , allDependencies = allDependencies
        , sourcesToResolveAmbiguity = sources
        , projectPackageName = Nothing
        , projectFiles = files
        }


suite : Test
suite =
    Test.describe "DependencySources"
        [ Test.test "`init` asks for source of hidden record aliases" <|
            \() ->
                case ( Elm.Parser.parseToFile hiddenSource, Elm.Parser.parseToFile mainSource ) of
                    ( Ok hidden, Ok main ) ->
                        let
                            mainFiles : List File
                            mainFiles =
                                [ main ]
                        in
                        case
                            projectWith
                                [ "example/css", "elm/core" ]
                                [ cssDependency, CoreFixture.core ]
                                Dict.empty
                                mainFiles
                        of
                            Ok _ ->
                                Expect.fail "First pass should request sources for example/css"

                            Err err ->
                                case err of
                                    NeedPackageSources needed ->
                                        if needed /= Dict.singleton "example/css" [ "src/Css/Internal.elm" ] then
                                            Expect.fail ("Should request example/css, requested: " ++ Debug.toString needed)

                                        else
                                            case
                                                projectWith
                                                    [ "example/css", "elm/core" ]
                                                    [ cssDependency, CoreFixture.core ]
                                                    (Dict.singleton "example/css" [ hidden ])
                                                    mainFiles
                                            of
                                                Err secondErr ->
                                                    Expect.fail ("Second pass should succeed, got: " ++ Debug.toString secondErr)

                                                Ok proj ->
                                                    mainHasNoErrors proj mainFiles

                                    _ ->
                                        Expect.fail ("First pass should request sources, not fail: " ++ Debug.toString err)

                    _ ->
                        Expect.fail "Regression source did not parse"
        , Test.test "`init` asks for source of a hidden record alias even when its own module is otherwise documented" <|
            \() ->
                -- Motivated by anmolitor/elm-protoc-utils
                -- `Protobuf.Utils.Duration` module is documented (its exposed
                -- functions are in docs.json) but the `Duration` alias itself
                -- isn't in the `exposing` list. We need the source to know it's
                -- a record and not an opaque type.
                case ( Elm.Parser.parseToFile hiddenTypeSource, Elm.Parser.parseToFile hiddenTypeMainSource ) of
                    ( Ok hidden, Ok main ) ->
                        let
                            mainFiles : List File
                            mainFiles =
                                [ main ]
                        in
                        case
                            projectWith
                                [ "example/duration", "elm/core" ]
                                [ durationDependency, CoreFixture.core ]
                                Dict.empty
                                mainFiles
                        of
                            Ok _ ->
                                Expect.fail "First pass should request sources for example/duration"

                            Err err ->
                                case err of
                                    NeedPackageSources needed ->
                                        if needed /= Dict.singleton "example/duration" [ "src/Duration.elm" ] then
                                            Expect.fail ("Should request example/duration, requested: " ++ Debug.toString needed)

                                        else
                                            case
                                                projectWith
                                                    [ "example/duration", "elm/core" ]
                                                    [ durationDependency, CoreFixture.core ]
                                                    (Dict.singleton "example/duration" [ hidden ])
                                                    mainFiles
                                            of
                                                Err secondErr ->
                                                    Expect.fail ("Second pass should succeed, got: " ++ Debug.toString secondErr)

                                                Ok proj ->
                                                    mainHasNoErrors proj mainFiles

                                    _ ->
                                        Expect.fail ("First pass should request sources, not fail: " ++ Debug.toString err)

                    _ ->
                        Expect.fail "Regression source did not parse"
        , Test.test "`init` asks for unexposed sibling modules the provided sources import" <|
            \() ->
                case
                    ( Elm.Parser.parseToFile importingHiddenSource
                    , Elm.Parser.parseToFile unitsSource
                    , Elm.Parser.parseToFile mainSource
                    )
                of
                    ( Ok hidden, Ok units, Ok main ) ->
                        let
                            mainFiles : List File
                            mainFiles =
                                [ main ]
                        in
                        case
                            projectWith
                                [ "example/css", "elm/core" ]
                                [ cssDependency, CoreFixture.core ]
                                (Dict.singleton "example/css" [ hidden ])
                                mainFiles
                        of
                            Ok _ ->
                                Expect.fail "Second pass should request the sibling module"

                            Err err ->
                                case err of
                                    NeedPackageSources needed ->
                                        if needed /= Dict.singleton "example/css" [ "src/Css/Internal/Units.elm" ] then
                                            Expect.fail ("Should request Css.Internal.Units only, requested: " ++ Debug.toString needed)

                                        else
                                            case
                                                projectWith
                                                    [ "example/css", "elm/core" ]
                                                    [ cssDependency, CoreFixture.core ]
                                                    (Dict.singleton "example/css" [ hidden, units ])
                                                    mainFiles
                                            of
                                                Err thirdErr ->
                                                    Expect.fail ("Third pass should succeed, got: " ++ Debug.toString thirdErr)

                                                Ok proj ->
                                                    mainHasNoErrors proj mainFiles

                                    _ ->
                                        Expect.fail ("Second pass should request sources, not fail: " ++ Debug.toString err)

                    _ ->
                        Expect.fail "Regression source did not parse"
        , Test.test "`init` rejects a recursive alias in the provided sources" <|
            \() ->
                case ( Elm.Parser.parseToFile recursiveHiddenSource, Elm.Parser.parseToFile mainSource ) of
                    ( Ok hidden, Ok main ) ->
                        case
                            projectWith
                                [ "example/css", "elm/core" ]
                                [ cssDependency, CoreFixture.core ]
                                (Dict.singleton "example/css" [ hidden ])
                                [ main ]
                        of
                            Ok _ ->
                                Expect.fail "Should have rejected the recursive alias"

                            Err err ->
                                err
                                    |> Elm.TypeInference.ProjectError.toString
                                    |> Expect.equal "Recursive alias { aliases = [Css.Internal.ExplicitLength] } (in Css.Internal.ExplicitLength from example/css)"

                    _ ->
                        Expect.fail "Regression source did not parse"
        ]


importingHiddenSource : String
importingHiddenSource =
    """module Css.Internal exposing (ExplicitLength)

import Css.Internal.Units as Units
import Elm.Kernel.Css

type alias ExplicitLength =
    { value : Int, unit : Units.Unit }
"""


unitsSource : String
unitsSource =
    """module Css.Internal.Units exposing (Unit)

type Unit = Px | Pct
"""


recursiveHiddenSource : String
recursiveHiddenSource =
    """module Css.Internal exposing (ExplicitLength)

type alias ExplicitLength =
    { value : Int
    , next : List ExplicitLength
    }
"""


durationDependency : Dependency
durationDependency =
    { name = "example/duration"
    , dependencies = [ "elm/core" ]
    , modules =
        [ { name = "Duration"
          , comment = ""
          , aliases = []
          , unions = []
          , binops = []
          , values =
                [ { name = "toSeconds"
                  , comment = ""
                  , tipe =
                        Elm.Type.Lambda
                            (Elm.Type.Type "Duration.Duration" [])
                            (Elm.Type.Type "Basics.Int" [])
                  }
                ]
          }
        ]
    }


hiddenTypeSource : String
hiddenTypeSource =
    """module Duration exposing (toSeconds)

type alias Duration =
    { seconds : Int, nanos : Int }


toSeconds : Duration -> Int
toSeconds d =
    d.seconds
"""


hiddenTypeMainSource : String
hiddenTypeMainSource =
    """module Main exposing (main)
import Duration
main = Duration.toSeconds { seconds = 1, nanos = 0 }
"""


cssDependency : Dependency
cssDependency =
    { name = "example/css"
    , dependencies = [ "elm/core" ]
    , modules =
        [ { name = "Css"
          , comment = ""
          , aliases = []
          , unions = []
          , binops = []
          , values =
                [ { name = "width"
                  , comment = ""
                  , tipe =
                        Elm.Type.Lambda
                            (Elm.Type.Record
                                [ ( "value", Elm.Type.Type "Basics.Int" [] ) ]
                                (Just "a")
                            )
                            (Elm.Type.Type "Basics.Int" [])
                  }
                , { name = "pct"
                  , comment = ""
                  , tipe =
                        Elm.Type.Lambda
                            (Elm.Type.Type "Basics.Float" [])
                            (Elm.Type.Type "Css.Internal.ExplicitLength" [])
                  }
                ]
          }
        ]
    }


hiddenSource : String
hiddenSource =
    """module Css.Internal exposing (ExplicitLength)

type alias ExplicitLength =
    { value : Int, numericValue : Float }

private = 1
"""


mainSource : String
mainSource =
    """module Main exposing (main)
import Css
main = Css.width (Css.pct 100)
"""
