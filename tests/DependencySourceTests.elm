module DependencySourceTests exposing (suite)

import Dict
import Elm.Parser
import Elm.Type
import Elm.TypeInference exposing (Dependency)
import Expect
import Test exposing (Test)
import Tests.Elm.TypeInference.Fixture.ElmCore as CoreFixture


suite : Test
suite =
    Test.describe "DependencySources"
        [ Test.test "`dependencyEnv` asks for source of hidden record aliases" <|
            \() ->
                case ( Elm.Parser.parseToFile hiddenSource, Elm.Parser.parseToFile mainSource ) of
                    ( Ok hidden, Ok main ) ->
                        case
                            Elm.TypeInference.dependencyEnv
                                { directDependencies = [ "example/css", "elm/core" ]
                                , allDependencies = [ cssDependency, CoreFixture.core ]
                                , sourcesToResolveAmbiguity = Dict.empty
                                }
                        of
                            Elm.TypeInference.Failed err ->
                                Expect.fail ("First pass should request sources, not fail: " ++ Debug.toString err)

                            Elm.TypeInference.Ready _ ->
                                Expect.fail "First pass should request sources for example/css"

                            Elm.TypeInference.NeedSources { neededPackages } ->
                                if neededPackages /= [ "example/css" ] then
                                    Expect.fail ("Should request example/css, requested: " ++ Debug.toString neededPackages)

                                else
                                    case
                                        Elm.TypeInference.dependencyEnv
                                            { directDependencies = [ "example/css", "elm/core" ]
                                            , allDependencies = [ cssDependency, CoreFixture.core ]
                                            , sourcesToResolveAmbiguity = Dict.singleton "example/css" [ hidden ]
                                            }
                                    of
                                        Elm.TypeInference.Failed err ->
                                            Expect.fail ("Second pass should succeed: " ++ Debug.toString err)

                                        Elm.TypeInference.NeedSources still ->
                                            Expect.fail ("Second pass should not request more sources: " ++ Debug.toString still)

                                        Elm.TypeInference.Ready env ->
                                            (Elm.TypeInference.inferProject env (Dict.singleton [ "Main" ] main)).errors
                                                |> Expect.equal Dict.empty

                    _ ->
                        Expect.fail "Regression source did not parse"
        ]


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
