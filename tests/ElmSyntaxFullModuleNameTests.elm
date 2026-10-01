module ElmSyntaxFullModuleNameTests exposing (suite)

import Elm.Syntax.FullModuleName as FullModuleName
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Elm.Syntax.FullModuleName"
        [ Test.describe "fromModuleName"
            [ Test.test "empty" <|
                \() ->
                    FullModuleName.fromModuleName []
                        |> Expect.equal Nothing
            , Test.test "nonempty" <|
                \() ->
                    FullModuleName.fromModuleName [ "Platform", "Cmd" ]
                        |> Expect.equal (Just ( "Platform", [ "Cmd" ] ))
            ]
        , Test.describe "fromModuleName_"
            [ Test.test "empty falls back to a BUG marker" <|
                \() ->
                    FullModuleName.fromModuleName_ []
                        |> Expect.equal ( "<BUG> The file didn't have a proper module name", [] )
            , Test.test "nonempty" <|
                \() ->
                    FullModuleName.fromModuleName_ [ "Platform", "Cmd" ]
                        |> Expect.equal ( "Platform", [ "Cmd" ] )
            ]
        , Test.describe "round trips"
            [ Test.test "fromDotted >> toString" <|
                \() ->
                    FullModuleName.fromDotted "Platform.Cmd"
                        |> FullModuleName.toString
                        |> Expect.equal "Platform.Cmd"
            , Test.test "fromDotted >> toModuleName" <|
                \() ->
                    FullModuleName.fromDotted "Platform.Cmd"
                        |> FullModuleName.toModuleName
                        |> Expect.equal [ "Platform", "Cmd" ]
            ]
        ]
