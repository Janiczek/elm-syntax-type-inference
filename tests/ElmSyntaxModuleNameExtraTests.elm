module ElmSyntaxModuleNameExtraTests exposing (suite)

import Elm.Syntax.ModuleName.Extra as ModuleNameExtra
import Expect
import Fuzz
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Elm.Syntax.ModuleName.Extra"
        [ Test.describe "splitLastDot"
            [ Test.test "qualified" <|
                \() ->
                    ModuleNameExtra.splitLastDot "Platform.Cmd.Cmd"
                        |> Expect.equal ( "Platform.Cmd", "Cmd" )
            , Test.test "one dot" <|
                \() ->
                    ModuleNameExtra.splitLastDot "Dict.Dict"
                        |> Expect.equal ( "Dict", "Dict" )
            , Test.test "no dot" <|
                \() ->
                    ModuleNameExtra.splitLastDot "Int"
                        |> Expect.equal ( "", "Int" )
            , Test.test "empty" <|
                \() ->
                    ModuleNameExtra.splitLastDot ""
                        |> Expect.equal ( "", "" )
            , Test.test "trailing dot" <|
                \() ->
                    ModuleNameExtra.splitLastDot "Foo."
                        |> Expect.equal ( "Foo", "" )
            , Test.fuzz Fuzz.string "agrees with split/reverse/join" <|
                \str ->
                    ModuleNameExtra.splitLastDot str
                        |> Expect.equal
                            (case List.reverse (String.split "." str) of
                                [] ->
                                    ( "", str )

                                [ _ ] ->
                                    ( "", str )

                                last :: revInit ->
                                    ( String.join "." (List.reverse revInit), last )
                            )
            ]
        , Test.describe "toString / fromDotted"
            [ Test.test "toString" <|
                \() ->
                    ModuleNameExtra.toString [ "Foo", "Bar" ]
                        |> Expect.equal "Foo.Bar"
            , Test.test "fromDotted" <|
                \() ->
                    ModuleNameExtra.fromDotted "Foo.Bar"
                        |> Expect.equal [ "Foo", "Bar" ]
            ]
        , Test.describe "qualifiedName"
            [ Test.test "nested module" <|
                \() ->
                    ModuleNameExtra.qualifiedName [ "Foo", "Bar" ] "baz"
                        |> Expect.equal "Foo.Bar.baz"
            , Test.test "type in module" <|
                \() ->
                    ModuleNameExtra.qualifiedName [ "Foo" ] "Bar"
                        |> Expect.equal "Foo.Bar"
            , Test.test "no module" <|
                \() ->
                    ModuleNameExtra.qualifiedName [] "baz"
                        |> Expect.equal "baz"
            ]
        , Test.describe "isNotEmpty"
            [ Test.test "empty" <| \() -> ModuleNameExtra.isNotEmpty [] |> Expect.equal False
            , Test.test "nonempty" <| \() -> ModuleNameExtra.isNotEmpty [ "A" ] |> Expect.equal True
            ]
        , Test.describe "dottedToFilePath"
            [ Test.test "nested" <|
                \() ->
                    ModuleNameExtra.dottedToFilePath "Css.Internal"
                        |> Expect.equal "src/Css/Internal.elm"
            ]
        ]
