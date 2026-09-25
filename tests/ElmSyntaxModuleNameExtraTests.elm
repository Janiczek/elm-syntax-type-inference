module ElmSyntaxModuleNameExtraTests exposing (suite)

import Elm.Syntax.ModuleName.Extra
import Expect
import Fuzz
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Elm.Syntax.ModuleName.Extra"
        [ Test.fuzz (Fuzz.list (Fuzz.oneOf [ Fuzz.asciiString, Fuzz.string ])) "splitLastDot" <| \segments ->
        let
            qualifiedName : String
            qualifiedName =
                String.join "." segments
        in
        Elm.Syntax.ModuleName.Extra.splitLastDot qualifiedName
            |> Expect.equal
                (case List.reverse (Elm.Syntax.ModuleName.Extra.fromDotted qualifiedName) of
                    [] ->
                        ( "", qualifiedName )

                    [ single ] ->
                        ( "", single )

                    last :: rest ->
                        ( Elm.Syntax.ModuleName.Extra.toString (List.reverse rest), last )
                )
        ]
