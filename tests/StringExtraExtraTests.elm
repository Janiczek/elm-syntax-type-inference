module StringExtraExtraTests exposing (suite)

import Expect
import Fuzz
import String.ExtraExtra
import Test exposing (Test)


suite : Test
suite =
    Test.describe "String.ExtraExtra"
        [ Test.fuzz Fuzz.string "firstCharIsUpper" <| \str ->
        String.ExtraExtra.firstCharIsUpper str
            |> Expect.equal
                (case String.toList str of
                    firstChar :: _ ->
                        Char.isUpper firstChar

                    [] ->
                        False
                )
        ]
