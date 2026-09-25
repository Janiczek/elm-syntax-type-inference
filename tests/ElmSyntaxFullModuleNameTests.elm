module ElmSyntaxFullModuleNameTests exposing (suite)

import Elm.Syntax.FullModuleName
import Elm.Syntax.ModuleName.Extra
import Expect
import Fuzz
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Elm.Syntax.FullModuleName"
        [ Test.fuzz
            (Fuzz.pair Fuzz.asciiString
                (Fuzz.list Fuzz.asciiString)
            )
            "toString"
          <| \fullModuleName ->
          Elm.Syntax.FullModuleName.toString fullModuleName
              |> Expect.equal (Elm.Syntax.ModuleName.Extra.toString (Elm.Syntax.FullModuleName.toModuleName fullModuleName))
        ]
