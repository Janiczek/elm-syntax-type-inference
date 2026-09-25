module ListExtraExtraTests exposing (suite)

import Expect
import Fuzz
import List.ExtraExtra
import Test exposing (Test)


suite : Test
suite =
    Test.describe "List.ExtraExtra"
        [ Test.fuzz (Fuzz.pair (Fuzz.list (Fuzz.intRange 0 5)) (Fuzz.intRange 0 10)) "findLastMap" <| \( left, rightLength ) ->
        List.ExtraExtra.findLastMap
            (\n ->
                if n < 3 then
                    Just n

                else
                    Nothing
            )
            (left ++ [ -1 ] ++ List.repeat rightLength 3)
            |> Expect.equal (Just -1)
        ]
