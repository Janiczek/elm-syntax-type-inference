module ListExtraExtraTests exposing (suite)

import Expect
import Fuzz
import List.ExtraExtra
import Test exposing (Test)


suite : Test
suite =
    Test.describe "List.ExtraExtra"
        [ Test.describe "findLastMap"
            [ Test.test "empty list" <|
                \() ->
                    List.ExtraExtra.findLastMap String.toInt []
                        |> Expect.equal Nothing
            , Test.test "no match" <|
                \() ->
                    List.ExtraExtra.findLastMap String.toInt [ "a", "b" ]
                        |> Expect.equal Nothing
            , Test.test "picks the last match, not the first" <|
                \() ->
                    List.ExtraExtra.findLastMap String.toInt [ "1", "x", "2", "y" ]
                        |> Expect.equal (Just 2)
            , Test.fuzz (Fuzz.list (Fuzz.maybe Fuzz.int)) "agrees with filterMap >> reverse >> head" <|
                \list ->
                    List.ExtraExtra.findLastMap identity list
                        |> Expect.equal
                            (List.filterMap identity list |> List.reverse |> List.head)
            ]
        , Test.describe "fastConcatMap"
            [ Test.test "keeps the order of the outer list and of each inner list" <|
                \() ->
                    List.ExtraExtra.fastConcatMap (\n -> [ n, n * 10 ]) [ 1, 2, 3 ]
                        |> Expect.equal [ 1, 10, 2, 20, 3, 30 ]
            , Test.test "empty outer list" <|
                \() ->
                    List.ExtraExtra.fastConcatMap (\n -> [ n ]) []
                        |> Expect.equal []
            , Test.test "empty inner lists are skipped" <|
                \() ->
                    List.ExtraExtra.fastConcatMap
                        (\n ->
                            if modBy 2 n == 0 then
                                [ n ]

                            else
                                []
                        )
                        [ 1, 2, 3, 4 ]
                        |> Expect.equal [ 2, 4 ]
            , Test.fuzz (Fuzz.list (Fuzz.list Fuzz.int)) "gives exactly the same result as List.concatMap (order included)" <|
                \lists ->
                    List.ExtraExtra.fastConcatMap identity lists
                        |> Expect.equal (List.concatMap identity lists)
            ]
        , Test.describe "fastConcatMapWithInitial"
            [ Test.test "initial list goes at the end, after all mapped items" <|
                \() ->
                    List.ExtraExtra.fastConcatMapWithInitial (\n -> [ n, n ]) [ 1, 2 ] [ 99 ]
                        |> Expect.equal [ 1, 1, 2, 2, 99 ]
            , Test.test "empty outer list returns the initial list" <|
                \() ->
                    List.ExtraExtra.fastConcatMapWithInitial (\n -> [ n ]) [] [ 7, 8 ]
                        |> Expect.equal [ 7, 8 ]
            ]
        ]
