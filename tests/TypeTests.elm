module TypeTests exposing (suite)

import Dict
import Elm.TypeInference.Type as Type exposing (Type(..))
import Expect
import Test exposing (Test)
import Tests.Elm.TypeInference.Helpers exposing (getExprType)


suite : Test
suite =
    Test.describe "Elm.TypeInference.Type"
        [ toStringSuite
        ]


toStringSuite : Test
toStringSuite =
    let
        maybeOf : Type -> Type
        maybeOf arg =
            Named
                { package = "elm/core"
                , moduleName = [ "Maybe" ]
                , name = "Maybe"
                , arguments = [ arg ]
                }

        shader : Type
        shader =
            WebGLShader
                { attributesFields = Dict.empty
                , attributesExtensionTypevar = Nothing
                , uniformsFields = Dict.empty
                , uniformsExtensionTypevar = Nothing
                , varyingsFields = Dict.empty
                , varyingsExtensionTypevar = Nothing
                }

        cases : List ( Type, String )
        cases =
            [ ( List (List (TypeVar "a"))
              , "List (List a)"
              )
            , ( List (List (List (TypeVar "a")))
              , "List (List (List a))"
              )
            , ( List (Function { from = TypeVar "a", to = TypeVar "b" })
              , "List (a -> b)"
              )
            , ( maybeOf (List (TypeVar "a"))
              , "Maybe.Maybe (List a)"
              )
            , ( List (maybeOf (TypeVar "a"))
              , "List (Maybe.Maybe a)"
              )
            , ( maybeOf (maybeOf (TypeVar "a"))
              , "Maybe.Maybe (Maybe.Maybe a)"
              )
            , ( Function { from = List (List (TypeVar "a")), to = Int }
              , "List (List a) -> Int"
              )
            , ( Function { from = maybeOf (TypeVar "a"), to = TypeVar "b" }
              , "Maybe.Maybe a -> b"
              )
            , ( Function { from = Function { from = TypeVar "a", to = TypeVar "b" }, to = TypeVar "c" }
              , "(a -> b) -> c"
              )
            , ( List shader
              , "List (Shader {} {} {})"
              )
            ]

        toTest : ( Type, String ) -> Test
        toTest ( tipe, expected ) =
            Test.test expected <| \() ->
            Type.toString tipe
                |> Expect.equal expected
    in
    Test.describe "toString"
        (List.map toTest cases
            ++ [ Test.test "inferred [[1]] toStrings with parens" <| \() ->
                    getExprType "[[1]]"
                        |> Result.map Type.toString
                        |> Expect.equal (Ok "List (List number)")
               ]
        )
