module NoSlowConcat exposing (rule)

{-| Make sure we use List.ExtraExtra.fastConcatMap instead of List.concatMap.

To be used with <https://package.elm-lang.org/packages/jfmengels/elm-review/latest/>

TODO: add an auto-fix


# Rule

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoSlowConcat" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Application (fn :: _) ->
            case Node.value fn of
                FunctionOrValue [ "List" ] "concatMap" ->
                    [ Rule.error
                        { message = "Slow List.concatMap function used"
                        , details =
                            [ "Please use List.ExtraExtra.fastConcatMap instead!"
                            ]
                        }
                        (Node.range node)
                    ]

                _ ->
                    []

        _ ->
            []
