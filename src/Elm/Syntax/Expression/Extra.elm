module Elm.Syntax.Expression.Extra exposing (functionName, referencedNames)

import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.VarName exposing (VarName)


functionName : Function -> String
functionName function =
    function.declaration
        |> Node.value
        |> .name
        |> Node.value


{-| Collects value names (not patterns).
Useful for binding-group SCC later.
-}
referencedNames : Expression -> List ( Maybe ModuleName, VarName )
referencedNames expression =
    let
        e : Node Expression -> List ( Maybe ModuleName, VarName )
        e node =
            referencedNames (Node.value node)

        many : List (Node Expression) -> List ( Maybe ModuleName, VarName )
        many nodes =
            List.concatMap e nodes
    in
    case expression of
        FunctionOrValue moduleName varName ->
            [ ( if List.isEmpty moduleName then
                    Nothing

                else
                    Just moduleName
              , varName
              )
            ]

        PrefixOperator operator ->
            [ ( Nothing, operator ) ]

        OperatorApplication operator _ e1 e2 ->
            ( Nothing, operator ) :: e e1 ++ e e2

        Application nodes ->
            many nodes

        IfBlock e1 e2 e3 ->
            many [ e1, e2, e3 ]

        Negation e1 ->
            e e1

        TupledExpression nodes ->
            many nodes

        ParenthesizedExpression e1 ->
            e e1

        LetExpression letBlock ->
            let
                declRefs : Node LetDeclaration -> List ( Maybe ModuleName, VarName )
                declRefs declNode =
                    case Node.value declNode of
                        LetFunction fn ->
                            e (Node.value fn.declaration).expression

                        LetDestructuring _ e1 ->
                            e e1
            in
            List.concatMap declRefs letBlock.declarations ++ e letBlock.expression

        CaseExpression caseBlock ->
            e caseBlock.expression ++ List.concatMap (\( _, body ) -> e body) caseBlock.cases

        LambdaExpression lambda ->
            e lambda.expression

        RecordExpr setters ->
            setters |> List.concatMap (Node.value >> Tuple.second >> e)

        ListExpr nodes ->
            many nodes

        RecordAccess recordNode _ ->
            e recordNode

        RecordAccessFunction _ ->
            []

        RecordUpdateExpression recordVarNode setters ->
            ( Nothing, Node.value recordVarNode )
                :: (setters |> List.concatMap (Node.value >> Tuple.second >> e))

        GLSLExpression _ ->
            []

        UnitExpr ->
            []

        Integer _ ->
            []

        Hex _ ->
            []

        Floatable _ ->
            []

        Literal _ ->
            []

        CharLiteral _ ->
            []

        Operator _ ->
            []
