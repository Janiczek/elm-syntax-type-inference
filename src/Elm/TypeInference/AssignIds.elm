module Elm.TypeInference.AssignIds exposing (assignIds)

import Elm.Syntax.ExpressionV2 as ExpressionV2
    exposing
        ( ExpressionV2(..)
        , LocatedExpr
        , TypedExpr
        , TypedMeta
        )
import Elm.Syntax.NodeV2 exposing (NodeV2(..))
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.State as State exposing (State, TIState)
import Elm.TypeInference.Type as Type exposing (Id, TypeOrId_(..))



--elm-format-ignore-begin
--elm-format-ignore-end


assignId : Range -> ExpressionV2 TypedMeta -> TIState TypedExpr
assignId range expr =
    State.getNextIdAndTick
        |> State.map (\id -> NodeV2 { range = range, type_ = Id id } expr)


assignIds : LocatedExpr -> TIState TypedExpr
assignIds (NodeV2 { range } expr) =
    -- TODO format manually and exempt
    let
        f : LocatedExpr -> TIState TypedExpr
        f =
            assignIds

        finish : ExpressionV2 TypedMeta -> TIState TypedExpr
        finish e =
            assignId range e

        list : List LocatedExpr -> TIState (List TypedExpr)
        list exprs =
            State.traverse f exprs
    in
    case expr of
        UnitExpr ->
            finish UnitExpr

        Application exprs ->
            State.do (list exprs) <|
                \exprs_ ->
                    finish <| Application exprs_

        OperatorApplication a b e1 e2 ->
            -- TODO possibly andMap?
            State.do (f e1) <|
                \e1_ ->
                    State.do (f e2) <|
                        \e2_ ->
                            finish <| OperatorApplication a b e1_ e2_

        FunctionOrValue a b ->
            finish <| FunctionOrValue a b

        IfBlock e1 e2 e3 ->
            State.do (f e1) <|
                \e1_ ->
                    State.do (f e2) <|
                        \e2_ ->
                            State.do (f e3) <|
                                \e3_ ->
                                    finish <| IfBlock e1_ e2_ e3_

        PrefixOperator a ->
            finish <| PrefixOperator a

        Operator a ->
            finish <| Operator a

        Integer a ->
            finish <| Integer a

        Hex a ->
            finish <| Hex a

        Floatable a ->
            finish <| Floatable a

        Negation e1 ->
            State.do (f e1) <|
                \e1_ ->
                    finish <| Negation e1_

        Literal a ->
            finish <| Literal a

        CharLiteral a ->
            finish <| CharLiteral a

        TupledExpression exprs ->
            State.do (list exprs) <|
                \exprs_ ->
                    finish <| TupledExpression exprs_

        ParenthesizedExpression e1 ->
            State.do (f e1) <|
                \e1_ ->
                    finish <| ParenthesizedExpression e1_

        LetExpression { declarations, expression } ->
            State.do (Debug.todo "assign: let: declarations") <|
                \declarations_ ->
                    State.do (f expression) <|
                        \expression_ ->
                            finish <|
                                LetExpression
                                    { declarations = declarations_
                                    , expression = expression_
                                    }

        CaseExpression { expression, cases } ->
            State.do (f expression) <|
                \expression_ ->
                    State.do (Debug.todo "assign: case: cases") <|
                        \cases_ ->
                            finish <|
                                CaseExpression
                                    { expression = expression_
                                    , cases = cases_
                                    }

        LambdaExpression { args, expression } ->
            State.do (Debug.todo "assign: lambda: args") <|
                \args_ ->
                    State.do (f expression) <|
                        \expression_ ->
                            finish <|
                                LambdaExpression
                                    { args = args_
                                    , expression = expression_
                                    }

        RecordExpr setters ->
            State.do (Debug.todo "assign: record: setters") <|
                \setters_ ->
                    finish <| RecordExpr setters_

        ListExpr exprs ->
            State.do (list exprs) <|
                \exprs_ ->
                    finish <| ListExpr exprs_

        RecordAccess e1 a ->
            State.do (f e1) <|
                \e1_ ->
                    finish <| RecordAccess e1_ a

        RecordAccessFunction a ->
            finish <| RecordAccessFunction a

        RecordUpdateExpression a setters ->
            State.do (Debug.todo "assign: record update: setters") <|
                \setters_ ->
                    finish <| RecordUpdateExpression a setters_

        GLSLExpression a ->
            finish <| GLSLExpression a
