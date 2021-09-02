module Elm.TypeInference.AssignIds exposing (assignIds)

import Elm.Syntax.ExpressionV2
    exposing
        ( ExpressionV2(..)
        , LetDeclaration(..)
        , LocatedExpr
        , TypedExpr
        , TypedMeta
        )
import Elm.Syntax.NodeV2 as NodeV2
    exposing
        ( LocatedMeta
        , LocatedNode
        , NodeV2(..)
        )
import Elm.Syntax.PatternV2
    exposing
        ( LocatedPattern
        , PatternV2(..)
        , TypedPattern
        )
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.Type exposing (TypeOrId_(..))


assignId : Range -> value -> TIState (NodeV2 TypedMeta value)
assignId range expr =
    State.getNextIdAndTick
        |> State.map (\id -> NodeV2 { range = range, type_ = Id id } expr)


assignIds : LocatedExpr -> TIState TypedExpr
assignIds (NodeV2 { range } expr) =
    let
        f : LocatedExpr -> TIState TypedExpr
        f =
            assignIds

        p : LocatedPattern -> TIState TypedPattern
        p =
            assignIdsToPattern

        finish : ExpressionV2 TypedMeta -> TIState TypedExpr
        finish e =
            assignId range e

        list : List LocatedExpr -> TIState (List TypedExpr)
        list exprs =
            State.traverse f exprs

        letDeclarations : List (LocatedNode (LetDeclaration LocatedMeta)) -> TIState (List (LocatedNode (LetDeclaration TypedMeta)))
        letDeclarations declarations =
            State.traverse letDeclaration declarations

        letDeclaration : LocatedNode (LetDeclaration LocatedMeta) -> TIState (LocatedNode (LetDeclaration TypedMeta))
        letDeclaration node =
            let
                meta =
                    NodeV2.meta node

                declaration =
                    NodeV2.value node
            in
            case declaration of
                LetFunction letFn ->
                    let
                        (NodeV2 declarationMeta declarationImpl) =
                            letFn.declaration
                    in
                    State.do (f declarationImpl.expression) <|
                        \expression ->
                            State.do (State.traverse p declarationImpl.arguments) <|
                                \arguments ->
                                    State.pure <|
                                        NodeV2 meta <|
                                            LetFunction
                                                { documentation = letFn.documentation
                                                , signature = letFn.signature
                                                , declaration =
                                                    NodeV2 declarationMeta
                                                        { name = declarationImpl.name
                                                        , arguments = arguments
                                                        , expression = expression
                                                        }
                                                }

                LetDestructuring pattern p1 ->
                    State.do (f p1) <|
                        \p1_ ->
                            State.do (p pattern) <|
                                \pattern_ ->
                                    State.pure <| NodeV2 meta <| LetDestructuring pattern_ p1_
    in
    case expr of
        UnitExpr ->
            finish UnitExpr

        Application exprs ->
            State.do (list exprs) <|
                \exprs_ ->
                    finish <| Application exprs_

        OperatorApplication a b e1 e2 ->
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
            State.do (letDeclarations declarations) <|
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


assignIdsToPattern : LocatedPattern -> TIState TypedPattern
assignIdsToPattern (NodeV2 { range } pattern) =
    let
        f : LocatedPattern -> TIState TypedPattern
        f =
            assignIdsToPattern

        finish : PatternV2 TypedMeta -> TIState TypedPattern
        finish p =
            assignId range p

        list : List LocatedPattern -> TIState (List TypedPattern)
        list patterns =
            State.traverse f patterns
    in
    case pattern of
        AllPattern ->
            finish AllPattern

        UnitPattern ->
            finish UnitPattern

        CharPattern a ->
            finish <| CharPattern a

        StringPattern a ->
            finish <| StringPattern a

        IntPattern a ->
            finish <| IntPattern a

        HexPattern a ->
            finish <| HexPattern a

        FloatPattern a ->
            finish <| FloatPattern a

        TuplePattern patterns ->
            State.do (list patterns) <|
                \patterns_ ->
                    finish <| TuplePattern patterns_

        RecordPattern fields ->
            finish <| RecordPattern fields

        UnConsPattern p1 p2 ->
            State.do (f p1) <|
                \p1_ ->
                    State.do (f p2) <|
                        \p2_ ->
                            finish <| UnConsPattern p1_ p2_

        ListPattern patterns ->
            State.do (list patterns) <|
                \patterns_ ->
                    finish <| ListPattern patterns_

        VarPattern a ->
            finish <| VarPattern a

        NamedPattern a patterns ->
            State.do (list patterns) <|
                \patterns_ ->
                    finish <| NamedPattern a patterns_

        AsPattern p1 a ->
            State.do (f p1) <|
                \p1_ ->
                    finish <| AsPattern p1_ a

        ParenthesizedPattern p1 ->
            State.do (f p1) <|
                \p1_ ->
                    finish <| ParenthesizedPattern p1_
