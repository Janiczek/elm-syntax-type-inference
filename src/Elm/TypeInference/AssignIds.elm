module Elm.TypeInference.AssignIds exposing (assignIds)

import Elm.Syntax.ExpressionV2
    exposing
        ( Case
        , Cases
        , ExpressionV2(..)
        , LetDeclaration(..)
        , LocatedExpr
        , RecordSetter
        , TypedExpr
        )
import Elm.Syntax.NodeV2 as NodeV2
    exposing
        ( LocatedMeta
        , LocatedNode
        , NodeV2(..)
        , TypedMeta
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

        letDeclarations :
            List (LocatedNode (LetDeclaration LocatedMeta))
            -> TIState (List (LocatedNode (LetDeclaration TypedMeta)))
        letDeclarations declarations =
            State.traverse letDeclaration declarations

        letDeclaration :
            LocatedNode (LetDeclaration LocatedMeta)
            -> TIState (LocatedNode (LetDeclaration TypedMeta))
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
                    State.map2
                        (\expression arguments ->
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
                        )
                        (f declarationImpl.expression)
                        (State.traverse p declarationImpl.arguments)

                LetDestructuring pattern p1 ->
                    State.map2
                        (\pattern_ p1_ -> NodeV2 meta <| LetDestructuring pattern_ p1_)
                        (p pattern)
                        (f p1)

        cases : Cases LocatedMeta -> TIState (Cases TypedMeta)
        cases cases_ =
            State.traverse case_ cases_

        case_ : Case LocatedMeta -> TIState (Case TypedMeta)
        case_ ( p1, e1 ) =
            State.map2 Tuple.pair
                (p p1)
                (f e1)

        recordSetter :
            NodeV2 LocatedMeta (RecordSetter LocatedMeta)
            -> TIState (NodeV2 LocatedMeta (RecordSetter TypedMeta))
        recordSetter (NodeV2 meta ( field, e1 )) =
            f e1
                |> State.map (\e1_ -> NodeV2 meta ( field, e1_ ))
    in
    case expr of
        UnitExpr ->
            finish UnitExpr

        Application exprs ->
            list exprs
                |> State.map (Application exprs_)
                |> State.andThen finish

        OperatorApplication a b e1 e2 ->
            State.map2 (OperatorApplication a b)
                (f e1)
                (f e2)
                |> State.andThen finish

        FunctionOrValue a b ->
            finish <| FunctionOrValue a b

        IfBlock e1 e2 e3 ->
            State.map3 IfBlock
                (f e1)
                (f e2)
                (f e3)
                |> State.andThen finish

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
            f e1
                |> State.map Negation
                |> State.andThen finish

        Literal a ->
            finish <| Literal a

        CharLiteral a ->
            finish <| CharLiteral a

        TupledExpression exprs ->
            list exprs
                |> State.map TupledExpression
                |> State.andThen finish

        ParenthesizedExpression e1 ->
            f e1
                |> State.map ParenthesizedExpression
                |> State.andThen finish

        LetExpression { declarations, expression } ->
            State.map2
                (\declarations_ expression_ ->
                    LetExpression
                        { declarations = declarations_
                        , expression = expression_
                        }
                )
                (letDeclarations declarations)
                (f expression)
                |> State.andThen finish

        CaseExpression caseBlock ->
            State.map2
                (\expression_ cases_ ->
                    CaseExpression
                        { expression = expression_
                        , cases = cases_
                        }
                )
                (f caseBlock.expression)
                (cases caseBlock.cases)
                |> State.andThen finish

        LambdaExpression { args, expression } ->
            State.map2
                (\args_ expression_ ->
                    LambdaExpression
                        { args = args_
                        , expression = expression_
                        }
                )
                (State.traverse p args)
                (f expression)
                |> State.andThen

        RecordExpr setters ->
            State.traverse recordSetter setters
                |> State.map RecordExpr
                |> State.andThen finish

        ListExpr exprs ->
            list exprs
                |> State.map ListExpr
                |> State.andThen finish

        RecordAccess e1 a ->
            f e1
                |> State.map (\e1_ -> RecordAccess e1_ a)
                |> State.andThen finish

        RecordAccessFunction a ->
            finish <| RecordAccessFunction a

        RecordUpdateExpression a setters ->
            State.traverse recordSetter setters
                |> State.map (RecordUpdateExpression a)
                |> State.andThen finish

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
            list patterns
                |> State.map TuplePattern
                |> State.andThen finish

        RecordPattern fields ->
            finish <| RecordPattern fields

        UnConsPattern p1 p2 ->
            State.map2 UnConsPattern
                (f p1)
                (f p2)
                |> State.andThen finish

        ListPattern patterns ->
            list patterns
                |> State.map ListPatern
                |> State.andThen finish

        VarPattern a ->
            finish <| VarPattern a

        NamedPattern a patterns ->
            list patterns
                |> State.map (NamedPattern a)
                |> State.andThen finish

        AsPattern p1 a ->
            f p1
                |> State.map (\p1_ -> AsPattern p1_ a)
                |> State.andThen finish

        ParenthesizedPattern p1 ->
            f p1
                |> State.map ParenthesizedPattern
                |> State.andThen finish
