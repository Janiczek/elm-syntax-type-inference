module Elm.Syntax.ExpressionV2 exposing
    ( Case
    , Cases
    , ExprWith
    , ExpressionV2(..)
    , FunctionImplementationV2
    , FunctionV2
    , LetDeclaration(..)
    , LocatedExpr
    , RecordSetter
    , TypedExpr
    , fromExpression
    , fromNodeExpression
    , map
    , mapFunction
    )

import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.NodeV2 as NodeV2
    exposing
        ( LocatedMeta
        , LocatedNode
        , NodeV2(..)
        , TypedMeta
        )
import Elm.Syntax.PatternV2 as PatternV2 exposing (PatternWith)
import Elm.Syntax.Signature exposing (Signature)


type alias ExprWith meta =
    NodeV2 meta (ExpressionV2 meta)


type alias LocatedExpr =
    ExprWith LocatedMeta


type alias TypedExpr =
    ExprWith TypedMeta


type ExpressionV2 meta
    = UnitExpr
    | Application (List (ExprWith meta))
    | OperatorApplication String InfixDirection (ExprWith meta) (ExprWith meta)
    | FunctionOrValue ModuleName String
    | IfBlock (ExprWith meta) (ExprWith meta) (ExprWith meta)
    | PrefixOperator String
    | Operator String
    | Integer Int
    | Hex Int
    | Floatable Float
    | Negation (ExprWith meta)
    | Literal String
    | CharLiteral Char
    | TupledExpression (List (ExprWith meta))
    | ParenthesizedExpression (ExprWith meta)
    | LetExpression (LetBlock meta)
    | CaseExpression (CaseBlock meta)
    | LambdaExpression (Lambda meta)
    | RecordExpr (List (LocatedNode (RecordSetter meta)))
    | ListExpr (List (ExprWith meta))
    | RecordAccess (ExprWith meta) (LocatedNode String)
    | RecordAccessFunction String
    | RecordUpdateExpression (LocatedNode String) (List (LocatedNode (RecordSetter meta)))
    | GLSLExpression String


type alias RecordSetter meta =
    ( LocatedNode String, ExprWith meta )


type alias Lambda meta =
    { args : List (PatternWith meta)
    , expression : ExprWith meta
    }


type alias CaseBlock meta =
    { expression : ExprWith meta
    , cases : Cases meta
    }


type alias Cases meta =
    List (Case meta)


type alias Case meta =
    ( PatternWith meta, ExprWith meta )


type alias LetBlock meta =
    { declarations : List (LocatedNode (LetDeclaration meta))
    , expression : ExprWith meta
    }


type LetDeclaration meta
    = LetFunction (FunctionV2 meta)
    | LetDestructuring (PatternWith meta) (ExprWith meta)


type alias FunctionV2 meta =
    { documentation : Maybe (LocatedNode Documentation)
    , signature : Maybe (LocatedNode Signature)
    , declaration : LocatedNode (FunctionImplementationV2 meta)
    }


type alias FunctionImplementationV2 meta =
    { name : LocatedNode String
    , arguments : List (PatternWith meta)
    , expression : ExprWith meta
    }


fromNodeExpression : Node Expression -> LocatedExpr
fromNodeExpression node =
    let
        range =
            Node.range node

        expr =
            Node.value node
    in
    NodeV2
        { range = range }
        (fromExpression expr)


fromExpression : Expression -> ExpressionV2 LocatedMeta
fromExpression expr =
    let
        f =
            fromNodeExpression

        convertLetDeclaration : Node Expression.LetDeclaration -> LocatedNode (LetDeclaration LocatedMeta)
        convertLetDeclaration declNode =
            declNode
                |> NodeV2.fromNode
                |> NodeV2.map
                    (\decl ->
                        case decl of
                            Expression.LetFunction { declaration, documentation, signature } ->
                                LetFunction
                                    { declaration =
                                        declaration
                                            |> NodeV2.fromNode
                                            |> NodeV2.map convertFnImplementation
                                    , documentation = Maybe.map NodeV2.fromNode documentation
                                    , signature = Maybe.map NodeV2.fromNode signature
                                    }

                            Expression.LetDestructuring a b ->
                                LetDestructuring
                                    (PatternV2.fromNodePattern a)
                                    (f b)
                    )

        convertFnImplementation : Expression.FunctionImplementation -> FunctionImplementationV2 LocatedMeta
        convertFnImplementation impl =
            { name = NodeV2.fromNode impl.name
            , arguments = List.map PatternV2.fromNodePattern impl.arguments
            , expression = f impl.expression
            }

        convertCase : Expression.Case -> Case LocatedMeta
        convertCase ( pattern, expr_ ) =
            ( PatternV2.fromNodePattern pattern
            , f expr_
            )

        convertRecordSetter : Expression.RecordSetter -> RecordSetter LocatedMeta
        convertRecordSetter ( string, expr_ ) =
            ( NodeV2.fromNode string
            , f expr_
            )
    in
    case expr of
        Expression.UnitExpr ->
            UnitExpr

        Expression.Application a ->
            Application (List.map f a)

        Expression.OperatorApplication a b c d ->
            OperatorApplication a b (f c) (f d)

        Expression.FunctionOrValue a b ->
            FunctionOrValue a b

        Expression.IfBlock a b c ->
            IfBlock (f a) (f b) (f c)

        Expression.PrefixOperator a ->
            PrefixOperator a

        Expression.Operator a ->
            Operator a

        Expression.Integer a ->
            Integer a

        Expression.Hex a ->
            Hex a

        Expression.Floatable a ->
            Floatable a

        Expression.Negation a ->
            Negation (f a)

        Expression.Literal a ->
            Literal a

        Expression.CharLiteral a ->
            CharLiteral a

        Expression.TupledExpression a ->
            TupledExpression (List.map f a)

        Expression.ParenthesizedExpression a ->
            ParenthesizedExpression (f a)

        Expression.LetExpression { declarations, expression } ->
            LetExpression
                { declarations = List.map convertLetDeclaration declarations
                , expression = f expression
                }

        Expression.CaseExpression { cases, expression } ->
            CaseExpression
                { cases = List.map convertCase cases
                , expression = f expression
                }

        Expression.LambdaExpression { args, expression } ->
            LambdaExpression
                { args = List.map PatternV2.fromNodePattern args
                , expression = f expression
                }

        Expression.RecordExpr a ->
            RecordExpr (List.map (NodeV2.fromNode >> NodeV2.map convertRecordSetter) a)

        Expression.ListExpr a ->
            ListExpr (List.map f a)

        Expression.RecordAccess a b ->
            RecordAccess (f a) (NodeV2.fromNode b)

        Expression.RecordAccessFunction a ->
            RecordAccessFunction a

        Expression.RecordUpdateExpression a b ->
            RecordUpdateExpression
                (NodeV2.fromNode a)
                (b |> List.map (NodeV2.fromNode >> NodeV2.map convertRecordSetter))

        Expression.GLSLExpression a ->
            GLSLExpression a


map : (meta1 -> meta2) -> ExprWith meta1 -> ExprWith meta2
map fn (NodeV2 meta expr) =
    let
        recordSetter : RecordSetter meta1 -> RecordSetter meta2
        recordSetter ( a, b ) =
            ( a, map fn b )

        case_ : Case meta1 -> Case meta2
        case_ ( a, b ) =
            ( PatternV2.map fn a, map fn b )

        let_ : LetDeclaration meta1 -> LetDeclaration meta2
        let_ decl =
            case decl of
                LetFunction fun ->
                    LetFunction (mapFunction fn fun)

                LetDestructuring a b ->
                    LetDestructuring
                        (PatternV2.map fn a)
                        (map fn b)
    in
    NodeV2 (fn meta) <|
        case expr of
            UnitExpr ->
                UnitExpr

            Application exprs ->
                Application (List.map (map fn) exprs)

            OperatorApplication a b e1 e2 ->
                OperatorApplication a b (map fn e1) (map fn e2)

            FunctionOrValue a b ->
                FunctionOrValue a b

            IfBlock e1 e2 e3 ->
                IfBlock (map fn e1) (map fn e2) (map fn e3)

            PrefixOperator a ->
                PrefixOperator a

            Operator a ->
                Operator a

            Integer a ->
                Integer a

            Hex a ->
                Hex a

            Floatable a ->
                Floatable a

            Negation e1 ->
                Negation (map fn e1)

            Literal a ->
                Literal a

            CharLiteral a ->
                CharLiteral a

            TupledExpression exprs ->
                TupledExpression (List.map (map fn) exprs)

            ParenthesizedExpression e1 ->
                ParenthesizedExpression (map fn e1)

            LetExpression letBlock ->
                LetExpression
                    { declarations = List.map (NodeV2.map let_) letBlock.declarations
                    , expression = map fn letBlock.expression
                    }

            CaseExpression caseBlock ->
                CaseExpression
                    { expression = map fn caseBlock.expression
                    , cases = List.map case_ caseBlock.cases
                    }

            LambdaExpression lambda ->
                LambdaExpression
                    { args = List.map (PatternV2.map fn) lambda.args
                    , expression = map fn lambda.expression
                    }

            RecordExpr setters ->
                RecordExpr (List.map (NodeV2.map recordSetter) setters)

            ListExpr exprs ->
                ListExpr (List.map (map fn) exprs)

            RecordAccess e1 a ->
                RecordAccess (map fn e1) a

            RecordAccessFunction a ->
                RecordAccessFunction a

            RecordUpdateExpression a setters ->
                RecordUpdateExpression a (List.map (NodeV2.map recordSetter) setters)

            GLSLExpression a ->
                GLSLExpression a


mapFunction : (meta1 -> meta2) -> FunctionV2 meta1 -> FunctionV2 meta2
mapFunction fn fun =
    { documentation = fun.documentation
    , signature = fun.signature
    , declaration =
        fun.declaration
            |> NodeV2.map
                (\decl_ ->
                    { name = decl_.name
                    , arguments = List.map (PatternV2.map fn) decl_.arguments
                    , expression = map fn decl_.expression
                    }
                )
    }
