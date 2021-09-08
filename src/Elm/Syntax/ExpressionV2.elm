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
    , getType
    , mapType
    , recurse
    , transformOnce
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
import Elm.Syntax.PatternV2 as PatternV2 exposing (PatternV2)
import Elm.Syntax.Signature exposing (Signature)
import Elm.TypeInference.Type exposing (TypeOrId)
import Transform


type alias ExprWith meta =
    NodeV2 meta (ExpressionV2 meta)


type alias LocatedExpr =
    ExprWith LocatedMeta


type alias TypedExpr =
    ExprWith TypedMeta


getType : TypedExpr -> TypeOrId
getType (NodeV2 { type_ } _) =
    type_


mapType : (TypeOrId -> TypeOrId) -> TypedExpr -> TypedExpr
mapType fn node =
    NodeV2.mapMeta (\meta -> { meta | type_ = fn meta.type_ }) node


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
    { args : List (NodeV2 meta (PatternV2 meta))
    , expression : ExprWith meta
    }


type alias CaseBlock meta =
    { expression : ExprWith meta
    , cases : Cases meta
    }


type alias Cases meta =
    List (Case meta)


type alias Case meta =
    ( NodeV2 meta (PatternV2 meta), ExprWith meta )


type alias LetBlock meta =
    { declarations : List (LocatedNode (LetDeclaration meta))
    , expression : ExprWith meta
    }


type LetDeclaration meta
    = LetFunction (FunctionV2 meta)
    | LetDestructuring (NodeV2 meta (PatternV2 meta)) (ExprWith meta)


type alias FunctionV2 meta =
    { documentation : Maybe (LocatedNode Documentation)
    , signature : Maybe (LocatedNode Signature)
    , declaration : LocatedNode (FunctionImplementationV2 meta)
    }


type alias FunctionImplementationV2 meta =
    { name : LocatedNode String
    , arguments : List (NodeV2 meta (PatternV2 meta))
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


transformOnce : (ExprWith meta -> ExprWith meta) -> ExprWith meta -> ExprWith meta
transformOnce pass expr =
    Transform.transformOnce
        recurse
        pass
        expr


recurse : (ExprWith meta -> ExprWith meta) -> ExprWith meta -> ExprWith meta
recurse fn node =
    let
        recurseRecordSetter : RecordSetter meta -> RecordSetter meta
        recurseRecordSetter ( a, b ) =
            ( a, fn b )

        recurseCase : Case meta -> Case meta
        recurseCase ( a, b ) =
            ( a, fn b )

        recurseLetDeclaration : LetDeclaration meta -> LetDeclaration meta
        recurseLetDeclaration decl =
            case decl of
                LetFunction ({ declaration } as fun) ->
                    LetFunction
                        { fun
                            | declaration =
                                NodeV2.map
                                    (\d -> { d | expression = fn d.expression })
                                    declaration
                        }

                LetDestructuring a b ->
                    LetDestructuring a (fn b)
    in
    node
        |> NodeV2.map
            (\expr ->
                case expr of
                    UnitExpr ->
                        expr

                    Application a ->
                        Application (List.map fn a)

                    OperatorApplication a b c d ->
                        OperatorApplication a b (fn c) (fn d)

                    FunctionOrValue _ _ ->
                        expr

                    IfBlock a b c ->
                        IfBlock (fn a) (fn b) (fn c)

                    PrefixOperator _ ->
                        expr

                    Operator _ ->
                        expr

                    Integer _ ->
                        expr

                    Hex _ ->
                        expr

                    Floatable _ ->
                        expr

                    Negation a ->
                        Negation (fn a)

                    Literal _ ->
                        expr

                    CharLiteral _ ->
                        expr

                    TupledExpression a ->
                        TupledExpression (List.map fn a)

                    ParenthesizedExpression a ->
                        ParenthesizedExpression (fn a)

                    LetExpression { declarations, expression } ->
                        LetExpression
                            { declarations = List.map (NodeV2.map recurseLetDeclaration) declarations
                            , expression = fn expression
                            }

                    CaseExpression { expression, cases } ->
                        CaseExpression
                            { expression = fn expression
                            , cases = List.map recurseCase cases
                            }

                    LambdaExpression ex ->
                        LambdaExpression { ex | expression = fn ex.expression }

                    RecordExpr a ->
                        RecordExpr (List.map (NodeV2.map recurseRecordSetter) a)

                    ListExpr a ->
                        ListExpr (List.map fn a)

                    RecordAccess a b ->
                        RecordAccess (fn a) b

                    RecordAccessFunction _ ->
                        expr

                    RecordUpdateExpression a b ->
                        RecordUpdateExpression a (List.map (NodeV2.map recurseRecordSetter) b)

                    GLSLExpression _ ->
                        expr
            )
