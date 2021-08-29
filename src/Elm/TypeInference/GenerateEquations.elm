module Elm.TypeInference.GenerateEquations exposing
    ( generateEquationsLinkingDeclarationsToUsages
    , generateLocalEquations
    )

import Elm.Syntax.ExpressionV2 exposing (ExpressionV2(..), TypedExpr)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.NodeV2 exposing (NodeV2(..))
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.State as State exposing (State, TIState)
import Elm.TypeInference.Type exposing (TypeOrId, TypeOrId_(..), Type_(..))
import Elm.TypeInference.TypeEquation as Eq exposing (TypeEquation)
import List.ExtraExtra as List


simple : List TypeEquation -> TIState (List TypeEquation)
simple e =
    State.pure e


list :
    (TypedExpr -> TIState (List TypeEquation))
    -> List TypedExpr
    -> TIState (List TypeEquation)
list f exprs =
    State.traverse f exprs
        |> State.map List.fastConcat


append :
    List TypeEquation
    -> TIState (List TypeEquation)
    -> TIState (List TypeEquation)
append equations s =
    State.map ((++) equations) s


generateLocalEquations : TypedExpr -> TIState (List TypeEquation)
generateLocalEquations ((NodeV2 ({ type_ } as meta) expr) as typedExpr) =
    let
        f : TypedExpr -> TIState (List TypeEquation)
        f =
            generateLocalEquations

        impossibleAstPattern =
            State.impossibleAstPattern typedExpr
    in
    case expr of
        UnitExpr ->
            simple [ Eq.equals type_ (Type Unit) ]

        Application [] ->
            impossibleAstPattern

        Application ((fn :: args) as exprs) ->
            let
                -- for expression `a b`:
                -- type of `a` is (argType -> resultType)
                -- which is `Function { from = argType, to = resultType }`
                {- TODO since this is application of multiple arguments, we'll
                   need to fold these together in the right direction.
                -}
                equations : List TypeEquation
                equations =
                    [ Debug.todo "generate eqs: application"
                    ]
            in
            append equations (list f exprs)

        OperatorApplication op _ e1 e2 ->
            let
                {- TODO link the op name in varTypes
                   For that we need the op to be fully qualified though...
                -}
                equations : List TypeEquation
                equations =
                    [ Debug.todo "generate eqs: op application: is a function"
                    , Debug.todo "generate eqs: op application: should probably use type_ somehow"
                    ]
            in
            append equations (list f [ e1, e2 ])

        FunctionOrValue moduleName varName ->
            functionOrValue moduleName varName type_

        IfBlock ((NodeV2 m1 _) as e1) ((NodeV2 m2 _) as e2) ((NodeV2 m3 _) as e3) ->
            list f [ e1, e2, e3 ]
                |> append
                    [ Eq.equals m1.type_ (Type Bool)
                    , Eq.equals m2.type_ m3.type_
                    , Eq.equals m2.type_ type_
                    ]

        PrefixOperator _ ->
            Debug.todo "generate eqs: prefix operator"

        Operator _ ->
            Debug.todo "generate eqs: operator"

        Integer _ ->
            -- TODO I wonder if we should somehow do `number` (int OR float) stuff here
            simple [ Eq.equals type_ (Type Int) ]

        Hex _ ->
            -- TODO I wonder if we should somehow do `number` (int OR float) stuff here
            simple [ Eq.equals type_ (Type Int) ]

        Floatable _ ->
            -- TODO I wonder if we should somehow do `number` (int OR float) stuff here
            simple [ Eq.equals type_ (Type Float) ]

        Negation ((NodeV2 m1 _) as e1) ->
            f e1
                |> append
                    [ Eq.equals type_ m1.type_
                    , Eq.equals type_ (Debug.todo "generate eqs: negation: number... int or float")
                    ]

        Literal _ ->
            simple [ Eq.equals type_ (Type String) ]

        CharLiteral _ ->
            simple [ Eq.equals type_ (Type Char) ]

        TupledExpression ([ NodeV2 m1 _, NodeV2 m2 _ ] as exprs) ->
            list f exprs
                |> append [ Eq.equals type_ (Type (Tuple m1.type_ m2.type_)) ]

        TupledExpression ([ NodeV2 m1 _, NodeV2 m2 _, NodeV2 m3 _ ] as exprs) ->
            list f exprs
                |> append [ Eq.equals type_ (Type (Tuple3 m1.type_ m2.type_ m3.type_)) ]

        TupledExpression _ ->
            impossibleAstPattern

        ParenthesizedExpression e ->
            list f [ e ]

        LetExpression _ ->
            Debug.todo "generate eqs: let"

        CaseExpression _ ->
            Debug.todo "generate eqs: case"

        LambdaExpression _ ->
            Debug.todo "generate eqs: lambda"

        RecordExpr _ ->
            Debug.todo "generate eqs: record"

        ListExpr exprs ->
            State.do (list f exprs) <|
                \exprsEquations ->
                    State.do State.getNextIdAndTick <|
                        \id ->
                            simple
                                (Eq.equals type_ (Type (List (Id id)))
                                    :: exprsEquations
                                    ++ List.map (\(NodeV2 m _) -> Eq.equals m.type_ (Id id)) exprs
                                )

        RecordAccess _ _ ->
            Debug.todo "generate eqs: record access"

        RecordAccessFunction _ ->
            Debug.todo "generate eqs: record access function"

        RecordUpdateExpression _ _ ->
            Debug.todo "generate eqs: record update expression"

        GLSLExpression _ ->
            Debug.todo "generate eqs: GLSL"



--elm-format-ignore-begin
functionOrValue : ModuleName -> VarName -> TypeOrId -> TIState (List TypeEquation)
functionOrValue moduleName varName type_ =
    State.do (State.addVarType moduleName varName type_) <| \() ->
    simple []
--elm-format-ignore-end


generateEquationsLinkingDeclarationsToUsages : () -> ()
generateEquationsLinkingDeclarationsToUsages () =
    ()
