module Elm.TypeInference.GenerateEquations exposing
    ( generateExprEquations
    , generatePatternEquations
    , generateVarEquations
    )

import Dict exposing (Dict)
import Elm.Syntax.ExpressionV2 exposing (ExpressionV2(..), TypedExpr)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.NodeV2 as NodeV2 exposing (NodeV2(..))
import Elm.Syntax.PatternV2
    exposing
        ( PatternV2(..)
        , TypedPattern
        )
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.Type exposing (Id, Type(..), TypeOrId(..))
import Elm.TypeInference.TypeEquation exposing (TypeEquation)
import List.ExtraExtra as List


finish : List TypeEquation -> TIState (List TypeEquation)
finish eqs =
    State.pure eqs


list :
    (a -> TIState (List TypeEquation))
    -> List a
    -> TIState (List TypeEquation)
list f items =
    State.traverse f items
        |> State.map List.fastConcat


append :
    List TypeEquation
    -> TIState (List TypeEquation)
    -> TIState (List TypeEquation)
append equations s =
    State.map ((++) equations) s


generateExprEquations : TypedExpr -> TIState (List TypeEquation)
generateExprEquations ((NodeV2 { type_ } expr) as typedExpr) =
    let
        f : TypedExpr -> TIState (List TypeEquation)
        f =
            generateExprEquations

        impossibleExpr =
            State.impossibleExpr typedExpr
    in
    case expr of
        UnitExpr ->
            finish [ ( type_, Type Unit ) ]

        Application [] ->
            impossibleExpr

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

        OperatorApplication _ _ e1 e2 ->
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
                    [ ( m1.type_, Type Bool )
                    , ( m2.type_, m3.type_ )
                    , ( m2.type_, type_ )
                    ]

        PrefixOperator _ ->
            Debug.todo "generate eqs: prefix operator"

        Operator _ ->
            impossibleExpr

        Integer _ ->
            -- TODO I wonder if we should somehow do `number` (int OR float) stuff here
            finish [ ( type_, Type Int ) ]

        Hex _ ->
            -- TODO I wonder if we should somehow do `number` (int OR float) stuff here
            finish [ ( type_, Type Int ) ]

        Floatable _ ->
            -- TODO I wonder if we should somehow do `number` (int OR float) stuff here
            finish [ ( type_, Type Float ) ]

        Negation ((NodeV2 m1 _) as e1) ->
            f e1
                |> append
                    [ ( type_, m1.type_ )
                    , ( type_, Debug.todo "generate eqs: negation: number... int or float" )
                    ]

        Literal _ ->
            finish [ ( type_, Type String ) ]

        CharLiteral _ ->
            finish [ ( type_, Type Char ) ]

        TupledExpression ([ NodeV2 m1 _, NodeV2 m2 _ ] as exprs) ->
            list f exprs
                |> append [ ( type_, Type (Tuple m1.type_ m2.type_) ) ]

        TupledExpression ([ NodeV2 m1 _, NodeV2 m2 _, NodeV2 m3 _ ] as exprs) ->
            list f exprs
                |> append [ ( type_, Type (Tuple3 m1.type_ m2.type_ m3.type_) ) ]

        TupledExpression _ ->
            impossibleExpr

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
            State.do (list f exprs) <| \exprsEquations ->
            State.do State.getNextIdAndTick <| \id ->
            finish
                (( type_, Type (List (Id id)) )
                    :: exprsEquations
                    ++ List.map (\(NodeV2 m _) -> ( m.type_, Id id )) exprs
                )

        RecordAccess _ _ ->
            Debug.todo "generate eqs: record access"

        RecordAccessFunction _ ->
            Debug.todo "generate eqs: record access function"

        RecordUpdateExpression _ _ ->
            Debug.todo "generate eqs: record update expression"

        GLSLExpression _ ->
            -- TODO will we need to parse the GLSL language ourselves?
            finish
                [ ( type_
                  , Type
                        (WebGLShader
                            { attributes = Debug.todo "generate eqs: webgl: attributes"
                            , uniforms = Debug.todo "generate eqs: webgl: uniforms"
                            , varyings = Debug.todo "generate eqs: webgl: varyings"
                            }
                        )
                  )
                ]


functionOrValue : ModuleName -> VarName -> TypeOrId -> TIState (List TypeEquation)
functionOrValue moduleName varName type_ =
    State.addVarType moduleName varName type_
        |> State.map (\() -> [])


generateVarEquations : TIState (List TypeEquation)
generateVarEquations =
    State.getVarTypes
        |> State.map
            (\varTypes ->
                varTypes
                    |> Dict.values
                    |> List.fastConcatMap List.consecutivePairs
            )


generatePatternEquations : Dict FullModuleName File -> File -> TypedPattern -> TIState (List TypeEquation)
generatePatternEquations files thisFile ((NodeV2 { type_ } pattern) as typedPattern) =
    let
        f : TypedPattern -> TIState (List TypeEquation)
        f =
            generatePatternEquations files thisFile

        impossiblePattern =
            State.impossiblePattern typedPattern
    in
    case pattern of
        AllPattern ->
            finish []

        UnitPattern ->
            finish [ ( type_, Type Unit ) ]

        CharPattern _ ->
            finish [ ( type_, Type Char ) ]

        StringPattern _ ->
            finish [ ( type_, Type String ) ]

        IntPattern _ ->
            -- TODO number / float?
            finish [ ( type_, Type Int ) ]

        HexPattern _ ->
            -- TODO number / float?
            finish [ ( type_, Type Int ) ]

        FloatPattern _ ->
            finish [ ( type_, Type Float ) ]

        TuplePattern ([ NodeV2 m1 _, NodeV2 m2 _ ] as patterns) ->
            list f patterns
                |> append [ ( type_, Type (Tuple m1.type_ m2.type_) ) ]

        TuplePattern ([ NodeV2 m1 _, NodeV2 m2 _, NodeV2 m3 _ ] as patterns) ->
            list f patterns
                |> append [ ( type_, Type (Tuple3 m1.type_ m2.type_ m3.type_) ) ]

        TuplePattern _ ->
            impossiblePattern

        RecordPattern _ ->
            Debug.todo "record pattern eqs"

        UnConsPattern ((NodeV2 m1 _) as p1) ((NodeV2 m2 _) as p2) ->
            list f [ p1, p2 ]
                |> append
                    [ ( type_, Type (List m1.type_) )
                    , ( type_, m2.type_ )
                    ]

        ListPattern patterns ->
            let
                homogenousListEqs : List TypeEquation
                homogenousListEqs =
                    List.mapConsecutivePairs
                        (\a b ->
                            [ ( NodeV2.type_ a
                              , NodeV2.type_ b
                              )
                            ]
                        )
                        patterns
                        |> List.fastConcat

                firstItemEq : Id -> List TypeEquation
                firstItemEq listItemId =
                    case patterns of
                        [] ->
                            []

                        (NodeV2 mx _) :: _ ->
                            [ ( Id listItemId, mx.type_ ) ]
            in
            State.do State.getNextIdAndTick <| \listItemId ->
            -- TODO all items against each other
            list f patterns
                |> append
                    (( type_, Type (List (Id listItemId)) )
                        :: firstItemEq listItemId
                        ++ homogenousListEqs
                    )

        VarPattern _ ->
            -- TODO should we remember that var for later use in exprs?
            finish []

        NamedPattern customType args ->
            State.map2
                (\fullModuleName argEquations ->
                    ( type_
                    , Type
                        (UserDefinedType
                            { moduleName = fullModuleName
                            , name = customType.name
                            , args = List.map NodeV2.type_ args
                            }
                        )
                    )
                        :: List.fastConcat argEquations
                )
                (State.findModuleOfVar
                    files
                    thisFile
                    (FullModuleName.fromModuleName customType.moduleName)
                    customType.name
                )
                (State.traverse f args)

        AsPattern p1 _ ->
            -- TODO should we remember that var for later use in exprs?
            f p1

        ParenthesizedPattern p1 ->
            f p1
