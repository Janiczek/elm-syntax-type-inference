module Elm.TypeInference.GenerateEquations exposing
    ( generateExprEquations
    , generatePatternEquations
    , generateVarEquations
    )

import Dict exposing (Dict)
import Elm.Syntax.ExpressionV2
    exposing
        ( ExpressionV2(..)
        , RecordSetter
        , TypedExpr
        )
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.NodeV2 as NodeV2
    exposing
        ( LocatedNode
        , NodeV2(..)
        , TypedMeta
        )
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


generateExprEquations :
    Dict FullModuleName File
    -> File
    -> TypedExpr
    -> TIState (List TypeEquation)
generateExprEquations files thisFile ((NodeV2 { type_ } expr) as typedExpr) =
    let
        f : TypedExpr -> TIState (List TypeEquation)
        f =
            generateExprEquations files thisFile

        impossibleExpr =
            State.impossibleExpr typedExpr

        recordSetters : List (LocatedNode (RecordSetter TypedMeta)) -> ( Dict VarName TypeOrId, List TypedExpr )
        recordSetters fieldSetters =
            let
                fields : Dict VarName TypeOrId
                fields =
                    fieldSetters
                        |> List.map
                            (\fieldSetterNode ->
                                let
                                    ( fieldNameNode, fieldExpr ) =
                                        NodeV2.value fieldSetterNode
                                in
                                ( NodeV2.value fieldNameNode
                                , NodeV2.type_ fieldExpr
                                )
                            )
                        |> Dict.fromList

                subexprs : List TypedExpr
                subexprs =
                    List.map (NodeV2.value >> Tuple.second) fieldSetters
            in
            ( fields, subexprs )
    in
    case expr of
        UnitExpr ->
            finish [ ( type_, Type Unit ) ]

        Application [] ->
            impossibleExpr

        Application ((fn :: _) as exprs) ->
            State.do State.getNextIdAndTick <| \resultId ->
            let
                fnType =
                    exprs
                        |> List.map NodeV2.type_
                        |> List.foldr
                            (\rightArgType leftArgType ->
                                Type <|
                                    Function
                                        { from = leftArgType
                                        , to = rightArgType
                                        }
                            )
                            (Id resultId)
            in
            append
                [ ( type_, Id resultId )
                , ( NodeV2.type_ fn, fnType )
                ]
                (list f exprs)

        OperatorApplication operator _ e1 e2 ->
            State.do (State.findModuleOfVar files thisFile Nothing operator) <| \moduleName ->
            State.do State.getNextIdAndTick <| \resultId ->
            let
                fnType =
                    Type <|
                        Function
                            { from = NodeV2.type_ e1
                            , to =
                                Type <|
                                    Function
                                        { from = NodeV2.type_ e2
                                        , to = Id resultId
                                        }
                            }
            in
            State.do (State.addVarType moduleName operator fnType) <| \() ->
            append
                [ ( type_, Id resultId ) ]
                (list f [ e1, e2 ])

        FunctionOrValue moduleName varName ->
            State.do
                (State.findModuleOfVar
                    files
                    thisFile
                    (FullModuleName.fromModuleName moduleName)
                    varName
                )
            <| \fullModuleName ->
            State.do (State.addVarType fullModuleName varName type_) <| \() ->
            finish []

        IfBlock ((NodeV2 m1 _) as e1) ((NodeV2 m2 _) as e2) ((NodeV2 m3 _) as e3) ->
            list f [ e1, e2, e3 ]
                |> append
                    [ ( m1.type_, Type Bool )
                    , ( m2.type_, m3.type_ )
                    , ( m2.type_, type_ )
                    ]

        PrefixOperator operator ->
            -- operator is a function of two arguments
            State.do (State.findModuleOfVar files thisFile Nothing operator) <| \moduleName ->
            State.do (State.addVarType moduleName operator type_) <| \() ->
            State.do State.getNextIdAndTick <| \firstArgId ->
            State.do State.getNextIdAndTick <| \secondArgId ->
            State.do State.getNextIdAndTick <| \resultId ->
            finish
                [ ( type_
                  , Type <|
                        Function
                            { from = Id firstArgId
                            , to =
                                Type <|
                                    Function
                                        { from = Id secondArgId
                                        , to = Id resultId
                                        }
                            }
                  )
                ]

        Operator _ ->
            impossibleExpr

        Integer _ ->
            finish [ ( type_, Type Number ) ]

        Hex _ ->
            finish [ ( type_, Type Number ) ]

        Floatable _ ->
            finish [ ( type_, Type Float ) ]

        Negation ((NodeV2 m1 _) as e1) ->
            f e1
                |> append
                    [ ( type_, m1.type_ )
                    , ( type_, Type Number )
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
            f e

        LetExpression _ ->
            Debug.todo "generate eqs: let"

        CaseExpression _ ->
            Debug.todo "generate eqs: case"

        LambdaExpression { args, expression } ->
            -- TODO will need foldr-ing
            -- arg1 <- ... <- argN <- expression
            -- with Function. See `Application` case.
            finish
                [ ( type_
                  , Type <|
                        Function
                            { from = ()
                            , to = ()
                            }
                  )
                ]

        RecordExpr fieldSetters ->
            let
                ( fields, subexprs ) =
                    recordSetters fieldSetters
            in
            append
                [ ( type_, Type (Record fields) ) ]
                (list f subexprs)

        ListExpr exprs ->
            State.do (list f exprs) <| \exprsEquations ->
            State.do State.getNextIdAndTick <| \id ->
            finish
                (( type_, Type (List (Id id)) )
                    :: exprsEquations
                    ++ List.map (\(NodeV2 m _) -> ( m.type_, Id id )) exprs
                )

        RecordAccess record fieldNameNode ->
            State.do State.getNextIdAndTick <| \extensibleRecordId ->
            State.do State.getNextIdAndTick <| \resultId ->
            finish
                [ ( type_, Id resultId )
                , ( NodeV2.type_ record
                  , Type <|
                        ExtensibleRecord
                            { type_ = Id extensibleRecordId
                            , fields = Dict.singleton (NodeV2.value fieldNameNode) (Id resultId)
                            }
                  )
                ]

        RecordAccessFunction fieldName ->
            State.do State.getNextIdAndTick <| \recordId ->
            State.do State.getNextIdAndTick <| \resultId ->
            finish
                [ ( type_
                  , Type <|
                        Function
                            { from =
                                Type <|
                                    ExtensibleRecord
                                        { type_ = Id recordId
                                        , fields = Dict.singleton fieldName (Id resultId)
                                        }
                            , to = Id resultId
                            }
                  )
                ]

        RecordUpdateExpression recordVarNode fieldSetters ->
            let
                recordVar =
                    NodeV2.value recordVarNode
            in
            State.do State.getNextIdAndTick <| \recordId ->
            State.do (State.findModuleOfVar files thisFile Nothing recordVar) <| \moduleName ->
            State.do (State.addVarType moduleName recordVar (Id recordId)) <| \() ->
            let
                ( fields, subexprs ) =
                    recordSetters fieldSetters
            in
            append
                [ ( type_
                  , Type
                        (ExtensibleRecord
                            { type_ = Id recordId
                            , fields = fields
                            }
                        )
                  )
                ]
                (list f subexprs)

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

        RecordPattern fields ->
            {- If we're pattern matching some record fields, we're mandating that
               the thing is a record that contains _at least_ these fields.

               Which is what our ExtensibleRecord type does!
            -}
            let
                fieldsDict : TIState (Dict VarName TypeOrId)
                fieldsDict =
                    fields
                        |> State.traverse
                            (\fieldName ->
                                State.getNextIdAndTick
                                    |> State.map
                                        (\fieldId ->
                                            ( NodeV2.value fieldName
                                            , Id fieldId
                                            )
                                        )
                            )
                        |> State.map Dict.fromList
            in
            State.map2
                (\fieldsDict_ recordId ->
                    [ ( type_
                      , Type
                            (ExtensibleRecord
                                { type_ = Id recordId
                                , fields = fieldsDict_
                                }
                            )
                      )
                    ]
                )
                fieldsDict
                State.getNextIdAndTick

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
