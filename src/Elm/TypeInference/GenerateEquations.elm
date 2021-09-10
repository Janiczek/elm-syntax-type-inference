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
import Elm.TypeInference.Type.External as ExternalType
import Elm.TypeInference.TypeEquation exposing (TypeEquation)
import List.ExtraExtra as List
import Regex exposing (Regex)


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
    -> TIState ()
generateExprEquations files thisFile expr =
    State.do (generateExprEquations_ files thisFile expr) <| \equations ->
    State.addTypeEquations equations


generateExprEquations_ :
    Dict FullModuleName File
    -> File
    -> TypedExpr
    -> TIState (List TypeEquation)
generateExprEquations_ files thisFile ((NodeV2 { type_ } expr) as typedExpr) =
    let
        f : TypedExpr -> TIState (List TypeEquation)
        f =
            generateExprEquations_ files thisFile

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

        Negation e1 ->
            f e1
                |> append
                    [ ( type_, NodeV2.type_ e1 )
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
                |> append [ ( type_, NodeV2.type_ e ) ]

        LetExpression _ ->
            Debug.todo "generate eqs: let"

        CaseExpression _ ->
            Debug.todo "generate eqs: case"

        LambdaExpression { args, expression } ->
            let
                resultType =
                    NodeV2.type_ expression

                fnType =
                    args
                        |> List.map NodeV2.type_
                        |> List.foldr
                            (\rightArgType leftArgType ->
                                Type <|
                                    Function
                                        { from = leftArgType
                                        , to = rightArgType
                                        }
                            )
                            resultType
            in
            State.map2
                (\bodyEqs argPatternEqs ->
                    ( type_, fnType )
                        :: bodyEqs
                        ++ argPatternEqs
                )
                (f expression)
                (list (generatePatternEquations_ files thisFile) args)

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

        GLSLExpression code ->
            {- TODO This currently only correctly detects "simple" declarations:

                   uniform mat4 u_worldViewProjection;
                   uniform vec3 u_lightWorldPos;
                   attribute vec4 a_position;
                   attribute vec2 a_texcoord;
                   varying vec4 v_position;
                   varying vec2 v_texcoord;

               and so on. Anything more advanced will probably not be picked up correctly:

                   uniform /* hello */ mat4 u_x, u_y, u_z;
                   attribute
                     vec4 a_position
                        ;

               It would be great to write a more precise parser that allows arbitrary
               whitespace and comments in between the uniform/varying/attribute declarations.

               Prior art:
                * https://github.com/noteed/language-glsl/blob/master/Language/GLSL/Parser.hs
                  * what elm/compiler uses under the hood
                * https://github.com/shuhei/elm-compiler/blob/glsl-parser/compiler/src/Parse/Shader.hs
                  * this one might be doing the least work
                * https://github.com/w0rm/elm-glsl/blob/main/Language/GLSL/NewParser.hs
                  * written using elm-parser-like primitives
            -}
            let
                declarations :
                    { uniforms : Dict VarName TypeOrId
                    , attributes : Dict VarName TypeOrId
                    , varyings : Dict VarName TypeOrId
                    }
                declarations =
                    code
                        |> Regex.find glslDeclarationRegex
                        |> List.foldl
                            (\{ submatches } acc ->
                                case submatches of
                                    [ Just storageQualifier, Just varType, Just varName ] ->
                                        parseGlslVarType varType
                                            |> Maybe.map
                                                (\varType_ ->
                                                    case storageQualifier of
                                                        "attribute" ->
                                                            { acc | attributes = Dict.insert varName (Type varType_) acc.attributes }

                                                        "varying" ->
                                                            { acc | varyings = Dict.insert varName (Type varType_) acc.varyings }

                                                        "uniform" ->
                                                            { acc | uniforms = Dict.insert varName (Type varType_) acc.uniforms }

                                                        _ ->
                                                            acc
                                                )
                                            |> Maybe.withDefault acc

                                    _ ->
                                        acc
                            )
                            { uniforms = Dict.empty
                            , attributes = Dict.empty
                            , varyings = Dict.empty
                            }
            in
            finish
                [ ( type_
                  , Type
                        (WebGLShader
                            { attributes = declarations.attributes
                            , uniforms = declarations.uniforms
                            , varyings = declarations.varyings
                            }
                        )
                  )
                ]


parseGlslVarType : String -> Maybe Type
parseGlslVarType type_ =
    case type_ of
        "vec2" ->
            Just ExternalType.vec2

        "vec3" ->
            Just ExternalType.vec3

        "vec4" ->
            Just ExternalType.vec4

        "mat4" ->
            Just ExternalType.mat4

        "sampler2d" ->
            Just ExternalType.texture

        "int" ->
            Just Int

        "float" ->
            Just Float

        _ ->
            Nothing


glslDeclarationRegex : Regex
glslDeclarationRegex =
    Regex.fromString "^(uniform|attribute|varying)\\s+([^\\s]+)\\s+([^;]+);$"
        |> Maybe.withDefault Regex.never


generateVarEquations : TIState ()
generateVarEquations =
    State.do State.getVarTypes <| \varTypes ->
    varTypes
        |> Dict.values
        |> List.fastConcatMap List.consecutivePairs
        |> State.addTypeEquations


generatePatternEquations :
    Dict FullModuleName File
    -> File
    -> TypedPattern
    -> TIState ()
generatePatternEquations files thisFile pattern =
    State.do (generatePatternEquations_ files thisFile pattern) <| \equations ->
    State.addTypeEquations equations


generatePatternEquations_ : Dict FullModuleName File -> File -> TypedPattern -> TIState (List TypeEquation)
generatePatternEquations_ files thisFile ((NodeV2 { type_ } pattern) as typedPattern) =
    let
        f : TypedPattern -> TIState (List TypeEquation)
        f =
            generatePatternEquations_ files thisFile

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
            finish [ ( type_, Type Number ) ]

        HexPattern _ ->
            finish [ ( type_, Type Number ) ]

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
