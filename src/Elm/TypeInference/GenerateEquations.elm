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
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.Type as Type
    exposing
        ( Id
        , MonoType(..)
        )
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
    -> TIState (List TypeEquation)
generateExprEquations files thisFile ((NodeV2 { type_ } expr) as typedExpr) =
    -- TODO use Transform.recursiveChildren?
    let
        f : TypedExpr -> TIState (List TypeEquation)
        f =
            generateExprEquations files thisFile

        impossibleExpr =
            State.error <| ImpossibleExpr typedExpr

        recordSetters :
            List (LocatedNode (RecordSetter TypedMeta))
            -> TIState ( Dict VarName MonoType, List TypedExpr, List TypeEquation )
        recordSetters fieldSetters =
            State.do (State.traverse (always State.getNextIdAndTick) fieldSetters) <| \fieldIds ->
            let
                fields : Dict VarName MonoType
                fields =
                    List.map2
                        (\fieldSetterNode fieldId ->
                            let
                                ( fieldNameNode, _ ) =
                                    NodeV2.value fieldSetterNode
                            in
                            ( NodeV2.value fieldNameNode
                            , Type.id_ fieldId
                            )
                        )
                        fieldSetters
                        fieldIds
                        |> Dict.fromList

                subexprs : List TypedExpr
                subexprs =
                    List.map (NodeV2.value >> Tuple.second) fieldSetters

                equations : List TypeEquation
                equations =
                    List.map2
                        (\fieldSetterNode fieldId ->
                            let
                                ( _, fieldExpr ) =
                                    NodeV2.value fieldSetterNode
                            in
                            ( NodeV2.type_ fieldExpr
                            , Type.id fieldId
                            )
                        )
                        fieldSetters
                        fieldIds
            in
            State.pure ( fields, subexprs, equations )
    in
    case expr of
        UnitExpr ->
            finish [ ( type_, Type.mono Unit ) ]

        Application [] ->
            impossibleExpr

        Application ((fn :: _) as exprs) ->
            State.do State.getNextIdAndTick <| \resultId ->
            State.do (State.traverse (always State.getNextIdAndTick) exprs) <| \exprIds ->
            let
                fnType =
                    exprIds
                        |> List.foldr
                            (\rightArgType leftArgType ->
                                Function
                                    { from = leftArgType
                                    , to = Type.id_ rightArgType
                                    }
                            )
                            (Type.id_ resultId)
                        |> Type.mono
            in
            append
                (( type_, Type.id resultId )
                    :: ( NodeV2.type_ fn, fnType )
                    :: List.map2 (\expr_ exprId -> ( NodeV2.type_ expr_, Type.id exprId ))
                        exprs
                        exprIds
                )
                (list f exprs)

        OperatorApplication operator _ e1 e2 ->
            State.do (State.findModuleOfVar files thisFile Nothing operator) <| \moduleName ->
            State.do State.getNextIdAndTick <| \resultId ->
            State.do State.getNextIdAndTick <| \e1Id ->
            State.do State.getNextIdAndTick <| \e2Id ->
            let
                fnType =
                    Type.mono <|
                        Function
                            { from = Type.id_ e1Id
                            , to =
                                Function
                                    { from = Type.id_ e2Id
                                    , to = Type.id_ resultId
                                    }
                            }
            in
            State.do (State.addVarType moduleName operator fnType) <| \() ->
            append
                [ ( type_, Type.id resultId )
                , ( NodeV2.type_ e1, Type.id e1Id )
                , ( NodeV2.type_ e2, Type.id e2Id )
                ]
                (list f [ e1, e2 ])

        FunctionOrValue moduleName varName ->
            {- TODO Diehl uses lookupEnv, do we need it? We seem to have
               a different strategy of "declaration before usage", so perhaps
               we're fine with our `addVarType` and dealing with it afterwards.
            -}
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
                    [ ( m1.type_, Type.mono Bool )
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
                  , Type.mono <|
                        Function
                            { from = Type.id_ firstArgId
                            , to =
                                Function
                                    { from = Type.id_ secondArgId
                                    , to = Type.id_ resultId
                                    }
                            }
                  )
                ]

        Operator _ ->
            impossibleExpr

        Integer _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number numberId ) ]

        Hex _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number numberId ) ]

        Floatable _ ->
            finish [ ( type_, Type.mono Float ) ]

        Negation e1 ->
            State.do State.getNextIdAndTick <| \numberId ->
            f e1
                |> append
                    [ ( type_, NodeV2.type_ e1 )
                    , ( type_, Type.number numberId )
                    ]

        Literal _ ->
            finish [ ( type_, Type.mono String ) ]

        CharLiteral _ ->
            finish [ ( type_, Type.mono Char ) ]

        TupledExpression ([ NodeV2 m1 _, NodeV2 m2 _ ] as exprs) ->
            State.do State.getNextIdAndTick <| \firstId ->
            State.do State.getNextIdAndTick <| \secondId ->
            list f exprs
                |> append
                    [ ( type_
                      , Type.mono <|
                            Tuple
                                (Type.id_ firstId)
                                (Type.id_ secondId)
                      )
                    , ( m1.type_, Type.id firstId )
                    , ( m2.type_, Type.id secondId )
                    ]

        TupledExpression ([ NodeV2 m1 _, NodeV2 m2 _, NodeV2 m3 _ ] as exprs) ->
            State.do State.getNextIdAndTick <| \firstId ->
            State.do State.getNextIdAndTick <| \secondId ->
            State.do State.getNextIdAndTick <| \thirdId ->
            list f exprs
                |> append
                    [ ( type_
                      , Type.mono <|
                            Tuple3
                                (Type.id_ firstId)
                                (Type.id_ secondId)
                                (Type.id_ thirdId)
                      )
                    , ( m1.type_, Type.id firstId )
                    , ( m2.type_, Type.id secondId )
                    , ( m3.type_, Type.id thirdId )
                    ]

        TupledExpression _ ->
            impossibleExpr

        ParenthesizedExpression e ->
            f e
                |> append [ ( type_, NodeV2.type_ e ) ]

        LetExpression _ ->
            -- x == bindingName
            -- e1 == bindingBody
            -- e2 == letBody
            -- TODO infer binding <| \bindingEqs ->
            -- TODO runSolve equations from bindingEqs <| \subst ->
            -- TODO let bindingType = generalize (substituteEnv subst typeEnv) (substituteMono subst bindingMonoType)
            -- TODO (exprType,exprEqs) <- withBinding (name, bindingType) <| withSubstitution subst (infer expression)
            -- TODO (type_, exprType) :: bindingEqs ++ exprEqs
            Debug.todo "generate eqs: let"

        CaseExpression _ ->
            Debug.todo "generate eqs: case"

        LambdaExpression { args, expression } ->
            -- TODO Diehl: inEnv infer ...
            State.do (State.traverse (always State.getNextIdAndTick) args) <| \argIds ->
            State.do State.getNextIdAndTick <| \resultId ->
            let
                fnType =
                    argIds
                        |> List.foldr
                            (\leftArgType rightArgType ->
                                Function
                                    { from = Type.id_ leftArgType
                                    , to = rightArgType
                                    }
                            )
                            (Type.id_ resultId)
                        |> Type.mono
            in
            State.map2
                (\bodyEqs argPatternEqs ->
                    ( type_, fnType )
                        :: ( NodeV2.type_ expression, Type.id resultId )
                        :: List.map2 (\arg argId -> ( NodeV2.type_ arg, Type.id argId ))
                            args
                            argIds
                        ++ bodyEqs
                        ++ argPatternEqs
                )
                (f expression)
                (list (generatePatternEquations files thisFile) args)

        RecordExpr fieldSetters ->
            State.do (recordSetters fieldSetters) <| \( fields, subexprs, fieldEquations ) ->
            append
                (( type_, Type.mono <| Record fields )
                    :: fieldEquations
                )
                (list f subexprs)

        ListExpr exprs ->
            State.do (list f exprs) <| \exprsEquations ->
            State.do State.getNextIdAndTick <| \listItemId ->
            finish
                (( type_, Type.mono <| List <| Type.id_ listItemId )
                    :: exprsEquations
                    ++ List.map (\(NodeV2 m _) -> ( m.type_, Type.id listItemId )) exprs
                )

        RecordAccess record fieldNameNode ->
            State.do State.getNextIdAndTick <| \extensibleRecordId ->
            State.do State.getNextIdAndTick <| \resultId ->
            finish
                [ ( type_, Type.id resultId )
                , ( NodeV2.type_ record
                  , Type.mono <|
                        ExtensibleRecord
                            { type_ = Type.id_ extensibleRecordId
                            , fields =
                                Dict.singleton
                                    (NodeV2.value fieldNameNode)
                                    (Type.id_ resultId)
                            }
                  )
                ]

        RecordAccessFunction fieldName ->
            State.do State.getNextIdAndTick <| \recordId ->
            State.do State.getNextIdAndTick <| \resultId ->
            finish
                [ ( type_
                  , Type.mono <|
                        Function
                            { from =
                                ExtensibleRecord
                                    { type_ = Type.id_ recordId
                                    , fields = Dict.singleton fieldName (Type.id_ resultId)
                                    }
                            , to = Type.id_ resultId
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
            State.do (State.addVarType moduleName recordVar (Type.id recordId)) <| \() ->
            State.do (recordSetters fieldSetters) <| \( fields, subexprs, fieldEquations ) ->
            append
                (( type_
                 , Type.mono <|
                    ExtensibleRecord
                        { type_ = Type.id_ recordId
                        , fields = fields
                        }
                 )
                    :: fieldEquations
                )
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
                    { uniforms : Dict VarName MonoType
                    , attributes : Dict VarName MonoType
                    , varyings : Dict VarName MonoType
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
                                                            { acc | attributes = Dict.insert varName varType_ acc.attributes }

                                                        "varying" ->
                                                            { acc | varyings = Dict.insert varName varType_ acc.varyings }

                                                        "uniform" ->
                                                            { acc | uniforms = Dict.insert varName varType_ acc.uniforms }

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
                  , Type.mono <|
                        WebGLShader
                            { attributes = declarations.attributes
                            , uniforms = declarations.uniforms
                            , varyings = declarations.varyings
                            }
                  )
                ]


parseGlslVarType : String -> Maybe MonoType
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


generateVarEquations : TIState (List TypeEquation)
generateVarEquations =
    State.getVarTypes
        |> State.map
            (\varTypes ->
                varTypes
                    |> Dict.values
                    |> List.fastConcatMap List.consecutivePairs
            )


generatePatternEquations :
    Dict FullModuleName File
    -> File
    -> TypedPattern
    -> TIState (List TypeEquation)
generatePatternEquations files thisFile ((NodeV2 { type_ } pattern) as typedPattern) =
    -- TODO use Transform.recursiveChildren?
    let
        f : TypedPattern -> TIState (List TypeEquation)
        f =
            generatePatternEquations files thisFile

        impossiblePattern =
            State.error <| ImpossiblePattern typedPattern
    in
    case pattern of
        AllPattern ->
            finish []

        UnitPattern ->
            finish [ ( type_, Type.mono Unit ) ]

        CharPattern _ ->
            finish [ ( type_, Type.mono Char ) ]

        StringPattern _ ->
            finish [ ( type_, Type.mono String ) ]

        IntPattern _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number numberId ) ]

        HexPattern _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number numberId ) ]

        FloatPattern _ ->
            finish [ ( type_, Type.mono Float ) ]

        TuplePattern ([ NodeV2 m1 _, NodeV2 m2 _ ] as patterns) ->
            State.do State.getNextIdAndTick <| \firstId ->
            State.do State.getNextIdAndTick <| \secondId ->
            list f patterns
                |> append
                    [ ( type_
                      , Type.mono <|
                            Tuple
                                (Type.id_ firstId)
                                (Type.id_ secondId)
                      )
                    , ( m1.type_, Type.id firstId )
                    , ( m2.type_, Type.id secondId )
                    ]

        TuplePattern ([ NodeV2 m1 _, NodeV2 m2 _, NodeV2 m3 _ ] as patterns) ->
            State.do State.getNextIdAndTick <| \firstId ->
            State.do State.getNextIdAndTick <| \secondId ->
            State.do State.getNextIdAndTick <| \thirdId ->
            list f patterns
                |> append
                    [ ( type_
                      , Type.mono <|
                            Tuple3
                                (Type.id_ firstId)
                                (Type.id_ secondId)
                                (Type.id_ thirdId)
                      )
                    , ( m1.type_, Type.id firstId )
                    , ( m2.type_, Type.id secondId )
                    , ( m3.type_, Type.id thirdId )
                    ]

        TuplePattern _ ->
            impossiblePattern

        RecordPattern fields ->
            {- If we're pattern matching some record fields, we're mandating that
               the thing is a record that contains _at least_ these fields.

               Which is what our ExtensibleRecord type does!
            -}
            let
                fieldsDict : TIState (Dict VarName MonoType)
                fieldsDict =
                    fields
                        |> State.traverse
                            (\fieldName ->
                                State.getNextIdAndTick
                                    |> State.map
                                        (\fieldId ->
                                            ( NodeV2.value fieldName
                                            , Type.id_ fieldId
                                            )
                                        )
                            )
                        |> State.map Dict.fromList
            in
            State.map2
                (\fieldsDict_ recordId ->
                    [ ( type_
                      , Type.mono <|
                            ExtensibleRecord
                                { type_ = Type.id_ recordId
                                , fields = fieldsDict_
                                }
                      )
                    ]
                )
                fieldsDict
                State.getNextIdAndTick

        UnConsPattern ((NodeV2 m1 _) as p1) ((NodeV2 m2 _) as p2) ->
            State.do State.getNextIdAndTick <| \listItemId ->
            list f [ p1, p2 ]
                |> append
                    [ ( type_, Type.mono <| List <| Type.id_ listItemId )
                    , ( type_, m2.type_ )
                    , ( m1.type_, Type.id listItemId )
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
                            [ ( mx.type_, Type.id listItemId ) ]
            in
            State.do State.getNextIdAndTick <| \listItemId ->
            list f patterns
                |> append
                    (( type_, Type.mono <| List <| Type.id_ listItemId )
                        :: firstItemEq listItemId
                        ++ homogenousListEqs
                    )

        VarPattern _ ->
            -- TODO should we remember that var for later use in exprs?
            finish []

        NamedPattern customType args ->
            State.map3
                (\fullModuleName argEquations argIds ->
                    ( type_
                    , Type.mono <|
                        UserDefinedType
                            { moduleName = fullModuleName
                            , name = customType.name
                            , args = List.map Type.id_ argIds
                            }
                    )
                        :: List.fastConcat argEquations
                        ++ List.map2 (\argNode argId -> ( NodeV2.type_ argNode, Type.id argId ))
                            args
                            argIds
                )
                (State.findModuleOfVar
                    files
                    thisFile
                    (FullModuleName.fromModuleName customType.moduleName)
                    customType.name
                )
                (State.traverse f args)
                (State.traverse (always State.getNextIdAndTick) args)

        AsPattern p1 _ ->
            -- TODO should we remember that var for later use in exprs?
            f p1

        ParenthesizedPattern p1 ->
            f p1
