module Elm.TypeInference.GenerateEquations exposing
    ( generateExprEquations
    , generatePatternEquations
    , generateVarEquations
    )

import Dict exposing (Dict)
import Elm.Syntax.ExpressionV2
    exposing
        ( ExpressionV2(..)
        , FunctionImplementationV2
        , LetDeclaration(..)
        , RecordSetter
        , TypedExpr
        )
import Elm.Syntax.File exposing (File)
import Elm.Syntax.File.Extra as File
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.NodeV2 as NodeV2
    exposing
        ( LocatedNode
        , NodeV2(..)
        , TypedMeta
        )
import Elm.Syntax.PatternV2
    exposing
        ( PatternV2(..)
        , PatternWith
        , TypedPattern
        )
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.State.VarModuleLookup as StateLookup
import Elm.TypeInference.Type as Type
    exposing
        ( Id
        , MonoType(..)
        )
import Elm.TypeInference.Type.External as ExternalType
import Elm.TypeInference.TypeEquation exposing (TypeEquation)
import List.ExtraExtra as List
import Regex exposing (Regex)
import Result.Extra as Result


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
            State.error <| ImpossibleExpr typedExpr

        recordSetters :
            String
            -> List (LocatedNode (RecordSetter TypedMeta))
            -> TIState ( Dict VarName MonoType, List TypedExpr, List TypeEquation )
        recordSetters label fieldSetters =
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
                            , label ++ ": record field"
                            )
                        )
                        fieldSetters
                        fieldIds
            in
            State.pure ( fields, subexprs, equations )
    in
    case expr of
        UnitExpr ->
            finish [ ( type_, Type.mono Unit, "Unit" ) ]

        Application [] ->
            impossibleExpr

        Application ((fn :: args) as exprs) ->
            State.do State.getNextIdAndTick <| \resultId ->
            State.do (State.traverse (always State.getNextIdAndTick) args) <| \argIds ->
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
            append
                (( type_, Type.id resultId, "Application = its result" )
                    :: ( NodeV2.type_ fn, fnType, "Application: first is fn" )
                    :: List.map2
                        (\arg_ argId ->
                            ( NodeV2.type_ arg_
                            , Type.id argId
                            , "Application: args = their id"
                            )
                        )
                        args
                        argIds
                )
                (list f exprs)

        OperatorApplication operator _ e1 e2 ->
            State.do (StateLookup.findModuleOfVar files thisFile Nothing operator) <| \moduleName ->
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
                [ ( type_, Type.id resultId, "Op application = its result" )
                , ( NodeV2.type_ e1, Type.id e1Id, "Op application: left" )
                , ( NodeV2.type_ e2, Type.id e2Id, "Op application: right" )
                ]
                (list f [ e1, e2 ])

        FunctionOrValue moduleName varName ->
            case
                StateLookup.moduleOfVar
                    files
                    thisFile
                    (FullModuleName.fromModuleName moduleName)
                    varName
            of
                Ok (Just fullModuleName) ->
                    State.do (State.addVarType fullModuleName varName type_) <| \() ->
                    finish []

                Ok Nothing ->
                    State.do (State.lookupEnv (File.moduleName thisFile) varName) <| \varType ->
                    finish [ ( type_, Type.mono varType, "FunctionOrValue: var from env" ) ]

                Err err ->
                    State.error err

        IfBlock ((NodeV2 m1 _) as e1) ((NodeV2 m2 _) as e2) ((NodeV2 m3 _) as e3) ->
            list f [ e1, e2, e3 ]
                |> append
                    [ ( m1.type_, Type.mono Bool, "If: condition = bool" )
                    , ( m2.type_, m3.type_, "If: then = else" )
                    , ( m2.type_, type_, "If: then = result" )
                    ]

        PrefixOperator operator ->
            -- operator is a function of two arguments
            State.do (StateLookup.findModuleOfVar files thisFile Nothing operator) <| \moduleName ->
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
                  , "Prefix operator: is a fn"
                  )
                ]

        Operator _ ->
            impossibleExpr

        Integer _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number numberId, "Int" ) ]

        Hex _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number numberId, "Hex" ) ]

        Floatable _ ->
            finish [ ( type_, Type.mono Float, "Float" ) ]

        Negation e1 ->
            State.do State.getNextIdAndTick <| \numberId ->
            f e1
                |> append
                    [ ( type_, NodeV2.type_ e1, "Negation = inner" )
                    , ( type_, Type.number numberId, "Negation = number" )
                    ]

        Literal _ ->
            finish [ ( type_, Type.mono String, "String" ) ]

        CharLiteral _ ->
            finish [ ( type_, Type.mono Char, "Char" ) ]

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
                      , "Tuple: top"
                      )
                    , ( m1.type_, Type.id firstId, "Tuple: first" )
                    , ( m2.type_, Type.id secondId, "Tuple: second" )
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
                      , "Tuple3: top"
                      )
                    , ( m1.type_, Type.id firstId, "Tuple3: first" )
                    , ( m2.type_, Type.id secondId, "Tuple3: second" )
                    , ( m3.type_, Type.id thirdId, "Tuple3: third" )
                    ]

        TupledExpression _ ->
            impossibleExpr

        ParenthesizedExpression e ->
            f e
                |> append [ ( type_, NodeV2.type_ e, "Parenthesized = inner" ) ]

        LetExpression { declarations, expression } ->
            let
                exprType =
                    NodeV2.type_ expression

                generateFnImplementation : Id -> LocatedNode (FunctionImplementationV2 TypedMeta) -> TIState (List TypeEquation)
                generateFnImplementation declId implNode =
                    let
                        impl =
                            NodeV2.value implNode
                                |> Debug.log "generate fn implementation"
                    in
                    if List.isEmpty impl.arguments then
                        generateConstantImplementation declId implNode impl

                    else
                        generateFnWithArgumentsImplementation declId implNode impl

                generateConstantImplementation : Id -> LocatedNode (FunctionImplementationV2 TypedMeta) -> FunctionImplementationV2 TypedMeta -> TIState (List TypeEquation)
                generateConstantImplementation declId implNode impl =
                    -- let x = e1 in e2
                    State.do State.getNextIdAndTick <| \resultId ->
                    State.do (f impl.expression) <| \bodyEqs ->
                    finish <|
                        ( NodeV2.type_ expression, Type.id resultId, "Let constant binding: expr = result" )
                            :: bodyEqs

                generateFnWithArgumentsImplementation : Id -> LocatedNode (FunctionImplementationV2 TypedMeta) -> FunctionImplementationV2 TypedMeta -> TIState (List TypeEquation)
                generateFnWithArgumentsImplementation declId implNode impl =
                    -- This is really similar to LambdaExpression over in `generateExprEquations`:
                    State.do (State.traverse (always State.getNextIdAndTick) impl.arguments) <| \argIds ->
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
                    State.do (list (generatePatternEquations files thisFile) impl.arguments) <| \argPatternEqs ->
                    State.do (State.traverse addPatternBinding (List.map2 Tuple.pair impl.arguments argIds)) <| \_ ->
                    State.do (f impl.expression) <| \bodyEqs ->
                    finish <|
                        ( type_, fnType, "Let function binding: is a function" )
                            :: ( NodeV2.type_ expression, Type.id resultId, "Let function binding: expr = result" )
                            :: List.map2
                                (\arg argId ->
                                    ( NodeV2.type_ arg
                                    , Type.id argId
                                    , "Let function binding: args = their id"
                                    )
                                )
                                impl.arguments
                                argIds
                            ++ bodyEqs
                            ++ argPatternEqs

                generateSignature : Id -> Maybe (LocatedNode Signature) -> TIState (List TypeEquation)
                generateSignature declId maybeSigNode =
                    maybeSigNode
                        |> Maybe.map
                            (NodeV2.value
                                >> .typeAnnotation
                                >> Node.value
                                >> Type.fromTypeAnnotation
                                >> Result.mapError (State.error << ImpossibleType)
                                >> Result.map
                                    (\annotationType ->
                                        finish
                                            [ ( Type.id declId
                                              , Type.mono annotationType
                                              , "Let: binding must be consistent with its annotation"
                                              )
                                            ]
                                    )
                                >> Result.merge
                            )
                        |> Maybe.withDefault (finish [])

                generateDecl : ( LetDeclaration TypedMeta, Id ) -> TIState (List TypeEquation)
                generateDecl ( decl, declId ) =
                    case decl of
                        LetFunction fn ->
                            State.map2 (++)
                                (generateSignature declId fn.signature)
                                (generateFnImplementation declId fn.declaration)

                        LetDestructuring pattern e ->
                            State.map2 (++)
                                (generatePatternEquations files thisFile pattern)
                                (f e)

                addDeclBinding : ( LetDeclaration TypedMeta, Id ) -> TIState ()
                addDeclBinding ( decl, declId ) =
                    case decl of
                        LetFunction fn ->
                            let
                                impl =
                                    NodeV2.value fn.declaration

                                varName =
                                    NodeV2.value impl.name
                            in
                            State.addBinding varName (Type.id declId)

                        LetDestructuring pattern e ->
                            addPatternBinding ( pattern, declId )
            in
            State.do (State.traverse (always State.getNextIdAndTick) declarations) <| \declIds ->
            let
                declsWithIds : List ( LetDeclaration TypedMeta, Id )
                declsWithIds =
                    List.map2 (\declNode declId -> ( NodeV2.value declNode, declId )) declarations declIds
            in
            State.do (State.traverse generateDecl declsWithIds) <| \declEqsLists ->
            State.do (State.traverse addDeclBinding declsWithIds) <| \_ ->
            State.do (f expression) <| \exprEqs ->
            -- TODO each decl needs to be generalized
            -- TODO decls need to be put into expr's env before inferring
            --
            -- TODO let bindingType = generalize (substituteEnv subst typeEnv) (substituteMono subst bindingMonoType)
            -- TODO (exprType,exprEqs) <- withBinding (name, bindingType) <| withSubstitution subst (infer expression)
            finish <|
                ( type_, exprType, "Let = its body" )
                    :: List.fastConcat declEqsLists
                    ++ exprEqs

        CaseExpression _ ->
            Debug.todo "generate eqs: case"

        LambdaExpression { args, expression } ->
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
            State.do (list (generatePatternEquations files thisFile) args) <| \argPatternEqs ->
            State.do (State.traverse addPatternBinding (List.map2 Tuple.pair args argIds)) <| \_ ->
            State.do (f expression) <| \bodyEqs ->
            finish <|
                ( type_, fnType, "Lambda: is a function" )
                    :: ( NodeV2.type_ expression, Type.id resultId, "Lambda: expr = result" )
                    :: List.map2
                        (\arg argId ->
                            ( NodeV2.type_ arg
                            , Type.id argId
                            , "Lambda: args = their id"
                            )
                        )
                        args
                        argIds
                    ++ bodyEqs
                    ++ argPatternEqs

        RecordExpr fieldSetters ->
            State.do (recordSetters "Record" fieldSetters) <| \( fields, subexprs, fieldEquations ) ->
            append
                (( type_, Type.mono <| Record fields, "Record: is a record" )
                    :: fieldEquations
                )
                (list f subexprs)

        ListExpr exprs ->
            State.do (list f exprs) <| \exprsEquations ->
            State.do State.getNextIdAndTick <| \listItemId ->
            finish
                (( type_, Type.mono <| List <| Type.id_ listItemId, "List: is a list" )
                    :: exprsEquations
                    ++ List.map
                        (\(NodeV2 m _) ->
                            ( m.type_
                            , Type.id listItemId
                            , "List: pin list type param to all inner"
                            )
                        )
                        exprs
                )

        RecordAccess record fieldNameNode ->
            State.do State.getNextIdAndTick <| \extensibleRecordId ->
            State.do State.getNextIdAndTick <| \resultId ->
            finish
                [ ( type_, Type.id resultId, "Record access = the field = the result" )
                , ( NodeV2.type_ record
                  , Type.mono <|
                        ExtensibleRecord
                            { type_ = Type.id_ extensibleRecordId
                            , fields =
                                Dict.singleton
                                    (NodeV2.value fieldNameNode)
                                    (Type.id_ resultId)
                            }
                  , "Record access: left is a record"
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
                                    , fields =
                                        -- the fieldName is ".a", not "a", so let's sanitize that
                                        Dict.singleton (String.dropLeft 1 fieldName) (Type.id_ resultId)
                                    }
                            , to = Type.id_ resultId
                            }
                  , "Record access fn: is a function"
                  )
                ]

        RecordUpdateExpression recordVarNode fieldSetters ->
            let
                recordVar =
                    NodeV2.value recordVarNode
            in
            State.do State.getNextIdAndTick <| \recordId ->
            State.do (StateLookup.findModuleOfVar files thisFile Nothing recordVar) <| \moduleName ->
            State.do (State.addVarType moduleName recordVar (Type.id recordId)) <| \() ->
            State.do (recordSetters "Record update" fieldSetters) <| \( fields, subexprs, fieldEquations ) ->
            append
                (( type_
                 , Type.mono <|
                    ExtensibleRecord
                        { type_ = Type.id_ recordId
                        , fields = fields
                        }
                 , "Record update: is record with at least that field"
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
                  , "GLSLExpression: is a shader"
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
                    |> List.fastConcatMap
                        (List.mapConsecutivePairs (\t1 t2 -> ( t1, t2, "vars linked" )))
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
            finish [ ( type_, Type.mono Unit, "Unit pattern" ) ]

        CharPattern _ ->
            finish [ ( type_, Type.mono Char, "Char pattern" ) ]

        StringPattern _ ->
            finish [ ( type_, Type.mono String, "String pattern" ) ]

        IntPattern _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number numberId, "Int pattern" ) ]

        HexPattern _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number numberId, "Hex pattern" ) ]

        FloatPattern _ ->
            finish [ ( type_, Type.mono Float, "Float pattern" ) ]

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
                      , "Tuple pattern: top"
                      )
                    , ( m1.type_, Type.id firstId, "Tuple pattern: first" )
                    , ( m2.type_, Type.id secondId, "Tuple pattern: second" )
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
                      , "Tuple3 pattern: top"
                      )
                    , ( m1.type_, Type.id firstId, "Tuple3 pattern: first" )
                    , ( m2.type_, Type.id secondId, "Tuple3 pattern: second" )
                    , ( m3.type_, Type.id thirdId, "Tuple3 pattern: third" )
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
                      , "Record pattern"
                      )
                    ]
                )
                fieldsDict
                State.getNextIdAndTick

        UnConsPattern ((NodeV2 m1 _) as p1) ((NodeV2 m2 _) as p2) ->
            State.do State.getNextIdAndTick <| \listItemId ->
            list f [ p1, p2 ]
                |> append
                    [ ( type_, Type.mono <| List <| Type.id_ listItemId, "UnCons pattern: result" )
                    , ( type_, m2.type_, "UnCons pattern: result same as tail" )
                    , ( m1.type_, Type.id listItemId, "UnCons pattern: head pins list type param" )
                    ]

        ListPattern patterns ->
            let
                homogenousListEqs : List TypeEquation
                homogenousListEqs =
                    List.mapConsecutivePairs
                        (\a b ->
                            [ ( NodeV2.type_ a
                              , NodeV2.type_ b
                              , "ListPattern: homogenous"
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
                            [ ( mx.type_, Type.id listItemId, "ListPattern: first item pins list type param" ) ]
            in
            State.do State.getNextIdAndTick <| \listItemId ->
            list f patterns
                |> append
                    (( type_, Type.mono <| List <| Type.id_ listItemId, "ListPattern: result" )
                        :: firstItemEq listItemId
                        ++ homogenousListEqs
                    )

        VarPattern var ->
            State.do State.getNextIdAndTick <| \patternId ->
            let
                id =
                    Type.id patternId
            in
            State.do (State.addBinding var id) <| \() ->
            finish [ ( type_, id, "VarPattern" ) ]

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
                    , "NamedPattern: is user defined type"
                    )
                        :: List.fastConcat argEquations
                        ++ List.map2
                            (\argNode argId ->
                                ( NodeV2.type_ argNode
                                , Type.id argId
                                , "NamedPattern: args = their ids"
                                )
                            )
                            args
                            argIds
                )
                (StateLookup.findModuleOfVar
                    files
                    thisFile
                    (FullModuleName.fromModuleName customType.moduleName)
                    customType.name
                )
                (State.traverse f args)
                (State.traverse (always State.getNextIdAndTick) args)

        AsPattern p1 varName ->
            -- TODO did this help?
            State.do (State.addBinding (NodeV2.value varName) type_) <| \() ->
            f p1

        ParenthesizedPattern p1 ->
            f p1


addPatternBinding : ( PatternWith meta, Id ) -> TIState ()
addPatternBinding ( arg, argId ) =
    case NodeV2.value arg of
        VarPattern varName ->
            State.addBinding varName (Type.id argId)

        AsPattern _ varName ->
            State.addBinding (NodeV2.value varName) (Type.id argId)

        RecordPattern fields ->
            fields
                |> State.traverse
                    (\field ->
                        State.do State.getNextIdAndTick <| \fieldId ->
                        State.addBinding (NodeV2.value field) (Type.id fieldId)
                    )
                |> State.map (always ())

        _ ->
            State.pure ()
