module Elm.TypeInference.Infer exposing
    ( Ctx
    , inferDeclaration
    , varEquations
    )

{-| A traversal over elm-syntax AST

  - Every node gets a fresh type ID.
  - Each walk function returns that ID along with the type equations it generated.
  - Parents can refer to children's types directly by inspecting the return value of the recursive call.

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Signature exposing (Signature)
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


type alias Ctx =
    { files : Dict FullModuleName File
    , thisFile : File
    , thisModuleName : FullModuleName
    }


type alias Inferred =
    ( Id, List TypeEquation )


inferMany : (a -> TIState Inferred) -> List a -> TIState ( List Id, List TypeEquation )
inferMany f items =
    State.traverse f items
        |> State.map
            (\inferreds ->
                ( List.map Tuple.first inferreds
                , List.fastConcatMap Tuple.second inferreds
                )
            )


{-| `arg1 -> arg2 -> ... -> result`
-}
functionType : List Id -> Id -> MonoType
functionType argIds resultId =
    argIds
        |> List.foldr
            (\argId toType -> Function { from = Type.id_ argId, to = toType })
            (Type.id_ resultId)



-- DECLARATIONS


inferDeclaration : Ctx -> Node Declaration -> TIState (List TypeEquation)
inferDeclaration ctx declNode =
    case Node.value declNode of
        Declaration.FunctionDeclaration fn ->
            -- TODO also bind the type annotation against the inferred type: fn.signature
            State.do (State.idForNode ctx.thisModuleName declNode) <| \declId ->
            State.do (aliasImplementation ctx declId fn.declaration) <| \impl ->
            inferImplementation ctx declId impl

        Declaration.Destructuring patternNode exprNode ->
            State.map2
                (\( _, patternEqs ) ( _, exprEqs ) -> patternEqs ++ exprEqs)
                (inferPattern ctx patternNode)
                (inferExpr ctx exprNode)

        -- Type aliases, custom types, ports and infixes contribute no nodes.
        _ ->
            State.pure []


{-| The `FunctionImplementation` node (and the name node inside it) want the same
type as the declaration they belong to -- and in the case of the implementation,
elm-syntax even gives it the very same range when there's no documentation and no
signature.
-}
aliasImplementation :
    Ctx
    -> Id
    -> Node Expression.FunctionImplementation
    -> TIState Expression.FunctionImplementation
aliasImplementation ctx declId implNode =
    let
        impl : Expression.FunctionImplementation
        impl =
            Node.value implNode
    in
    State.do (State.aliasNodeId ctx.thisModuleName (Node.range implNode) declId) <| \() ->
    State.do (State.aliasNodeId ctx.thisModuleName (Node.range impl.name) declId) <| \() ->
    State.pure impl


inferImplementation : Ctx -> Id -> Expression.FunctionImplementation -> TIState (List TypeEquation)
inferImplementation ctx declId impl =
    State.do (inferMany (inferPattern ctx) impl.arguments) <| \( argIds, argEqs ) ->
    State.do (inferExpr ctx impl.expression) <| \( bodyId, bodyEqs ) ->
    State.pure <|
        ( Type.id declId
        , Type.mono <| functionType argIds bodyId
        , "Binding: from its args to its body"
        )
            :: argEqs
            ++ bodyEqs


signatureEquations : Id -> Maybe (Node Signature) -> TIState (List TypeEquation)
signatureEquations declId maybeSigNode =
    maybeSigNode
        |> Maybe.map
            (Node.value
                >> .typeAnnotation
                >> Node.value
                >> Type.fromTypeAnnotation
                >> Result.mapError (State.error << ImpossibleType)
                >> Result.map
                    (\annotationType ->
                        State.pure
                            [ ( Type.id declId
                              , Type.mono annotationType
                              , "Binding must be consistent with its annotation"
                              )
                            ]
                    )
                >> Result.merge
            )
        |> Maybe.withDefault (State.pure [])



-- EXPRESSIONS


inferExpr : Ctx -> Node Expression -> TIState Inferred
inferExpr ctx exprNode =
    State.do (State.idForNode ctx.thisModuleName exprNode) <| \exprId ->
    let
        type_ =
            Type.id exprId

        f : Node Expression -> TIState Inferred
        f =
            inferExpr ctx

        finish : List TypeEquation -> TIState Inferred
        finish eqs =
            State.pure ( exprId, eqs )

        impossibleExpr : TIState Inferred
        impossibleExpr =
            State.error <| ImpossibleExpr exprNode
    in
    case Node.value exprNode of
        UnitExpr ->
            finish [ ( type_, Type.mono Unit, "Unit" ) ]

        Application [] ->
            impossibleExpr

        Application (fnNode :: argNodes) ->
            State.do State.getNextIdAndTick <| \resultId ->
            State.do (f fnNode) <| \( fnId, fnEqs ) ->
            State.do (inferMany f argNodes) <| \( argIds, argEqs ) ->
            finish <|
                ( type_, Type.id resultId, "Application = its result" )
                    :: ( Type.id fnId
                       , Type.mono <| functionType argIds resultId
                       , "Application: first is fn"
                       )
                    :: fnEqs
                    ++ argEqs

        OperatorApplication operator _ e1 e2 ->
            State.do (StateLookup.findModuleOfVar ctx.files ctx.thisFile Nothing operator) <| \moduleName ->
            State.do State.getNextIdAndTick <| \resultId ->
            State.do (f e1) <| \( e1Id, e1Eqs ) ->
            State.do (f e2) <| \( e2Id, e2Eqs ) ->
            State.do (State.addVarType moduleName operator (Type.mono <| functionType [ e1Id, e2Id ] resultId)) <| \() ->
            finish <|
                ( type_, Type.id resultId, "Op application = its result" )
                    :: e1Eqs
                    ++ e2Eqs

        FunctionOrValue moduleName varName ->
            case
                StateLookup.moduleOfVar
                    ctx.files
                    ctx.thisFile
                    (FullModuleName.fromModuleName moduleName)
                    varName
            of
                Ok (Just fullModuleName) ->
                    State.do (State.addVarType fullModuleName varName type_) <| \() ->
                    finish []

                Ok Nothing ->
                    State.do (State.lookupEnv ctx.thisModuleName varName) <| \varType ->
                    {- TODO let poly: do we need to instantiate here? -}
                    finish [ ( type_, Type.mono varType, "FunctionOrValue: var from env" ) ]

                Err err ->
                    State.error err

        IfBlock e1 e2 e3 ->
            State.do (f e1) <| \( id1, eqs1 ) ->
            State.do (f e2) <| \( id2, eqs2 ) ->
            State.do (f e3) <| \( id3, eqs3 ) ->
            finish <|
                ( Type.id id1, Type.mono Bool, "If: condition = bool" )
                    :: ( Type.id id2, Type.id id3, "If: then = else" )
                    :: ( Type.id id2, type_, "If: then = result" )
                    :: eqs1
                    ++ eqs2
                    ++ eqs3

        PrefixOperator operator ->
            -- operator is a function of two arguments
            State.do (StateLookup.findModuleOfVar ctx.files ctx.thisFile Nothing operator) <| \moduleName ->
            State.do (State.addVarType moduleName operator type_) <| \() ->
            State.do State.getNextIdAndTick <| \firstArgId ->
            State.do State.getNextIdAndTick <| \secondArgId ->
            State.do State.getNextIdAndTick <| \resultId ->
            finish
                [ ( type_
                  , Type.mono <| functionType [ firstArgId, secondArgId ] resultId
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
            State.do (f e1) <| \( id1, eqs1 ) ->
            finish <|
                ( type_, Type.id id1, "Negation = inner" )
                    :: ( type_, Type.number numberId, "Negation = number" )
                    :: eqs1

        Literal _ ->
            finish [ ( type_, Type.mono String, "String" ) ]

        CharLiteral _ ->
            finish [ ( type_, Type.mono Char, "Char" ) ]

        TupledExpression exprNodes ->
            State.do (inferMany f exprNodes) <| \( ids, eqs ) ->
            case ids of
                [ id1, id2 ] ->
                    finish <|
                        ( type_
                        , Type.mono <| Tuple (Type.id_ id1) (Type.id_ id2)
                        , "Tuple: top"
                        )
                            :: eqs

                [ id1, id2, id3 ] ->
                    finish <|
                        ( type_
                        , Type.mono <| Tuple3 (Type.id_ id1) (Type.id_ id2) (Type.id_ id3)
                        , "Tuple3: top"
                        )
                            :: eqs

                _ ->
                    impossibleExpr

        ParenthesizedExpression e1 ->
            State.do (f e1) <| \( id1, eqs1 ) ->
            finish <| ( type_, Type.id id1, "Parenthesized = inner" ) :: eqs1

        LetExpression { declarations, expression } ->
            let
                inferLetDecl : Node LetDeclaration -> TIState Inferred
                inferLetDecl declNode =
                    State.do (State.idForNode ctx.thisModuleName declNode) <| \declId ->
                    case Node.value declNode of
                        LetFunction fn ->
                            State.do (aliasImplementation ctx declId fn.declaration) <| \impl ->
                            State.map2 (\sigEqs implEqs -> ( declId, sigEqs ++ implEqs ))
                                (signatureEquations declId fn.signature)
                                (inferImplementation ctx declId impl)

                        LetDestructuring patternNode e1 ->
                            State.do (inferPattern ctx patternNode) <| \( _, patternEqs ) ->
                            State.do (f e1) <| \( _, exprEqs ) ->
                            State.pure ( declId, patternEqs ++ exprEqs )

                {- ...to the env we'll later look the var name up from inside
                   `FunctionOrValue`. Destructurings are already handled by
                   `inferPattern`.
                -}
                addLetDeclBinding : ( Node LetDeclaration, Inferred ) -> TIState ()
                addLetDeclBinding ( declNode, ( declId, _ ) ) =
                    case Node.value declNode of
                        LetFunction fn ->
                            State.addBinding
                                (Node.value (Node.value fn.declaration).name)
                                (Type.id declId)

                        LetDestructuring _ _ ->
                            State.pure ()
            in
            State.do (State.traverse inferLetDecl declarations) <| \declInferreds ->
            State.do (State.traverse addLetDeclBinding (List.map2 Tuple.pair declarations declInferreds)) <| \_ ->
            -- TODO let poly: each use of a generalized binding needs to be instantiated
            State.do (f expression) <| \( bodyId, bodyEqs ) ->
            finish <|
                ( type_, Type.id bodyId, "Let = its body" )
                    :: List.fastConcatMap Tuple.second declInferreds
                    ++ bodyEqs

        CaseExpression { expression, cases } ->
            State.do (f expression) <| \( scrutineeId, scrutineeEqs ) ->
            State.do
                (State.traverse
                    (\( patternNode, bodyNode ) ->
                        State.do (inferPattern ctx patternNode) <| \( patternId, patternEqs ) ->
                        State.do (f bodyNode) <| \( bodyId, bodyEqs ) ->
                        State.pure ( ( patternId, bodyId ), patternEqs ++ bodyEqs )
                    )
                    cases
                )
            <| \caseInferreds ->
            let
                caseIds : List ( Id, Id )
                caseIds =
                    List.map Tuple.first caseInferreds

                caseEqs : List TypeEquation
                caseEqs =
                    List.fastConcatMap Tuple.second caseInferreds

                scrutineeEquations : List TypeEquation
                scrutineeEquations =
                    caseIds
                        |> List.map
                            (\( patternId, _ ) ->
                                ( Type.id scrutineeId
                                , Type.id patternId
                                , "Case: scrutinee = branch pattern"
                                )
                            )

                bodyEquations : List TypeEquation
                bodyEquations =
                    caseIds
                        |> List.map
                            (\( _, bodyId ) ->
                                ( type_
                                , Type.id bodyId
                                , "Case: result = branch body"
                                )
                            )
            in
            finish <|
                scrutineeEquations
                    ++ bodyEquations
                    ++ scrutineeEqs
                    ++ caseEqs

        LambdaExpression { args, expression } ->
            State.do (inferMany (inferPattern ctx) args) <| \( argIds, argEqs ) ->
            State.do (f expression) <| \( bodyId, bodyEqs ) ->
            finish <|
                ( type_
                , Type.mono <| functionType argIds bodyId
                , "Lambda: is a function"
                )
                    :: argEqs
                    ++ bodyEqs

        RecordExpr fieldSetters ->
            State.do (inferRecordSetters ctx fieldSetters) <| \( fields, eqs ) ->
            finish <| ( type_, Type.mono <| Record fields, "Record: is a record" ) :: eqs

        ListExpr exprNodes ->
            State.do (inferMany f exprNodes) <| \( ids, eqs ) ->
            State.do State.getNextIdAndTick <| \listItemId ->
            finish <|
                ( type_, Type.mono <| List <| Type.id_ listItemId, "List: is a list" )
                    :: List.map
                        (\itemId ->
                            ( Type.id itemId
                            , Type.id listItemId
                            , "List: pin list type param to all inner"
                            )
                        )
                        ids
                    ++ eqs

        RecordAccess recordNode fieldNameNode ->
            State.do (f recordNode) <| \( recordNodeId, recordEqs ) ->
            State.do State.getNextIdAndTick <| \extensibleRecordId ->
            State.do State.getNextIdAndTick <| \resultId ->
            finish <|
                [ ( type_, Type.id resultId, "Record access = the field = the result" )
                , ( Type.id recordNodeId
                  , Type.mono <|
                        ExtensibleRecord
                            { type_ = Type.id_ extensibleRecordId
                            , fields =
                                Dict.singleton
                                    (Node.value fieldNameNode)
                                    (Type.id_ resultId)
                            }
                  , "Record access: left is a record"
                  )
                ]
                    ++ recordEqs

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
                recordVar : VarName
                recordVar =
                    Node.value recordVarNode
            in
            State.do State.getNextIdAndTick <| \recordId ->
            State.do (StateLookup.findModuleOfVar ctx.files ctx.thisFile Nothing recordVar) <| \moduleName ->
            State.do (State.addVarType moduleName recordVar (Type.id recordId)) <| \() ->
            State.do (inferRecordSetters ctx fieldSetters) <| \( fields, eqs ) ->
            finish <|
                ( type_
                , Type.mono <|
                    ExtensibleRecord
                        { type_ = Type.id_ recordId
                        , fields = fields
                        }
                , "Record update: is record with at least that field"
                )
                    :: eqs

        GLSLExpression code ->
            let
                declarations :
                    { uniforms : Dict VarName MonoType
                    , attributes : Dict VarName MonoType
                    , varyings : Dict VarName MonoType
                    }
                declarations =
                    glslDeclarations code
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


inferRecordSetters :
    Ctx
    -> List (Node Expression.RecordSetter)
    -> TIState ( Dict VarName MonoType, List TypeEquation )
inferRecordSetters ctx fieldSetters =
    fieldSetters
        |> State.traverse
            (\fieldSetterNode ->
                let
                    ( fieldNameNode, fieldExprNode ) =
                        Node.value fieldSetterNode
                in
                State.do (inferExpr ctx fieldExprNode) <| \( fieldId, eqs ) ->
                State.pure ( ( Node.value fieldNameNode, Type.id_ fieldId ), eqs )
            )
        |> State.map
            (\fieldsAndEqs ->
                ( fieldsAndEqs |> List.map Tuple.first |> Dict.fromList
                , fieldsAndEqs |> List.fastConcatMap Tuple.second
                )
            )



-- PATTERNS


inferPattern : Ctx -> Node Pattern -> TIState Inferred
inferPattern ctx patternNode =
    State.do (State.idForNode ctx.thisModuleName patternNode) <| \patternId ->
    let
        type_ =
            Type.id patternId

        p : Node Pattern -> TIState Inferred
        p =
            inferPattern ctx

        finish : List TypeEquation -> TIState Inferred
        finish eqs =
            State.pure ( patternId, eqs )

        impossiblePattern : TIState Inferred
        impossiblePattern =
            State.error <| ImpossiblePattern patternNode
    in
    case Node.value patternNode of
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

        TuplePattern patterns ->
            State.do (inferMany p patterns) <| \( ids, eqs ) ->
            case ids of
                [ id1, id2 ] ->
                    finish <|
                        ( type_
                        , Type.mono <| Tuple (Type.id_ id1) (Type.id_ id2)
                        , "Tuple pattern: top"
                        )
                            :: eqs

                [ id1, id2, id3 ] ->
                    finish <|
                        ( type_
                        , Type.mono <| Tuple3 (Type.id_ id1) (Type.id_ id2) (Type.id_ id3)
                        , "Tuple3 pattern: top"
                        )
                            :: eqs

                _ ->
                    impossiblePattern

        RecordPattern fields ->
            {- If we're pattern matching some record fields, we're mandating that
               the thing is a record that contains _at least_ these fields.

               Which is what our ExtensibleRecord type does!
            -}
            State.do
                (fields
                    |> State.traverse
                        (\fieldNode ->
                            State.do (State.idForNode ctx.thisModuleName fieldNode) <| \fieldId ->
                            State.do (State.addBinding (Node.value fieldNode) (Type.id fieldId)) <| \() ->
                            State.pure ( Node.value fieldNode, Type.id_ fieldId )
                        )
                )
            <| \fields_ ->
            State.do State.getNextIdAndTick <| \recordId ->
            finish
                [ ( type_
                  , Type.mono <|
                        ExtensibleRecord
                            { type_ = Type.id_ recordId
                            , fields = Dict.fromList fields_
                            }
                  , "Record pattern"
                  )
                ]

        UnConsPattern p1 p2 ->
            State.do State.getNextIdAndTick <| \listItemId ->
            State.do (p p1) <| \( id1, eqs1 ) ->
            State.do (p p2) <| \( id2, eqs2 ) ->
            finish <|
                ( type_, Type.mono <| List <| Type.id_ listItemId, "UnCons pattern: result" )
                    :: ( type_, Type.id id2, "UnCons pattern: result same as tail" )
                    :: ( Type.id id1, Type.id listItemId, "UnCons pattern: head pins list type param" )
                    :: eqs1
                    ++ eqs2

        ListPattern patterns ->
            State.do State.getNextIdAndTick <| \listItemId ->
            State.do (inferMany p patterns) <| \( ids, eqs ) ->
            finish <|
                ( type_, Type.mono <| List <| Type.id_ listItemId, "ListPattern: result" )
                    :: List.map
                        (\itemId ->
                            ( Type.id itemId
                            , Type.id listItemId
                            , "ListPattern: pin list type param to all items"
                            )
                        )
                        ids
                    ++ eqs

        VarPattern var ->
            State.do (State.addBinding var type_) <| \() ->
            finish []

        NamedPattern customType args ->
            State.do
                (StateLookup.findModuleOfVar
                    ctx.files
                    ctx.thisFile
                    (FullModuleName.fromModuleName customType.moduleName)
                    customType.name
                )
            <| \fullModuleName ->
            State.do (inferMany p args) <| \( argIds, eqs ) ->
            finish <|
                ( type_
                , Type.mono <|
                    UserDefinedType
                        { moduleName = fullModuleName
                        , name = customType.name
                        , args = List.map Type.id_ argIds
                        }
                , "NamedPattern: is user defined type"
                )
                    :: eqs

        AsPattern p1 varNameNode ->
            State.do (State.addBinding (Node.value varNameNode) type_) <| \() ->
            State.do (State.aliasNodeId ctx.thisModuleName (Node.range varNameNode) patternId) <| \() ->
            State.do (p p1) <| \( id1, eqs1 ) ->
            finish <| ( type_, Type.id id1, "AsPattern = inner" ) :: eqs1

        ParenthesizedPattern p1 ->
            State.do (p p1) <| \( id1, eqs1 ) ->
            finish <| ( type_, Type.id id1, "Parenthesized pattern = inner" ) :: eqs1



-- VARS


{-| Top-level bindings are deliberately monomorphic for now: we pairwise link
every use of a name.
-}
varEquations : TIState (List TypeEquation)
varEquations =
    State.getVarTypes
        |> State.map
            (\varTypes ->
                varTypes
                    |> Dict.values
                    |> List.fastConcatMap
                        (List.mapConsecutivePairs (\t1 t2 -> ( t1, t2, "vars linked" )))
            )



-- GLSL


{-| TODO This currently only correctly detects "simple" declarations:

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
\* <https://github.com/noteed/language-glsl/blob/master/Language/GLSL/Parser.hs>
\* what elm/compiler uses under the hood
\* <https://github.com/shuhei/elm-compiler/blob/glsl-parser/compiler/src/Parse/Shader.hs>
\* this one might be doing the least work
\* <https://github.com/w0rm/elm-glsl/blob/main/Language/GLSL/NewParser.hs>
\* written using elm-parser-like primitives

-}
glslDeclarations :
    String
    ->
        { uniforms : Dict VarName MonoType
        , attributes : Dict VarName MonoType
        , varyings : Dict VarName MonoType
        }
glslDeclarations code =
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
