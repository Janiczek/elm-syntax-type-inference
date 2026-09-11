module Elm.TypeInference.Infer exposing
    ( Ctx
    , inferExpr
    , inferPattern
    , letFunctionMember
    , topLevelMember
    )

{-| A traversal over elm-syntax AST

  - Every node gets a fresh type ID.
  - Each walk function returns that ID along with the type equations it generated.
  - Parents can refer to children's types directly by inspecting the return value of the recursive call.

Top-level and `let`-bound functions need to be gathered into binding groups and
solved together for mutual recursion and let-polymorphism.
See `topLevelMember` and `letFunctionMember`.

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Expression.Extra exposing (functionName, referencedNames)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Pattern.Extra
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.BindingGroup as BindingGroup
import Elm.TypeInference.Dependencies exposing (Dependencies)
import Elm.TypeInference.Error as Error exposing (Error(..))
import Elm.TypeInference.ModuleLookup as ModuleLookup
import Elm.TypeInference.SCC as SCC
import Elm.TypeInference.State as State exposing (PackageName, TIState)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type as Type
    exposing
        ( Id
        , MonoType(..)
        , Type
        , TypeResolver
        )
import Elm.TypeInference.Type.External as ExternalType
import Elm.TypeInference.TypeEquation exposing (TypeEquation)
import Elm.TypeInference.Unify as Unify exposing (TypeAlias)
import List.ExtraExtra
import Regex exposing (Regex)
import Result.Extra
import Set exposing (Set)


type alias Ctx =
    { files : Dict FullModuleName File
    , thisFile : File
    , thisModuleName : FullModuleName
    , typeAliases : Dict ( PackageName, FullModuleName, VarName ) TypeAlias
    , dependencies : Dependencies
    }


typeResolver : Ctx -> TypeResolver
typeResolver ctx =
    ModuleLookup.typeResolverFor ctx.dependencies ctx.files ctx.thisFile


type alias Inferred =
    ( Id, List TypeEquation )


inferMany : (a -> TIState Inferred) -> List a -> TIState ( List Id, List TypeEquation )
inferMany f items =
    State.traverse f items
        |> State.map
            (\inferreds ->
                ( List.map Tuple.first inferreds
                , List.ExtraExtra.fastConcatMap Tuple.second inferreds
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


{-| Resolves a value or operator symbol to its type.
-}
lookupVarOrOperator : Ctx -> Maybe FullModuleName -> VarName -> TIState MonoType
lookupVarOrOperator ctx maybeModuleName name =
    State.do (ModuleLookup.findModuleOfVar ctx.dependencies ctx.files ctx.thisFile maybeModuleName name) <| \( package, moduleName ) ->
    let
        ( aliasedPackage, aliasedModuleName, aliasedName ) =
            if package == "" then
                ModuleLookup.resolveOperatorFunction ctx.files moduleName name
                    |> Result.withDefault Nothing
                    |> Maybe.map (\( m, n ) -> ( "", m, n ))
                    |> Maybe.withDefault ( package, moduleName, name )

            else
                ( package, moduleName, name )
    in
    State.lookupGlobalEnv aliasedPackage aliasedModuleName aliasedName



-- BINDING GROUP MEMBERS


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


inferFnImplementation : Ctx -> Id -> Expression.FunctionImplementation -> TIState (List TypeEquation)
inferFnImplementation ctx declId impl =
    State.withScopedEnv <|
        (State.do (inferMany (inferPattern ctx) impl.arguments) <| \( argIds, argEqs ) ->
        State.do (inferExpr ctx impl.expression) <| \( bodyId, bodyEqs ) ->
        State.pure <|
            ( Type.id_ declId
            , functionType argIds bodyId
            , "Binding: from its args to its body"
            )
                :: argEqs
                ++ bodyEqs
        )


annotationScheme : Ctx -> Maybe (Node Signature) -> TIState (Maybe Type)
annotationScheme ctx maybeSigNode =
    case maybeSigNode of
        Nothing ->
            State.pure Nothing

        Just sigNode ->
            Node.value sigNode
                |> .typeAnnotation
                |> Node.value
                |> Type.fromTypeAnnotation (typeResolver ctx)
                |> Result.mapError (State.error << Error.fromTypeAnnotationError)
                |> Result.map (Type.closeOver >> Just >> State.pure)
                |> Result.Extra.merge


{-| `declId ≡ annotationType`, if the function is annotated.
-}
signatureEquations : Ctx -> Id -> Maybe (Node Signature) -> TIState (List TypeEquation)
signatureEquations ctx declId maybeSigNode =
    maybeSigNode
        |> Maybe.map
            (Node.value
                >> .typeAnnotation
                >> Node.value
                >> Type.fromTypeAnnotation (typeResolver ctx)
                >> Result.mapError (State.error << Error.fromTypeAnnotationError)
                >> Result.map
                    (\annotationType ->
                        State.pure
                            [ ( Type.id_ declId
                              , annotationType
                              , "Binding must be consistent with its annotation"
                              )
                            ]
                    )
                >> Result.Extra.merge
            )
        |> Maybe.withDefault (State.pure [])


{-| Top-level function declaration. Adds a binding to `globalEnv`.
-}
topLevelMember : Ctx -> Node Declaration -> Expression.Function -> TIState BindingGroup.Member
topLevelMember ctx declNode fn =
    State.do (State.idForNode ctx.thisModuleName declNode) <| \declId ->
    State.do (aliasImplementation ctx declId fn.declaration) <| \impl ->
    State.do (annotationScheme ctx fn.signature) <| \maybeAnnotation ->
    let
        varName : VarName
        varName =
            Node.value impl.name
    in
    State.pure
        { id = declId
        , maybeAnnotation = maybeAnnotation
        , install = State.addGlobalBinding ( "", ctx.thisModuleName, varName )
        , equations =
            State.map2 (++)
                (signatureEquations ctx declId fn.signature)
                (inferFnImplementation ctx declId impl)
        }


{-| A `let..in` function declaration. Adds a binding to lexical `lexicalEnv`
-}
letFunctionMember : Ctx -> Node LetDeclaration -> Expression.Function -> TIState BindingGroup.Member
letFunctionMember ctx declNode fn =
    State.do (State.idForNode ctx.thisModuleName declNode) <| \declId ->
    State.do (aliasImplementation ctx declId fn.declaration) <| \impl ->
    State.do (annotationScheme ctx fn.signature) <| \maybeAnnotation ->
    let
        varName : VarName
        varName =
            Node.value impl.name
    in
    State.pure
        { id = declId
        , maybeAnnotation = maybeAnnotation
        , install = State.addBinding varName
        , equations =
            State.map2 (++)
                (signatureEquations ctx declId fn.signature)
                (inferFnImplementation ctx declId impl)
        }



-- EXPRESSIONS


inferExpr : Ctx -> Node Expression -> TIState Inferred
inferExpr ctx exprNode =
    State.do (State.idForNode ctx.thisModuleName exprNode) <| \exprId ->
    let
        type_ =
            Type.id_ exprId

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
            finish [ ( type_, Unit, "Unit" ) ]

        Application [] ->
            impossibleExpr

        Application (fnNode :: argNodes) ->
            State.do State.getNextIdAndTick <| \resultId ->
            State.do (f fnNode) <| \( fnId, fnEqs ) ->
            State.do (inferMany f argNodes) <| \( argIds, argEqs ) ->
            finish <|
                ( type_, Type.id_ resultId, "Application = its result" )
                    :: ( Type.id_ fnId
                       , functionType argIds resultId
                       , "Application: first is fn"
                       )
                    :: fnEqs
                    ++ argEqs

        OperatorApplication operator _ e1 e2 ->
            State.do State.getNextIdAndTick <| \resultId ->
            State.do (f e1) <| \( e1Id, e1Eqs ) ->
            State.do (f e2) <| \( e2Id, e2Eqs ) ->
            State.do (lookupVarOrOperator ctx Nothing operator) <| \operatorType ->
            finish <|
                ( type_, Type.id_ resultId, "Op application = its result" )
                    :: ( operatorType, functionType [ e1Id, e2Id ] resultId, "Op application: is a fn" )
                    :: e1Eqs
                    ++ e2Eqs

        FunctionOrValue moduleName varName ->
            -- Lexically bound name wins over imported one
            State.do
                (if List.isEmpty moduleName then
                    State.existsInEnv varName

                 else
                    State.pure False
                )
            <| \isLexical ->
            if isLexical then
                State.do (State.lookupEnv ctx.thisModuleName varName) <| \varType ->
                finish [ ( type_, varType, "FunctionOrValue: var from env" ) ]

            else
                case
                    ModuleLookup.moduleOfVar
                        ctx.dependencies
                        ctx.files
                        ctx.thisFile
                        (FullModuleName.fromModuleName moduleName)
                        varName
                of
                    Ok (Just ( package, fullModuleName )) ->
                        State.do (State.lookupGlobalEnv package fullModuleName varName) <| \varType ->
                        finish [ ( type_, varType, "FunctionOrValue: global/top-level var" ) ]

                    Ok Nothing ->
                        State.do (State.lookupEnv ctx.thisModuleName varName) <| \varType ->
                        finish [ ( type_, varType, "FunctionOrValue: var from env" ) ]

                    Err err ->
                        State.error err

        IfBlock e1 e2 e3 ->
            State.do (f e1) <| \( id1, eqs1 ) ->
            State.do (f e2) <| \( id2, eqs2 ) ->
            State.do (f e3) <| \( id3, eqs3 ) ->
            finish <|
                ( Type.id_ id1, Bool, "If: condition = bool" )
                    :: ( Type.id_ id2, Type.id_ id3, "If: then = else" )
                    :: ( Type.id_ id2, type_, "If: then = result" )
                    :: eqs1
                    ++ eqs2
                    ++ eqs3

        PrefixOperator operator ->
            State.do (lookupVarOrOperator ctx Nothing operator) <| \operatorType ->
            finish [ ( type_, operatorType, "Prefix operator: is a fn" ) ]

        Operator _ ->
            impossibleExpr

        Integer _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number_ numberId, "Int" ) ]

        Hex _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number_ numberId, "Hex" ) ]

        Floatable _ ->
            finish [ ( type_, Float, "Float" ) ]

        Negation e1 ->
            State.do State.getNextIdAndTick <| \numberId ->
            State.do (f e1) <| \( id1, eqs1 ) ->
            finish <|
                ( type_, Type.id_ id1, "Negation = inner" )
                    :: ( type_, Type.number_ numberId, "Negation = number" )
                    :: eqs1

        Literal _ ->
            finish [ ( type_, String, "String" ) ]

        CharLiteral _ ->
            finish [ ( type_, Char, "Char" ) ]

        TupledExpression exprNodes ->
            State.do (inferMany f exprNodes) <| \( ids, eqs ) ->
            case ids of
                [ id1, id2 ] ->
                    finish <|
                        ( type_
                        , Tuple (Type.id_ id1) (Type.id_ id2)
                        , "Tuple: top"
                        )
                            :: eqs

                [ id1, id2, id3 ] ->
                    finish <|
                        ( type_
                        , Tuple3 (Type.id_ id1) (Type.id_ id2) (Type.id_ id3)
                        , "Tuple3: top"
                        )
                            :: eqs

                _ ->
                    impossibleExpr

        ParenthesizedExpression e1 ->
            State.do (f e1) <| \( id1, eqs1 ) ->
            finish <| ( type_, Type.id_ id1, "Parenthesized = inner" ) :: eqs1

        LetExpression { declarations, expression } ->
            State.withScopedEnv <|
                (State.do (solveLetDeclarations ctx declarations) <| \() ->
                State.do (f expression) <| \( bodyId, bodyEqs ) ->
                finish <|
                    ( type_, Type.id_ bodyId, "Let = its body" )
                        :: bodyEqs
                )

        CaseExpression { expression, cases } ->
            State.do (f expression) <| \( scrutineeId, scrutineeEqs ) ->
            State.do
                (State.traverse
                    (\( patternNode, bodyNode ) ->
                        State.withScopedEnv <|
                            (State.do (inferPattern ctx patternNode) <| \( patternId, patternEqs ) ->
                            State.do (f bodyNode) <| \( bodyId, bodyEqs ) ->
                            State.pure ( ( patternId, bodyId ), patternEqs ++ bodyEqs )
                            )
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
                    List.ExtraExtra.fastConcatMap Tuple.second caseInferreds

                scrutineeEquations : List TypeEquation
                scrutineeEquations =
                    caseIds
                        |> List.map
                            (\( patternId, _ ) ->
                                ( Type.id_ scrutineeId
                                , Type.id_ patternId
                                , "Case: scrutinee = branch pattern"
                                )
                            )

                bodyEquations : List TypeEquation
                bodyEquations =
                    caseIds
                        |> List.map
                            (\( _, bodyId ) ->
                                ( type_
                                , Type.id_ bodyId
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
            State.withScopedEnv <|
                (State.do (inferMany (inferPattern ctx) args) <| \( argIds, argEqs ) ->
                State.do (f expression) <| \( bodyId, bodyEqs ) ->
                finish <|
                    ( type_
                    , functionType argIds bodyId
                    , "Lambda: is a function"
                    )
                        :: argEqs
                        ++ bodyEqs
                )

        RecordExpr fieldSetters ->
            State.do (inferRecordSetters ctx fieldSetters) <| \( fields, eqs ) ->
            finish <| ( type_, Record fields, "Record: is a record" ) :: eqs

        ListExpr exprNodes ->
            State.do (inferMany f exprNodes) <| \( ids, eqs ) ->
            State.do State.getNextIdAndTick <| \listItemId ->
            finish <|
                ( type_, List <| Type.id_ listItemId, "List: is a list" )
                    :: List.map
                        (\itemId ->
                            ( Type.id_ itemId
                            , Type.id_ listItemId
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
                [ ( type_, Type.id_ resultId, "Record access = the field = the result" )
                , ( Type.id_ recordNodeId
                  , ExtensibleRecord
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
                  , Function
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
            State.do (State.existsInEnv recordVar) <| \isLexical ->
            State.do
                (if isLexical then
                    State.lookupEnv ctx.thisModuleName recordVar

                 else
                    lookupVarOrOperator ctx Nothing recordVar
                )
            <| \recordVarType ->
            State.do (inferRecordSetters ctx fieldSetters) <| \( fields, eqs ) ->
            let
                asExtensibleRecord : MonoType
                asExtensibleRecord =
                    ExtensibleRecord
                        { type_ = Type.id_ recordId
                        , fields = fields
                        }
            in
            finish <|
                ( recordVarType, asExtensibleRecord, "Record update: base record has at least that field" )
                    :: ( type_, asExtensibleRecord, "Record update: result has the same shape as the base record" )
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
                  , WebGLShader
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
                , fieldsAndEqs |> List.ExtraExtra.fastConcatMap Tuple.second
                )
            )



-- LET DECLARATIONS


{-| Solve decls in `let..in` in dependency order.
`let` functions and `let` destructurings are available at the same time.
-}
solveLetDeclarations : Ctx -> List (Node LetDeclaration) -> TIState ()
solveLetDeclarations ctx declarations =
    let
        indexed : List ( Int, Node LetDeclaration )
        indexed =
            List.indexedMap Tuple.pair declarations

        byIndex : Dict Int (Node LetDeclaration)
        byIndex =
            Dict.fromList indexed

        boundNames : Node LetDeclaration -> List VarName
        boundNames declNode =
            case Node.value declNode of
                LetFunction fn ->
                    [ functionName fn ]

                LetDestructuring patternNode _ ->
                    Elm.Syntax.Pattern.Extra.varNames (Node.value patternNode)

        -- name -> index of the declaration binding it
        indexOfName : Dict VarName Int
        indexOfName =
            indexed
                |> List.concatMap
                    (\( index, declNode ) ->
                        boundNames declNode |> List.map (\name -> ( name, index ))
                    )
                |> Dict.fromList

        bodyOf : Node LetDeclaration -> Expression
        bodyOf declNode =
            case Node.value declNode of
                LetFunction fn ->
                    Node.value (Node.value fn.declaration).expression

                LetDestructuring _ exprNode ->
                    Node.value exprNode

        edges : Int -> List Int
        edges index =
            case Dict.get index byIndex of
                Nothing ->
                    []

                Just declNode ->
                    referencedNames (bodyOf declNode)
                        |> List.filterMap
                            (\( maybeModuleName, refName ) ->
                                if maybeModuleName == Nothing then
                                    Dict.get refName indexOfName

                                else
                                    Nothing
                            )

        sccs : List (List Int)
        sccs =
            SCC.stronglyConnectedComponents (List.map Tuple.first indexed) edges

        inferDestructuring : Node LetDeclaration -> Node Pattern -> Node Expression -> TIState ()
        inferDestructuring declNode patternNode exprNode =
            State.do (State.idForNode ctx.thisModuleName declNode) <| \declId ->
            State.do (inferPattern ctx patternNode) <| \( patternId, patternEqs ) ->
            State.do (inferExpr ctx exprNode) <| \( exprId, exprEqs ) ->
            let
                eqs : List TypeEquation
                eqs =
                    ( Type.id_ declId, Type.id_ patternId, "Let destructuring: alias" )
                        :: ( Type.id_ patternId, Type.id_ exprId, "Let destructuring: pattern = expr" )
                        :: patternEqs
                        ++ exprEqs
            in
            State.do State.getSubst <| \accumulatedSubst ->
            let
                preSubstitutedEqs =
                    eqs
                        |> List.map (\( t1, t2, _ ) -> ( t1, t2 ))
                        |> List.map
                            (Tuple.mapBoth
                                (SubstitutionMap.substituteMono accumulatedSubst)
                                (SubstitutionMap.substituteMono accumulatedSubst)
                            )
            in
            State.do (Unify.unifyMany ctx.typeAliases preSubstitutedEqs) <| \groupSubst ->
            State.composeSubst groupSubst

        solveGroup : List Int -> TIState ()
        solveGroup groupIndices =
            let
                groupDecls : List (Node LetDeclaration)
                groupDecls =
                    groupIndices |> List.filterMap (\index -> Dict.get index byIndex)

                functions : List ( Node LetDeclaration, Expression.Function )
                functions =
                    groupDecls
                        |> List.filterMap
                            (\declNode ->
                                case Node.value declNode of
                                    LetFunction fn ->
                                        Just ( declNode, fn )

                                    LetDestructuring _ _ ->
                                        Nothing
                            )

                destructurings : List ( Node LetDeclaration, Node Pattern, Node Expression )
                destructurings =
                    groupDecls
                        |> List.filterMap
                            (\declNode ->
                                case Node.value declNode of
                                    LetDestructuring patternNode exprNode ->
                                        Just ( declNode, patternNode, exprNode )

                                    LetFunction _ ->
                                        Nothing
                            )
            in
            State.do
                (functions
                    |> State.traverse (\( declNode, fn ) -> letFunctionMember ctx declNode fn)
                    |> State.andThen (BindingGroup.solveGroup ctx.typeAliases)
                )
            <| \() ->
            destructurings
                |> State.traverse
                    (\( declNode, patternNode, exprNode ) ->
                        inferDestructuring declNode patternNode exprNode
                    )
                |> State.map (always ())
    in
    sccs
        |> State.traverse solveGroup
        |> State.map (always ())



-- PATTERNS


inferPattern : Ctx -> Node Pattern -> TIState Inferred
inferPattern ctx patternNode =
    State.do (State.idForNode ctx.thisModuleName patternNode) <| \patternId ->
    let
        type_ =
            Type.id_ patternId

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
            finish [ ( type_, Unit, "Unit pattern" ) ]

        CharPattern _ ->
            finish [ ( type_, Char, "Char pattern" ) ]

        StringPattern _ ->
            finish [ ( type_, String, "String pattern" ) ]

        IntPattern _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number_ numberId, "Int pattern" ) ]

        HexPattern _ ->
            State.do State.getNextIdAndTick <| \numberId ->
            finish [ ( type_, Type.number_ numberId, "Hex pattern" ) ]

        FloatPattern _ ->
            finish [ ( type_, Float, "Float pattern" ) ]

        TuplePattern patterns ->
            State.do (inferMany p patterns) <| \( ids, eqs ) ->
            case ids of
                [ id1, id2 ] ->
                    finish <|
                        ( type_
                        , Tuple (Type.id_ id1) (Type.id_ id2)
                        , "Tuple pattern: top"
                        )
                            :: eqs

                [ id1, id2, id3 ] ->
                    finish <|
                        ( type_
                        , Tuple3 (Type.id_ id1) (Type.id_ id2) (Type.id_ id3)
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
                            State.do (State.addBinding (Node.value fieldNode) (Type.mono <| Type.id_ fieldId)) <| \() ->
                            State.pure ( Node.value fieldNode, Type.id_ fieldId )
                        )
                )
            <| \fields_ ->
            State.do State.getNextIdAndTick <| \recordId ->
            finish
                [ ( type_
                  , ExtensibleRecord
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
                ( type_, List <| Type.id_ listItemId, "UnCons pattern: result" )
                    :: ( type_, Type.id_ id2, "UnCons pattern: result same as tail" )
                    :: ( Type.id_ id1, Type.id_ listItemId, "UnCons pattern: head pins list type param" )
                    :: eqs1
                    ++ eqs2

        ListPattern patterns ->
            State.do State.getNextIdAndTick <| \listItemId ->
            State.do (inferMany p patterns) <| \( ids, eqs ) ->
            finish <|
                ( type_, List <| Type.id_ listItemId, "ListPattern: result" )
                    :: List.map
                        (\itemId ->
                            ( Type.id_ itemId
                            , Type.id_ listItemId
                            , "ListPattern: pin list type param to all items"
                            )
                        )
                        ids
                    ++ eqs

        VarPattern var ->
            State.do (State.addBinding var (Type.mono type_)) <| \() ->
            finish []

        NamedPattern customType args ->
            State.do
                (ModuleLookup.findModuleOfVar
                    ctx.dependencies
                    ctx.files
                    ctx.thisFile
                    (FullModuleName.fromModuleName customType.moduleName)
                    customType.name
                )
            <| \( package, fullModuleName ) ->
            State.do (State.lookupGlobalEnv package fullModuleName customType.name) <| \ctorType ->
            State.do State.getNextIdAndTick <| \resultId ->
            State.do (inferMany p args) <| \( argIds, eqs ) ->
            finish <|
                ( ctorType, functionType argIds resultId, "NamedPattern: constructor is a fn" )
                    :: ( type_, Type.id_ resultId, "NamedPattern: result" )
                    :: eqs

        AsPattern p1 varNameNode ->
            State.do (State.addBinding (Node.value varNameNode) (Type.mono type_)) <| \() ->
            State.do (State.aliasNodeId ctx.thisModuleName (Node.range varNameNode) patternId) <| \() ->
            State.do (p p1) <| \( id1, eqs1 ) ->
            finish <| ( type_, Type.id_ id1, "AsPattern = inner" ) :: eqs1

        ParenthesizedPattern p1 ->
            State.do (p p1) <| \( id1, eqs1 ) ->
            finish <| ( type_, Type.id_ id1, "Parenthesized pattern = inner" ) :: eqs1



-- GLSL


{-| Extracts the `uniform` / `attribute` / `varying` declarations of a shader.

Approximation, not a full GLSL parser.

       uniform /* hello */ mat4 u_x, u_y, u_z;
       attribute
         vec4 a_position
            ;
       uniform lowp float u_alpha;

TODO preprocessor directives (`#ifdef`, `#define`d types)
TODO `struct` declarations

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
        |> Regex.replace glslCommentRegex (\_ -> " ")
        |> String.split ";"
        |> List.foldl
            (\chunk acc -> List.foldl insertGlslDeclaration acc (glslDeclaration chunk))
            { uniforms = Dict.empty
            , attributes = Dict.empty
            , varyings = Dict.empty
            }


insertGlslDeclaration :
    ( String, VarName, MonoType )
    ->
        { uniforms : Dict VarName MonoType
        , attributes : Dict VarName MonoType
        , varyings : Dict VarName MonoType
        }
    ->
        { uniforms : Dict VarName MonoType
        , attributes : Dict VarName MonoType
        , varyings : Dict VarName MonoType
        }
insertGlslDeclaration ( storageQualifier, varName, varType ) acc =
    case storageQualifier of
        "attribute" ->
            { acc | attributes = Dict.insert varName varType acc.attributes }

        "varying" ->
            { acc | varyings = Dict.insert varName varType acc.varyings }

        "uniform" ->
            { acc | uniforms = Dict.insert varName varType acc.uniforms }

        _ ->
            acc


{-|

     "uniform mediump mat4 u_x, u_y"
     -->
     [ ( "uniform", "u_x", ExternalType.mat4 )
     , ( "uniform", "u_y", ExternalType.mat4 )
     ]

-}
glslDeclaration : String -> List ( String, VarName, MonoType )
glslDeclaration chunk =
    case Regex.findAtMost 1 glslDeclarationRegex chunk of
        [ { submatches } ] ->
            case submatches of
                [ Just storageQualifier, _, Just varType, Just declarators ] ->
                    case parseGlslVarType varType of
                        Nothing ->
                            []

                        Just varType_ ->
                            declarators
                                |> String.split ","
                                |> List.filterMap
                                    (glslDeclaratorName
                                        >> Maybe.map (\varName -> ( storageQualifier, varName, varType_ ))
                                    )

                _ ->
                    []

        _ ->
            []


glslDeclaratorName : String -> Maybe VarName
glslDeclaratorName declarator =
    let
        name : String
        name =
            declarator
                |> String.split "="
                |> List.head
                |> Maybe.withDefault ""
                |> String.trim
    in
    if Regex.contains glslVarNameRegex name then
        Just name

    else
        Nothing


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

        "sampler2D" ->
            Just ExternalType.texture

        "int" ->
            Just Int

        "float" ->
            Just Float

        _ ->
            Nothing


glslDeclarationRegex : Regex
glslDeclarationRegex =
    Regex.fromString "^\\s*(uniform|attribute|varying)\\s+(highp\\s+|mediump\\s+|lowp\\s+)?([A-Za-z_][A-Za-z0-9_]*)\\s+([\\s\\S]+)"
        |> Maybe.withDefault Regex.never


glslVarNameRegex : Regex
glslVarNameRegex =
    Regex.fromString "^[A-Za-z_][A-Za-z0-9_]*$"
        |> Maybe.withDefault Regex.never


glslCommentRegex : Regex
glslCommentRegex =
    Regex.fromString "//[^\\n]*|/\\*[\\s\\S]*?\\*/"
        |> Maybe.withDefault Regex.never
