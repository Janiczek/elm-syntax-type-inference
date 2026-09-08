module Elm.TypeInference exposing (infer)

{-| TODO write docs

TODO check declarations against their type annotations

@docs infer

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Expression.Extra as ExpressionExtra
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as SyntaxType
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.BindingGroup as BindingGroup
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.Infer as Infer
import Elm.TypeInference.SCC as SCC
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.State.VarModuleLookup as VarModuleLookup
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Elm.TypeInference.Type as Type exposing (Id, MonoType(..))
import Elm.TypeInference.Unify exposing (TypeAlias)
import List.ExtraExtra as List
import Maybe.Extra as Maybe
import RangeLike exposing (RangeLike)
import Result.Extra as Result
import Set exposing (Set)
import TypeLookupTable exposing (TypeLookupTable)



{- TODO Look at converting between our Type
   and https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAnnotation
   and https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Type
-}


{-| TODO docs
-}
infer : Dict ModuleName File -> Result Error (Dict ModuleName TypeLookupTable)
infer files =
    files
        |> parseModuleNameKeys
        |> State.fromMaybe MissingModuleName
        |> State.andThen
            (\files_ ->
                files_
                    |> gatherTypeAliases
                    |> State.andThen (infer_ files_)
            )
        |> State.run (State.init Dict.empty)
        |> Tuple.first


parseModuleNameKeys : Dict ModuleName a -> Maybe (Dict FullModuleName a)
parseModuleNameKeys dict =
    dict
        |> Dict.toList
        |> Maybe.combineMap
            (\( moduleName, value ) ->
                FullModuleName.fromModuleName moduleName
                    |> Maybe.map (\fullModuleName -> ( fullModuleName, value ))
            )
        |> Maybe.map Dict.fromList


infer_ :
    Dict FullModuleName File
    -> Dict ( FullModuleName, VarName ) TypeAlias
    -> TIState (Dict ModuleName TypeLookupTable)
infer_ files typeAliases =
    State.do (registerConstructorsAndPorts files) <| \() ->
    let
        topLevelFunctions : List ( ( FullModuleName, VarName ), ( File, Node Declaration, Expression.Function ) )
        topLevelFunctions =
            files
                |> Dict.toList
                |> List.fastConcatMap
                    (\( moduleName, file ) ->
                        file.declarations
                            |> List.filterMap
                                (\declNode ->
                                    case Node.value declNode of
                                        Declaration.FunctionDeclaration fn ->
                                            Just ( ( moduleName, ExpressionExtra.functionName fn ), ( file, declNode, fn ) )

                                        _ ->
                                            Nothing
                                )
                    )

        nodeSet : Set ( FullModuleName, VarName )
        nodeSet =
            topLevelFunctions
                |> List.map Tuple.first
                |> Set.fromList

        byKey : Dict ( FullModuleName, VarName ) ( File, Node Declaration, Expression.Function )
        byKey =
            Dict.fromList topLevelFunctions

        edges : ( FullModuleName, VarName ) -> List ( FullModuleName, VarName )
        edges key =
            case Dict.get key byKey of
                Nothing ->
                    []

                Just ( file, _, fn ) ->
                    ExpressionExtra.referencedNames (Node.value (Node.value fn.declaration).expression)
                        -- Resolve operator aliases to the underlying functions
                        |> List.filterMap
                            (\( maybeModuleName, varName ) ->
                                case VarModuleLookup.moduleOfVar files file (Maybe.andThen FullModuleName.fromModuleName maybeModuleName) varName of
                                    Ok (Just fullModuleName) ->
                                        let
                                            resolvedKey : ( FullModuleName, VarName )
                                            resolvedKey =
                                                VarModuleLookup.resolveOperatorFunction files fullModuleName varName
                                                    |> Result.withDefault Nothing
                                                    |> Maybe.withDefault ( fullModuleName, varName )
                                        in
                                        if Set.member resolvedKey nodeSet then
                                            Just resolvedKey

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )

        sccs : List (List ( FullModuleName, VarName ))
        sccs =
            SCC.stronglyConnectedComponents (Set.toList nodeSet) edges
    in
    State.do
        (State.traverse
            (\group ->
                group
                    |> List.filterMap (\key -> Dict.get key byKey |> Maybe.map (Tuple.pair key))
                    |> State.traverse
                        (\( ( moduleName, _ ), ( file, declNode, fn ) ) ->
                            Infer.topLevelMember
                                { files = files
                                , thisFile = file
                                , thisModuleName = moduleName
                                , typeAliases = typeAliases
                                }
                                declNode
                                fn
                        )
                    |> State.andThen (BindingGroup.solveGroup typeAliases)
            )
            sccs
        )
    <| \_ ->
    State.do State.getNodeIds <| \nodeIds ->
    State.do State.getSubst <| \substitutionMap ->
    files
        |> Dict.keys
        |> List.map (toTypeLookupTable substitutionMap nodeIds)
        |> Dict.fromList
        |> State.pure


toTypeLookupTable :
    SubstitutionMap
    -> Dict FullModuleName (Dict RangeLike Id)
    -> FullModuleName
    -> ( ModuleName, TypeLookupTable )
toTypeLookupTable substitutionMap nodeIds fullModuleName =
    let
        moduleName : ModuleName
        moduleName =
            FullModuleName.toModuleName fullModuleName
    in
    ( moduleName
    , nodeIds
        |> Dict.get fullModuleName
        |> Maybe.withDefault Dict.empty
        |> Dict.map (\_ id -> Type.mono (SubstitutionMap.substituteMono substitutionMap (Type.id_ id)))
        |> TypeLookupTable.fromDict moduleName
    )


gatherTypeAliases :
    Dict FullModuleName File
    -> TIState (Dict ( FullModuleName, VarName ) TypeAlias)
gatherTypeAliases files =
    files
        |> Dict.toList
        |> List.map
            (\( moduleName, file ) ->
                file.declarations
                    |> List.map
                        (\declarationNode ->
                            case Node.value declarationNode of
                                Declaration.AliasDeclaration typeAlias ->
                                    let
                                        type_ : TIState MonoType
                                        type_ =
                                            typeAlias.typeAnnotation
                                                |> Node.value
                                                |> Type.fromTypeAnnotation
                                                |> Result.mapError (State.error << ImpossibleType)
                                                |> Result.map State.pure
                                                |> Result.merge
                                    in
                                    type_
                                        |> State.map
                                            (\type__ ->
                                                Just
                                                    ( ( moduleName, Node.value typeAlias.name )
                                                    , { args = List.map Node.value typeAlias.generics
                                                      , type_ = type__
                                                      }
                                                    )
                                            )

                                _ ->
                                    State.pure Nothing
                        )
                    |> State.combine
                    |> State.map Maybe.values
            )
        |> State.combine
        |> State.map (List.fastConcat >> Dict.fromList)


registerConstructorsAndPorts : Dict FullModuleName File -> TIState ()
registerConstructorsAndPorts files =
    files
        |> Dict.toList
        |> State.traverse
            (\( moduleName, file ) ->
                file.declarations
                    |> State.traverse
                        (\declNode ->
                            case Node.value declNode of
                                Declaration.CustomTypeDeclaration customType ->
                                    registerCustomType moduleName customType

                                Declaration.PortDeclaration sig ->
                                    registerPort moduleName sig

                                _ ->
                                    State.pure ()
                        )
                    |> State.map (always ())
            )
        |> State.map (always ())


registerCustomType : FullModuleName -> SyntaxType.Type -> TIState ()
registerCustomType moduleName customType =
    let
        typeName : String
        typeName =
            Node.value customType.name

        resultType : MonoType
        resultType =
            UserDefinedType
                { moduleName = moduleName
                , name = typeName
                , args =
                    customType.generics
                        |> List.map (\g -> TypeVar ( Type.Named (Node.value g), Type.Normal ))
                }
    in
    customType.constructors
        |> State.traverse
            (\ctorNode ->
                let
                    ctor =
                        Node.value ctorNode

                    ctorName : String
                    ctorName =
                        Node.value ctor.name

                    argTypes : Result TypeAnnotation (List MonoType)
                    argTypes =
                        ctor.arguments
                            |> List.map (Node.value >> Type.fromTypeAnnotation)
                            |> Result.combine
                in
                argTypes
                    |> Result.mapError (State.error << ImpossibleType)
                    |> Result.map
                        (\args ->
                            let
                                ctorType : MonoType
                                ctorType =
                                    List.foldr (\argT acc -> Function { from = argT, to = acc }) resultType args
                            in
                            State.addGlobalBinding ( "", moduleName, ctorName ) (Type.closeOver ctorType)
                        )
                    |> Result.merge
            )
        |> State.map (always ())


registerPort : FullModuleName -> Signature -> TIState ()
registerPort moduleName sig =
    sig.typeAnnotation
        |> Node.value
        |> Type.fromTypeAnnotation
        |> Result.mapError (State.error << ImpossibleType)
        |> Result.map
            (\t ->
                State.addGlobalBinding
                    ( "", moduleName, Node.value sig.name )
                    (Type.closeOver t)
            )
        |> Result.merge
