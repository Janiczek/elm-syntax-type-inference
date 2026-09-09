module Elm.TypeInference exposing (infer)

{-| TODO write docs

TODO check declarations against their type annotations

@docs infer

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Expression.Extra
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as SyntaxType
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.BindingGroup as BindingGroup
import Elm.TypeInference.Dependencies as Dependencies exposing (Dependencies, DependencyPackage)
import Elm.TypeInference.Error as Error exposing (Error(..))
import Elm.TypeInference.Infer as Infer
import Elm.TypeInference.SCC as SCC
import Elm.TypeInference.State as State exposing (PackageName, TIState)
import Elm.TypeInference.State.VarModuleLookup as VarModuleLookup
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Elm.TypeInference.Type as Type exposing (Id, MonoType(..), TypeResolver)
import Elm.TypeInference.Unify exposing (TypeAlias)
import List.ExtraExtra
import Maybe.Extra
import RangeLike exposing (RangeLike)
import Result.Extra
import Set exposing (Set)
import TypeLookupTable exposing (TypeLookupTable)



{- TODO Look at converting between our Type
   and https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAnnotation
   and https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Type
-}


{-| TODO docs
-}
infer :
    { dependencies : List DependencyPackage
    , files : Dict ModuleName File
    }
    -> Result Error (Dict ModuleName TypeLookupTable)
infer { dependencies, files } =
    let
        deps : Dependencies
        deps =
            Dependencies.fromList dependencies
    in
    files
        |> parseModuleNameKeys
        |> State.fromMaybe MissingModuleName
        |> State.andThen
            (\files_ ->
                State.do (Dependencies.register deps) <| \depAliases ->
                files_
                    |> gatherTypeAliases deps
                    |> State.map (Dict.union depAliases)
                    |> State.andThen (infer_ deps files_)
            )
        |> State.run (State.init Dict.empty)
        |> Tuple.first


parseModuleNameKeys : Dict ModuleName a -> Maybe (Dict FullModuleName a)
parseModuleNameKeys dict =
    dict
        |> Dict.toList
        |> Maybe.Extra.combineMap
            (\( moduleName, value ) ->
                FullModuleName.fromModuleName moduleName
                    |> Maybe.map (\fullModuleName -> ( fullModuleName, value ))
            )
        |> Maybe.map Dict.fromList


infer_ :
    Dependencies
    -> Dict FullModuleName File
    -> Dict ( PackageName, FullModuleName, VarName ) TypeAlias
    -> TIState (Dict ModuleName TypeLookupTable)
infer_ deps files typeAliases =
    State.do (registerConstructorsAndPorts deps files) <| \() ->
    let
        topLevelFunctions : List ( ( FullModuleName, VarName ), ( File, Node Declaration, Expression.Function ) )
        topLevelFunctions =
            files
                |> Dict.toList
                |> List.ExtraExtra.fastConcatMap
                    (\( moduleName, file ) ->
                        file.declarations
                            |> List.filterMap
                                (\declNode ->
                                    case Node.value declNode of
                                        Declaration.FunctionDeclaration fn ->
                                            Just ( ( moduleName, Elm.Syntax.Expression.Extra.functionName fn ), ( file, declNode, fn ) )

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
                    Elm.Syntax.Expression.Extra.referencedNames (Node.value (Node.value fn.declaration).expression)
                        -- Resolve operator aliases to the underlying functions
                        |> List.filterMap
                            (\( maybeModuleName, varName ) ->
                                case VarModuleLookup.moduleOfVar deps files file (Maybe.andThen FullModuleName.fromModuleName maybeModuleName) varName of
                                    Ok (Just ( "", fullModuleName )) ->
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
                                , dependencies = deps
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
    Dependencies
    -> Dict FullModuleName File
    -> TIState (Dict ( PackageName, FullModuleName, VarName ) TypeAlias)
gatherTypeAliases deps files =
    files
        |> Dict.toList
        |> List.map
            (\( moduleName, file ) ->
                let
                    resolver : TypeResolver
                    resolver =
                        VarModuleLookup.typeResolverFor deps files file
                in
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
                                                |> Type.fromTypeAnnotation resolver
                                                |> Result.mapError (State.error << Error.fromTypeAnnotationError)
                                                |> Result.map State.pure
                                                |> Result.Extra.merge
                                    in
                                    type_
                                        |> State.map
                                            (\type__ ->
                                                Just
                                                    ( ( "", moduleName, Node.value typeAlias.name )
                                                    , { args = List.map Node.value typeAlias.generics
                                                      , type_ = type__
                                                      }
                                                    )
                                            )

                                _ ->
                                    State.pure Nothing
                        )
                    |> State.combine
                    |> State.map Maybe.Extra.values
            )
        |> State.combine
        |> State.map (List.ExtraExtra.fastConcat >> Dict.fromList)


registerConstructorsAndPorts : Dependencies -> Dict FullModuleName File -> TIState ()
registerConstructorsAndPorts deps files =
    files
        |> Dict.toList
        |> State.traverse
            (\( moduleName, file ) ->
                let
                    resolver : TypeResolver
                    resolver =
                        VarModuleLookup.typeResolverFor deps files file
                in
                file.declarations
                    |> State.traverse
                        (\declNode ->
                            case Node.value declNode of
                                Declaration.CustomTypeDeclaration customType ->
                                    registerCustomType resolver moduleName customType

                                Declaration.PortDeclaration sig ->
                                    registerPort resolver moduleName sig

                                _ ->
                                    State.pure ()
                        )
                    |> State.map (always ())
            )
        |> State.map (always ())


registerCustomType : TypeResolver -> FullModuleName -> SyntaxType.Type -> TIState ()
registerCustomType resolver moduleName customType =
    let
        typeName : String
        typeName =
            Node.value customType.name

        resultType : MonoType
        resultType =
            UserDefinedType
                { package = ""
                , moduleName = moduleName
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

                    argTypes : Result Type.FromTypeAnnotationError (List MonoType)
                    argTypes =
                        ctor.arguments
                            |> List.map (Node.value >> Type.fromTypeAnnotation resolver)
                            |> Result.Extra.combine
                in
                argTypes
                    |> Result.mapError (State.error << Error.fromTypeAnnotationError)
                    |> Result.map
                        (\args ->
                            let
                                ctorType : MonoType
                                ctorType =
                                    List.foldr (\argT acc -> Function { from = argT, to = acc }) resultType args
                            in
                            State.addGlobalBinding ( "", moduleName, ctorName ) (Type.closeOver ctorType)
                        )
                    |> Result.Extra.merge
            )
        |> State.map (always ())


registerPort : TypeResolver -> FullModuleName -> Signature -> TIState ()
registerPort resolver moduleName sig =
    sig.typeAnnotation
        |> Node.value
        |> Type.fromTypeAnnotation resolver
        |> Result.mapError (State.error << Error.fromTypeAnnotationError)
        |> Result.map
            (\t ->
                State.addGlobalBinding
                    ( "", moduleName, Node.value sig.name )
                    (Type.closeOver t)
            )
        |> Result.Extra.merge
