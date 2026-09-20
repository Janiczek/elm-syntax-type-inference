module Elm.TypeInference exposing
    ( inferProject
    , DependencyEnv, DependencyEnvOutcome(..), dependencyEnv
    , Dependency
    )

{-| Type inference for [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/)
ASTs.


# Whole project at once

@docs inferProject


# Dependencies

@docs DependencyEnv, DependencyEnvOutcome, dependencyEnv
@docs Dependency

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Expression.Extra
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as SyntaxType
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.TypeInference.BindingGroup as BindingGroup
import Elm.TypeInference.Dependencies as Dependencies exposing (Dependencies)
import Elm.TypeInference.DependencySources as DependencySources
import Elm.TypeInference.Error exposing (Error, ErrorDetails(..))
import Elm.TypeInference.Error.Internal exposing (FromTypeAnnotationError)
import Elm.TypeInference.Infer as Infer
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.ModuleIndex as ModuleIndex exposing (ModuleIndex)
import Elm.TypeInference.ModuleLookup as ModuleLookup
import Elm.TypeInference.SCC as SCC
import Elm.TypeInference.State as State exposing (GlobalKey, StateM)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type exposing (PackageName, VarName)
import Elm.TypeInference.Type.Internal as TypeI exposing (MonoType(..), TypeResolver)
import Elm.TypeInference.TypeVar as TypeVar
import Elm.TypeInference.Unify exposing (TypeAlias)
import List.ExtraExtra
import Maybe.Extra
import Result.Extra
import Set exposing (Set)
import TypeLookupTable exposing (TypeLookupTable)
import TypeLookupTable.Internal



-- WHOLE-PROJECT ENTRY POINTS


{-| Infer every module of a project.
-}
inferProject :
    Maybe PackageName
    -> DependencyEnv
    -> Dict ModuleName File
    -> { tables : Dict ModuleName TypeLookupTable, errors : Dict ModuleName Error }
inferProject currentPackage depEnv files =
    let
        (DependencyEnv dep) =
            depEnv

        ( modules, moduleMapping ) =
            Dict.foldl
                (\key file ( acc, accModuleMapping ) ->
                    if FullModuleName.fromModuleName key == Nothing then
                        ( acc, accModuleMapping )

                    else
                        let
                            ( index, newModuleMapping ) =
                                ModuleIndex.fromFile accModuleMapping file
                        in
                        ( { key = key, index = index, file = file } :: acc, newModuleMapping )
                )
                ( [], dep.moduleMapping )
                files
                |> (\( reversed, finalModuleMapping ) -> ( List.reverse reversed, finalModuleMapping ))

        missingModuleName : Bool
        missingModuleName =
            List.length modules /= Dict.size files

        byName : Dict ModuleId ProjectModule
        byName =
            modules
                |> List.map (\m -> ( m.index.moduleId, m ))
                |> Dict.fromList

        firstPartyImports : ModuleId -> List ModuleId
        firstPartyImports moduleId =
            case Dict.get moduleId byName of
                Nothing ->
                    []

                Just m ->
                    m.index.imports
                        |> List.filterMap
                            (\import_ ->
                                if Dict.member import_.moduleId byName then
                                    Just import_.moduleId

                                else
                                    Nothing
                            )
    in
    if missingModuleName then
        { tables = Dict.empty
        , errors =
            Dict.singleton []
                { moduleName = [ "<Missing>" ]
                , declarationNames = []
                , details = MissingModuleName
                }
        }

    else
        let
            {- Tarjan emits a component only after everything it can reach, so this
               is already in dependency-first order. Elm forbids import cycles, so
               each component is a single module -- but if the caller hands us one
               anyway we still infer every module in it, just without the benefit
               of its cyclic partners' interfaces.
            -}
            order : List ProjectModule
            order =
                SCC.stronglyConnectedComponents (Dict.keys byName) firstPartyImports
                    |> List.ExtraExtra.fastConcatMap (List.filterMap (\name -> Dict.get name byName))
        in
        order
            |> List.foldl (inferOne currentPackage depEnv moduleMapping)
                { tables = Dict.empty
                , errors = Dict.empty
                , interfaces = Dict.empty
                }
            |> (\acc -> { tables = acc.tables, errors = acc.errors })



-- DEPENDENCIES


{-| A dependency package with its type information.

  - `name` -- the package identifier (e.g. `"elm/core"`).
  - `dependencies` -- names of the package's _immediate_ `elm.json` dependencies (eg. "elm/json").
  - `modules` -- the decoded `docs.json` modules

-}
type alias Dependency =
    { name : PackageName
    , dependencies : List PackageName
    , modules : List Elm.Docs.Module
    }


{-| Dependency types and other info computed from dependencies' docs.json files.

This cache doesn't change as user's project code changes - only invalidate it
when elm.json changes.

-}
type DependencyEnv
    = DependencyEnv
        { globalEnv : Dict GlobalKey TypeI.Type
        , typeAliases : Dict GlobalKey TypeAlias
        , index : ModuleLookup.Index
        , moduleMapping : ModuleIds.Mapping
        }


{-| Did dependencies process correctly?
-}
type DependencyEnvOutcome
    = Ready DependencyEnv
    | NeedPackageSources (Dict PackageName (List String))
    | Failed Error


{-| Build a `DependencyEnv`.

Start by running `dependencyEnv` with empty `sourcesToResolveAmbiguity`.

If you get `NeedPackageSources` back, read those Elm files from the dependencies in
your ELM\_HOME and supply them in `sourcesToResolveAmbiguity` in the next call.

If you get `Failed` back, the dependencies' `docs.json` types could not be
resolved.

-}
dependencyEnv :
    { directDependencies : List PackageName
    , allDependencies : List Dependency
    , sourcesToResolveAmbiguity : Dict PackageName (List File)
    }
    -> DependencyEnvOutcome
dependencyEnv { directDependencies, allDependencies, sourcesToResolveAmbiguity } =
    let
        deps : Dependencies
        deps =
            Dependencies.fromList allDependencies

        directVisibleDeps : Dependencies
        directVisibleDeps =
            allDependencies
                |> List.filter (\pkg -> List.member pkg.name directDependencies)
                |> Dependencies.fromList

        depModuleNames : List FullModuleName
        depModuleNames =
            (allDependencies
                |> List.ExtraExtra.fastConcatMap (\pkg -> List.map (\m -> FullModuleName.fromDotted m.name) pkg.modules)
            )
                ++ (DependencySources.referencedModules deps
                        |> List.map FullModuleName.fromDotted
                   )

        moduleMapping0 : ModuleIds.Mapping
        moduleMapping0 =
            List.foldl (\name acc -> ModuleIds.intern name acc |> Tuple.second) ModuleIds.empty depModuleNames

        ( depIndex, moduleMapping1 ) =
            ModuleLookup.buildIndex moduleMapping0 directVisibleDeps

        baseEnv : Result Error DependencyEnv
        baseEnv =
            (State.do (Dependencies.register moduleMapping1 deps) <| \( depAliases, moduleMapping2 ) ->
            State.do State.getGlobalEnv <| \globalEnv ->
            State.pure <|
                DependencyEnv
                    { globalEnv = globalEnv
                    , typeAliases = depAliases
                    , index = depIndex
                    , moduleMapping = moduleMapping2
                    }
            )
                |> State.run State.empty
                |> Tuple.first
    in
    case baseEnv of
        Err err ->
            Failed err

        Ok (DependencyEnv env) ->
            let
                reachable : Set PackageName
                reachable =
                    reachablePackages deps directDependencies

                needed : Dict PackageName (List String)
                needed =
                    DependencySources.neededSources deps sourcesToResolveAmbiguity
                        |> List.filter (\( pkg, _ ) -> Set.member pkg reachable)
                        |> Dict.fromList
            in
            if Dict.isEmpty needed then
                case DependencySources.aliases env.moduleMapping deps sourcesToResolveAmbiguity of
                    Err err ->
                        Failed err

                    Ok ( sourceAliases, moduleMapping2 ) ->
                        Ready
                            (DependencyEnv
                                { env
                                    | typeAliases = Dict.union sourceAliases env.typeAliases
                                    , moduleMapping = moduleMapping2
                                }
                            )

            else
                NeedPackageSources needed


reachablePackages : Dependencies -> List PackageName -> Set PackageName
reachablePackages deps roots =
    reachablePackagesHelp deps roots Set.empty


reachablePackagesHelp : Dependencies -> List PackageName -> Set PackageName -> Set PackageName
reachablePackagesHelp deps queue seen =
    case queue of
        [] ->
            seen

        name :: rest ->
            if Set.member name seen then
                reachablePackagesHelp deps rest seen

            else
                case Dict.get name deps of
                    Nothing ->
                        reachablePackagesHelp deps rest (Set.insert name seen)

                    Just pkg ->
                        reachablePackagesHelp deps (rest ++ pkg.dependencies) (Set.insert name seen)



-- PER-MODULE INFERENCE (internal)


{-| What one module contributes to the modules that import it.
-}
type alias ModuleInterface =
    { moduleIndex : ModuleIndex
    , values : Dict VarName TypeI.Type
    , typeAliases : Dict GlobalKey TypeAlias
    }


type alias ProjectModule =
    { key : ModuleName
    , index : ModuleIndex
    , file : File
    }


type alias ProjectAcc =
    { tables : Dict ModuleName TypeLookupTable
    , errors : Dict ModuleName Error
    , interfaces : Dict ModuleId ModuleInterface
    }


inferOne : Maybe PackageName -> DependencyEnv -> ModuleIds.Mapping -> ProjectModule -> ProjectAcc -> ProjectAcc
inferOne currentPackage depEnv moduleMapping m acc =
    let
        imported : Dict ModuleId ModuleInterface
        imported =
            m.index.imports
                |> List.foldl
                    (\import_ inner ->
                        case Dict.get import_.moduleId acc.interfaces of
                            Just interface ->
                                Dict.insert import_.moduleId interface inner

                            Nothing ->
                                inner
                    )
                    Dict.empty
    in
    case inferModule_ currentPackage depEnv moduleMapping imported m.file of
        Ok { table, interface } ->
            { acc
                | tables = Dict.insert m.key table acc.tables
                , interfaces = Dict.insert m.index.moduleId interface acc.interfaces
            }

        Err err ->
            { acc
                | errors = Dict.insert m.key err acc.errors
                , interfaces =
                    Dict.insert m.index.moduleId
                        { moduleIndex = m.index
                        , values = Dict.empty
                        , typeAliases = Dict.empty
                        }
                        acc.interfaces
            }



-- THE CORE


{-| Everything a single module's inference needs, derived once from the
`DependencyEnv` and the imported interfaces.
-}
type alias ModuleCtx =
    { thisIndex : ModuleIndex
    , modules : Dict ModuleId ModuleIndex
    , resolver : TypeResolver
    , index : ModuleLookup.Index
    , moduleMapping : ModuleIds.Mapping
    , -- what this module passes on to its own importers
      inheritedAliases : Dict GlobalKey TypeAlias
    , depTypeAliases : Dict GlobalKey TypeAlias
    , globalEnv : Dict GlobalKey TypeI.Type
    , allowKernel : Bool
    }


allowsKernel : Maybe PackageName -> Bool
allowsKernel currentPackage =
    case currentPackage of
        Nothing ->
            True

        Just name ->
            String.startsWith "elm/" name
                || String.startsWith "elm-explorations/" name


moduleCtx : Maybe PackageName -> DependencyEnv -> ModuleIds.Mapping -> Dict ModuleId ModuleInterface -> File -> ModuleCtx
moduleCtx currentPackage (DependencyEnv depEnv) moduleMapping importedInterfaces file =
    let
        ( thisIndex, _ ) =
            ModuleIndex.fromFile moduleMapping file

        modules : Dict ModuleId ModuleIndex
        modules =
            importedInterfaces
                |> Dict.map (\_ interface -> interface.moduleIndex)
                |> Dict.insert thisIndex.moduleId thisIndex

        imported :
            { inheritedAliases : Dict GlobalKey TypeAlias
            , globalEnv : Dict GlobalKey TypeI.Type
            }
        imported =
            Dict.foldl
                (\moduleId interface acc ->
                    { inheritedAliases = Dict.union interface.typeAliases acc.inheritedAliases
                    , globalEnv =
                        Dict.foldl
                            (\name scheme inner -> Dict.insert ( "", moduleId, name ) scheme inner)
                            acc.globalEnv
                            interface.values
                    }
                )
                { inheritedAliases = Dict.empty
                , globalEnv = depEnv.globalEnv
                }
                importedInterfaces
    in
    { thisIndex = thisIndex
    , modules = modules
    , resolver = ModuleLookup.typeResolverFor moduleMapping depEnv.index modules thisIndex
    , index = depEnv.index
    , moduleMapping = moduleMapping
    , inheritedAliases = imported.inheritedAliases
    , depTypeAliases = depEnv.typeAliases
    , globalEnv = imported.globalEnv
    , allowKernel = allowsKernel currentPackage
    }


inferModule_ :
    Maybe PackageName
    -> DependencyEnv
    -> ModuleIds.Mapping
    -> Dict ModuleId ModuleInterface
    -> File
    -> Result Error { table : TypeLookupTable, interface : ModuleInterface }
inferModule_ currentPackage depEnv moduleMapping importedInterfaces file =
    let
        ctx : ModuleCtx
        ctx =
            moduleCtx currentPackage depEnv moduleMapping importedInterfaces file
    in
    (State.do (gatherTypeAliases ctx file) <| \ownAliases ->
    let
        outgoingAliases : Dict GlobalKey TypeAlias
        outgoingAliases =
            Dict.union ownAliases ctx.inheritedAliases

        typeAliases : Dict GlobalKey TypeAlias
        typeAliases =
            Dict.union outgoingAliases ctx.depTypeAliases
    in
    State.do (registerConstructorsAndPorts ctx file) <| \() ->
    State.do (registerEffectMagic ctx) <| \() ->
    State.do (solveModule ctx typeAliases file) <| \() ->
    State.do (moduleResult ctx outgoingAliases) <| \result ->
    State.pure result
    )
        |> State.run (State.init ctx.globalEnv)
        |> Tuple.first


moduleResult :
    ModuleCtx
    -> Dict GlobalKey TypeAlias
    ->
        StateM
            { table : TypeLookupTable
            , interface : ModuleInterface
            }
moduleResult ctx outgoingAliases =
    State.do State.getNodeIds <| \nodeIds ->
    State.do State.getSubst <| \substitutionMap ->
    State.do State.getGlobalEnv <| \globalEnv ->
    let
        exposedValues : Dict VarName TypeI.Type
        exposedValues =
            ctx.thisIndex.exposedValues
                |> Set.foldl
                    (\name acc ->
                        case Dict.get ( "", ctx.thisIndex.moduleId, name ) globalEnv of
                            Just scheme ->
                                Dict.insert name scheme acc

                            Nothing ->
                                acc
                    )
                    Dict.empty
    in
    State.pure
        { table =
            TypeLookupTable.Internal.TLT
                { nodeIds = nodeIds
                , subst = SubstitutionMap.forLookup substitutionMap
                , moduleMapping = ctx.moduleMapping
                , cache = Dict.empty
                , pool = Dict.empty
                }
        , interface =
            { moduleIndex = ctx.thisIndex
            , values = exposedValues
            , typeAliases = outgoingAliases
            }
        }



-- SOLVING ONE MODULE'S TOP-LEVEL DECLARATIONS


solveModule :
    ModuleCtx
    -> Dict GlobalKey TypeAlias
    -> File
    -> StateM ()
solveModule ctx typeAliases file =
    let
        topLevelFunctions : List ( VarName, ( Node Declaration, Expression.Function ) )
        topLevelFunctions =
            file.declarations
                |> List.filterMap
                    (\declNode ->
                        case Node.value declNode of
                            Declaration.FunctionDeclaration fn ->
                                Just
                                    ( Elm.Syntax.Expression.Extra.functionName fn
                                    , ( declNode, fn )
                                    )

                            _ ->
                                Nothing
                    )

        ( nodeSet, byKey ) =
            List.foldl
                (\( name, member ) ( names, dict ) ->
                    ( Set.insert name names
                    , Dict.insert name member dict
                    )
                )
                ( Set.empty, Dict.empty )
                topLevelFunctions

        edges : VarName -> List VarName
        edges key =
            case Dict.get key byKey of
                Nothing ->
                    []

                Just ( _, fn ) ->
                    Elm.Syntax.Expression.Extra.referencedNames (Node.value (Node.value fn.declaration).expression)
                        -- Resolve operator aliases to the underlying functions
                        |> List.filterMap
                            (\( maybeModuleName, varName ) ->
                                case ModuleLookup.moduleOfVar ctx.moduleMapping ctx.index ctx.modules ctx.thisIndex (Maybe.andThen FullModuleName.fromModuleName maybeModuleName) varName of
                                    Ok (Just ( "", moduleId )) ->
                                        let
                                            ( resolvedModule, resolvedName ) =
                                                ModuleLookup.resolveOperatorFunction ctx.moduleMapping ctx.modules moduleId varName
                                                    |> Result.withDefault Nothing
                                                    |> Maybe.withDefault ( moduleId, varName )
                                        in
                                        -- Only this module's own declarations
                                        -- are being ordered here; everything
                                        -- else is already in `globalEnv`.
                                        if resolvedModule == ctx.thisIndex.moduleId && Set.member resolvedName nodeSet then
                                            Just resolvedName

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )

        sccs : List (List VarName)
        sccs =
            SCC.stronglyConnectedComponents (Set.toList nodeSet) edges

        inferCtx : Infer.Ctx
        inferCtx =
            { modules = ctx.modules
            , thisModule = ctx.thisIndex
            , typeAliases = typeAliases
            , index = ctx.index
            , allowKernel = ctx.allowKernel
            , moduleMapping = ctx.moduleMapping
            }
    in
    sccs
        |> State.traverse
            (\group ->
                group
                    |> List.filterMap (\key -> Dict.get key byKey)
                    |> State.traverse (\( declNode, fn ) -> Infer.topLevelMember inferCtx declNode fn)
                    |> State.andThen
                        (BindingGroup.solveGroup
                            (Infer.unifyConfigForGroup inferCtx group)
                        )
            )
        |> State.map (always ())



-- REGISTERING A MODULE'S DECLARATIONS


gatherTypeAliases : ModuleCtx -> File -> StateM (Dict GlobalKey TypeAlias)
gatherTypeAliases ctx file =
    let
        resolver : TypeResolver
        resolver =
            ctx.resolver

        moduleName : FullModuleName
        moduleName =
            ctx.thisIndex.moduleName

        moduleId : ModuleId
        moduleId =
            ctx.thisIndex.moduleId
    in
    file.declarations
        |> State.traverse
            (\declarationNode ->
                case Node.value declarationNode of
                    Declaration.AliasDeclaration typeAlias ->
                        let
                            toError : ErrorDetails -> Error
                            toError details =
                                { moduleName = FullModuleName.toModuleName moduleName
                                , declarationNames = [ Node.value typeAlias.name ]
                                , details = details
                                }

                            type_ : StateM MonoType
                            type_ =
                                typeAlias.typeAnnotation
                                    |> Node.value
                                    |> TypeI.fromTypeAnnotation resolver
                                    |> Result.mapError (State.error << toError << TypeI.fromTypeAnnotationError)
                                    |> Result.map State.pure
                                    |> Result.Extra.merge

                            -- A record type alias also gets a constructor function
                            -- (eg. `type alias Foo = { a : Int }` lets you write `Foo 1`).
                            registerConstructor : MonoType -> StateM ()
                            registerConstructor aliasMono =
                                case Node.value typeAlias.typeAnnotation of
                                    TypeAnnotation.Record fields ->
                                        fields
                                            |> State.traverse
                                                (\fieldNode ->
                                                    Tuple.second (Node.value fieldNode)
                                                        |> Node.value
                                                        |> TypeI.fromTypeAnnotation resolver
                                                        |> Result.mapError (State.error << toError << TypeI.fromTypeAnnotationError)
                                                        |> Result.map State.pure
                                                        |> Result.Extra.merge
                                                )
                                            |> State.map
                                                (\fieldTypes ->
                                                    fieldTypes
                                                        |> List.foldr (\fieldT acc -> Function { from = fieldT, to = acc }) aliasMono
                                                )
                                            |> State.andThen
                                                (\ctorType ->
                                                    State.addGlobalBinding
                                                        ( "", moduleId, Node.value typeAlias.name )
                                                        (TypeI.closeOver ctorType)
                                                )

                                    _ ->
                                        State.pure ()
                        in
                        State.do type_ <| \type__ ->
                        State.do (registerConstructor type__) <| \() ->
                        State.pure <|
                            Just
                                ( ( "", moduleId, Node.value typeAlias.name )
                                , { args = List.map (Node.value >> TypeVar.parse) typeAlias.generics
                                  , type_ = type__
                                  }
                                )

                    _ ->
                        State.pure Nothing
            )
        |> State.map (Maybe.Extra.values >> Dict.fromList)


registerConstructorsAndPorts : ModuleCtx -> File -> StateM ()
registerConstructorsAndPorts ctx file =
    file.declarations
        |> State.traverse
            (\declNode ->
                case Node.value declNode of
                    Declaration.CustomTypeDeclaration customType ->
                        registerCustomType ctx.resolver ctx.thisIndex.moduleId ctx.thisIndex.moduleName customType

                    Declaration.PortDeclaration sig ->
                        registerPort ctx.resolver ctx.thisIndex.moduleId ctx.thisIndex.moduleName sig

                    _ ->
                        State.pure ()
            )
        |> State.map (always ())


registerCustomType :
    TypeResolver
    -> ModuleId
    -> FullModuleName
    -> SyntaxType.Type
    -> StateM ()
registerCustomType resolver moduleId moduleName customType =
    let
        typeName : String
        typeName =
            Node.value customType.name

        toError : ErrorDetails -> Error
        toError details =
            { moduleName = FullModuleName.toModuleName moduleName
            , declarationNames = [ typeName ]
            , details = details
            }

        resultType : MonoType
        resultType =
            UserDefinedType
                { package = ""
                , moduleId = moduleId
                , name = typeName
                , args =
                    customType.generics
                        |> List.map
                            (\g ->
                                TypeVar
                                    (TypeVar.parse (Node.value g))
                            )
                }
    in
    customType.constructors
        |> State.traverse
            (\ctorNode ->
                let
                    ctor : SyntaxType.ValueConstructor
                    ctor =
                        Node.value ctorNode

                    argTypes : Result FromTypeAnnotationError (List MonoType)
                    argTypes =
                        ctor.arguments
                            |> List.map (Node.value >> TypeI.fromTypeAnnotation resolver)
                            |> Result.Extra.combine
                in
                argTypes
                    |> Result.mapError (State.error << toError << TypeI.fromTypeAnnotationError)
                    |> Result.map
                        (\args ->
                            let
                                ctorName : String
                                ctorName =
                                    Node.value ctor.name

                                ctorType : MonoType
                                ctorType =
                                    List.foldr (\argT acc -> Function { from = argT, to = acc }) resultType args
                            in
                            State.addGlobalBinding ( "", moduleId, ctorName ) (TypeI.closeOver ctorType)
                        )
                    |> Result.Extra.merge
            )
        |> State.map (always ())


registerPort : TypeResolver -> ModuleId -> FullModuleName -> Signature -> StateM ()
registerPort resolver moduleId moduleName sig =
    let
        toError : ErrorDetails -> Error
        toError details =
            { moduleName = FullModuleName.toModuleName moduleName
            , declarationNames = [ Node.value sig.name ]
            , details = details
            }
    in
    sig.typeAnnotation
        |> Node.value
        |> TypeI.fromTypeAnnotation resolver
        |> Result.mapError (State.error << toError << TypeI.fromTypeAnnotationError)
        |> Result.map
            (\t ->
                State.addGlobalBinding
                    ( "", moduleId, Node.value sig.name )
                    (TypeI.closeOver t)
            )
        |> Result.Extra.merge


{-| Register the magic `command` / `subscription` values for `effect module`s.

The Elm compiler magically provides:

    command : MyCmd msg -> Cmd msg

    subscription : MySub msg -> Sub msg

`MyCmd` / `MySub` are the custom types named in the module header

    effect module Random where { command = MyCmd } exposing (..)

-}
registerEffectMagic : ModuleCtx -> StateM ()
registerEffectMagic ctx =
    if ctx.allowKernel then
        State.do (registerEffectCommand ctx) <| \() ->
        registerEffectSubscription ctx

    else
        State.pure ()


registerEffectCommand : ModuleCtx -> StateM ()
registerEffectCommand ctx =
    case ctx.thisIndex.effectCommand of
        Nothing ->
            State.pure ()

        Just myCmdName ->
            case ctx.resolver [] "Cmd" of
                Err _ ->
                    State.pure ()

                Ok ( cmdPackage, cmdModuleId ) ->
                    let
                        msgVar : MonoType
                        msgVar =
                            TypeVar (TypeVar.parse "msg")

                        magicType : MonoType
                        magicType =
                            Function
                                { from =
                                    UserDefinedType
                                        { package = ""
                                        , moduleId = ctx.thisIndex.moduleId
                                        , name = myCmdName
                                        , args = [ msgVar ]
                                        }
                                , to =
                                    UserDefinedType
                                        { package = cmdPackage
                                        , moduleId = cmdModuleId
                                        , name = "Cmd"
                                        , args = [ msgVar ]
                                        }
                                }
                    in
                    State.addGlobalBinding
                        ( "", ctx.thisIndex.moduleId, ModuleIndex.effectCommandVar )
                        (TypeI.closeOver magicType)


registerEffectSubscription : ModuleCtx -> StateM ()
registerEffectSubscription ctx =
    case ctx.thisIndex.effectSubscription of
        Nothing ->
            State.pure ()

        Just mySubName ->
            case ctx.resolver [] "Sub" of
                Err _ ->
                    State.pure ()

                Ok ( subPackage, subModuleId ) ->
                    let
                        msgVar : MonoType
                        msgVar =
                            TypeVar (TypeVar.parse "msg")

                        magicType : MonoType
                        magicType =
                            Function
                                { from =
                                    UserDefinedType
                                        { package = ""
                                        , moduleId = ctx.thisIndex.moduleId
                                        , name = mySubName
                                        , args = [ msgVar ]
                                        }
                                , to =
                                    UserDefinedType
                                        { package = subPackage
                                        , moduleId = subModuleId
                                        , name = "Sub"
                                        , args = [ msgVar ]
                                        }
                                }
                    in
                    State.addGlobalBinding
                        ( "", ctx.thisIndex.moduleId, ModuleIndex.effectSubscriptionVar )
                        (TypeI.closeOver magicType)
