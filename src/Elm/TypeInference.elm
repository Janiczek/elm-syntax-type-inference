module Elm.TypeInference exposing
    ( inferAndCheck, inferCorrectCode
    , DependencyEnv, dependencyEnv, Dependency
    )

{-| Type inference for [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/)
ASTs.

Note: Type annotations are trusted, not checked: this library is written with
elm-review in mind, which runs _after_ Elm compiler has typechecked the code.
If you would benefit from this library checking annotations, let me know!

TODO: inferAndCheck needs to check the annotations too. inferCorrectCode can trust them.


# Whole project at once

@docs inferAndCheck, inferCorrectCode


# Dependencies

@docs DependencyEnv, dependencyEnv, Dependency

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
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.BindingGroup as BindingGroup
import Elm.TypeInference.Dependencies as Dependencies exposing (Dependencies)
import Elm.TypeInference.Error exposing (Error, ErrorDetails(..))
import Elm.TypeInference.Error.Internal exposing (FromTypeAnnotationError)
import Elm.TypeInference.Infer as Infer
import Elm.TypeInference.ModuleIndex as ModuleIndex exposing (ModuleIndex)
import Elm.TypeInference.ModuleLookup as ModuleLookup
import Elm.TypeInference.SCC as SCC
import Elm.TypeInference.State as State exposing (GlobalKey, TIState)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type exposing (PackageName)
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


{-| Infers types of code already typechecked by Elm compiler.
This invariant allows it to skip sanity checks that could never fire on already-accepted code.
-}
inferCorrectCode :
    { directDependencies : List PackageName
    , allDependencies : List Dependency
    , files : Dict ModuleName File
    }
    -> Result Error (Dict ModuleName TypeLookupTable)
inferCorrectCode =
    infer { checks = False }


{-| Infers types of code that might not typecheck.
Runs all sanity checks.
-}
inferAndCheck :
    { directDependencies : List PackageName
    , allDependencies : List Dependency
    , files : Dict ModuleName File
    }
    -> Result Error (Dict ModuleName TypeLookupTable)
inferAndCheck =
    infer { checks = True }


infer :
    { checks : Bool }
    ->
        { directDependencies : List PackageName
        , allDependencies : List Dependency
        , files : Dict ModuleName File
        }
    -> Result Error (Dict ModuleName TypeLookupTable)
infer checks { directDependencies, allDependencies, files } =
    dependencyEnv
        { directDependencies = directDependencies
        , allDependencies = allDependencies
        }
        |> Result.andThen
            (\depEnv ->
                let
                    project : { tables : Dict ModuleName TypeLookupTable, errors : Dict ModuleName Error }
                    project =
                        inferProject checks depEnv files
                in
                case Dict.values project.errors of
                    [] ->
                        Ok project.tables

                    err :: _ ->
                        Err err
            )



-- DEPENDENCIES


{-| A dependency from the top-level elm.json.
Its `modules` come from the dependency's `docs.json` file.
Its `dependencies` come from the dependency's `elm.json` file.
I
-}
type alias Dependency =
    { name : PackageName
    , dependencies : List PackageName
    , modules : List Elm.Docs.Module
    }


{-| The dependencies' contribution to inference, precomputed.

Dependencies change far less often than source does, so this is worth building
once and reusing across many `inferModule` / `inferProject` calls.

-}
type DependencyEnv
    = DependencyEnv
        { globalEnv : Dict GlobalKey TypeI.Type
        , typeAliases : Dict GlobalKey TypeAlias
        , index : ModuleLookup.Index
        }


{-| `directDependencies` are the project's own direct dependencies: only those
may own a module our source `import`s. `dependencies` is the full transitive
closure, needed because a direct dependency's types mention them.
-}
dependencyEnv :
    { directDependencies : List PackageName
    , allDependencies : List Dependency
    }
    -> Result Error DependencyEnv
dependencyEnv { directDependencies, allDependencies } =
    let
        deps : Dependencies
        deps =
            Dependencies.fromList allDependencies

        directVisibleDeps : Dependencies
        directVisibleDeps =
            allDependencies
                |> List.filter (\pkg -> List.member pkg.name directDependencies)
                |> Dependencies.fromList
    in
    (State.do (Dependencies.register deps) <|
        \depAliases ->
            State.do State.getGlobalEnv <|
                \globalEnv ->
                    State.pure <|
                        DependencyEnv
                            { globalEnv = globalEnv
                            , typeAliases = depAliases
                            , index = ModuleLookup.buildIndex directVisibleDeps
                            }
    )
        |> State.run State.empty
        |> Tuple.first



-- PER-MODULE INFERENCE (internal)


{-| What one module contributes to the modules that import it.

Inference runs one module at a time (Elm forbids import cycles, so a module can
always be inferred once its imports are done). An `Interface` is everything the
importing module needs: it replaces having the imported `File`s around.

The types in here are id-space independent: `State.generalizeWith` substitutes
before quantifying vars younger than the enclosing let, and
`State.lookupGlobalEnv` re-instantiates with fresh ids on every lookup. So an
interface stays valid no matter which `State` consumes it.

  - `moduleIndex` -- the declared/exposed names, imports and infix declarations
    that name resolution in the importing module needs.
  - `values` -- the module's _exposed_ values (functions, constructors, ports,
    record-alias constructors) with their generalized schemes.
  - `typeAliases` -- the module's own type aliases _plus_ every alias it
    inherited from its own imports. A type flowing out of this module's
    signatures can mention an alias the importer never imported itself, and
    `Unify.expandAlias` still has to be able to expand it. (Dependency aliases
    are not in here: they live in the `DependencyEnv`, which every module has
    anyway.)

-}
type Interface
    = Interface
        { moduleIndex : ModuleIndex
        , values : Dict VarName TypeI.Type
        , typeAliases : Dict GlobalKey TypeAlias
        }


{-| Infer a single module, given the interfaces of the modules it imports.

Feed modules in topological order (or use `inferProject`, which does that for you).

-}
inferModule :
    { checks : Bool }
    -> DependencyEnv
    -> Dict ModuleName Interface
    -> File
    -> Result Error { table : TypeLookupTable, interface : Interface }
inferModule checks depEnv importedInterfaces file =
    inferModule_ checks depEnv (fullModuleNameKeys importedInterfaces) file


{-| An interface built from explicit type annotations alone, with no inference
at all: unannotated exposed values simply aren't in it.

This is the degradation path. A module whose inference failed can still give
its dependents _something_ instead of cascading the failure through the whole
import graph.

-}
interfaceFromAnnotations : DependencyEnv -> Dict ModuleName Interface -> File -> Interface
interfaceFromAnnotations depEnv importedInterfaces file =
    interfaceFromAnnotations_ depEnv (fullModuleNameKeys importedInterfaces) file


{-| Infer every module of a project, in dependency order.

Unlike `inferCorrectCode` this reports failures **per
module**: a type error in one module no longer kills the whole run. The failed
module's dependents are inferred against its annotations
(see `interfaceFromAnnotations`).

-}
inferProject :
    { checks : Bool }
    -> DependencyEnv
    -> Dict ModuleName File
    -> { tables : Dict ModuleName TypeLookupTable, errors : Dict ModuleName Error }
inferProject checks depEnv files =
    let
        modules : List ProjectModule
        modules =
            files
                |> Dict.toList
                |> List.filterMap
                    (\( key, file ) ->
                        -- The caller's key is what the result is keyed by; the
                        -- file's own `module Foo exposing (..)` line is what
                        -- imports elsewhere refer to. They agree in practice.
                        if FullModuleName.fromModuleName key == Nothing then
                            Nothing

                        else
                            let
                                index : ModuleIndex
                                index =
                                    ModuleIndex.fromFile file
                            in
                            Just { key = key, index = index, file = file }
                    )

        missingModuleName : Bool
        missingModuleName =
            List.length modules /= Dict.size files

        byName : Dict FullModuleName ProjectModule
        byName =
            modules
                |> List.map (\m -> ( m.index.moduleName, m ))
                |> Dict.fromList

        firstPartyImports : FullModuleName -> List FullModuleName
        firstPartyImports moduleName =
            case Dict.get moduleName byName of
                Nothing ->
                    []

                Just m ->
                    m.index.imports
                        |> List.filterMap
                            (\import_ ->
                                if Dict.member import_.moduleName byName then
                                    Just import_.moduleName

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
            |> List.foldl (inferOne checks depEnv)
                { tables = Dict.empty
                , errors = Dict.empty
                , interfaces = Dict.empty
                }
            |> (\acc -> { tables = acc.tables, errors = acc.errors })


type alias ProjectModule =
    { key : ModuleName
    , index : ModuleIndex
    , file : File
    }


type alias ProjectAcc =
    { tables : Dict ModuleName TypeLookupTable
    , errors : Dict ModuleName Error
    , interfaces : Dict FullModuleName Interface
    }


inferOne : { checks : Bool } -> DependencyEnv -> ProjectModule -> ProjectAcc -> ProjectAcc
inferOne checks depEnv m acc =
    let
        imported : Dict FullModuleName Interface
        imported =
            m.index.imports
                |> List.foldl
                    (\import_ inner ->
                        case Dict.get import_.moduleName acc.interfaces of
                            Just interface ->
                                Dict.insert import_.moduleName interface inner

                            Nothing ->
                                inner
                    )
                    Dict.empty
    in
    case inferModule_ checks depEnv imported m.file of
        Ok { table, interface } ->
            { acc
                | tables = Dict.insert m.key table acc.tables
                , interfaces = Dict.insert m.index.moduleName interface acc.interfaces
            }

        Err err ->
            { acc
                | errors = Dict.insert m.key err acc.errors
                , interfaces =
                    Dict.insert m.index.moduleName
                        (interfaceFromAnnotations_ depEnv imported m.file)
                        acc.interfaces
            }


fullModuleNameKeys : Dict ModuleName a -> Dict FullModuleName a
fullModuleNameKeys dict =
    dict
        |> Dict.toList
        |> List.filterMap
            (\( moduleName, value ) ->
                FullModuleName.fromModuleName moduleName
                    |> Maybe.map (\fullModuleName -> ( fullModuleName, value ))
            )
        |> Dict.fromList



-- THE CORE


{-| Everything a single module's inference needs, derived once from the
`DependencyEnv` and the imported interfaces.
-}
type alias ModuleCtx =
    { thisIndex : ModuleIndex
    , thisModuleName : FullModuleName
    , modules : Dict FullModuleName ModuleIndex
    , resolver : TypeResolver
    , index : ModuleLookup.Index
    , -- what this module passes on to its own importers
      inheritedAliases : Dict GlobalKey TypeAlias
    , depTypeAliases : Dict GlobalKey TypeAlias
    , globalEnv : Dict GlobalKey TypeI.Type
    }


moduleCtx : DependencyEnv -> Dict FullModuleName Interface -> File -> ModuleCtx
moduleCtx (DependencyEnv depEnv) importedInterfaces file =
    let
        thisIndex : ModuleIndex
        thisIndex =
            ModuleIndex.fromFile file

        modules : Dict FullModuleName ModuleIndex
        modules =
            importedInterfaces
                |> Dict.map (\_ (Interface interface) -> interface.moduleIndex)
                |> Dict.insert thisIndex.moduleName thisIndex

        imported :
            { inheritedAliases : Dict GlobalKey TypeAlias
            , globalEnv : Dict GlobalKey TypeI.Type
            }
        imported =
            Dict.foldl
                (\moduleName (Interface interface) acc ->
                    { inheritedAliases = Dict.union interface.typeAliases acc.inheritedAliases
                    , globalEnv =
                        Dict.foldl
                            (\name scheme inner -> Dict.insert ( "", moduleName, name ) scheme inner)
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
    , thisModuleName = thisIndex.moduleName
    , modules = modules
    , resolver = ModuleLookup.typeResolverFor depEnv.index modules thisIndex
    , index = depEnv.index
    , inheritedAliases = imported.inheritedAliases
    , depTypeAliases = depEnv.typeAliases
    , globalEnv = imported.globalEnv
    }


inferModule_ :
    { checks : Bool }
    -> DependencyEnv
    -> Dict FullModuleName Interface
    -> File
    -> Result Error { table : TypeLookupTable, interface : Interface }
inferModule_ { checks } depEnv importedInterfaces file =
    let
        ctx : ModuleCtx
        ctx =
            moduleCtx depEnv importedInterfaces file
    in
    (State.do (gatherTypeAliases ctx file) <|
        \ownAliases ->
            let
                outgoingAliases : Dict GlobalKey TypeAlias
                outgoingAliases =
                    Dict.union ownAliases ctx.inheritedAliases

                typeAliases : Dict GlobalKey TypeAlias
                typeAliases =
                    Dict.union outgoingAliases ctx.depTypeAliases
            in
            State.do (registerConstructorsAndPorts ctx file) <|
                \() ->
                    State.do (solveModule { checks = checks } ctx typeAliases file) <|
                        \() ->
                            State.do (moduleResult ctx outgoingAliases) <|
                                \result ->
                                    State.pure result
    )
        |> State.run (State.init { lexicalEnv = Dict.empty, globalEnv = ctx.globalEnv })
        |> Tuple.first


interfaceFromAnnotations_ : DependencyEnv -> Dict FullModuleName Interface -> File -> Interface
interfaceFromAnnotations_ depEnv importedInterfaces file =
    let
        ctx : ModuleCtx
        ctx =
            moduleCtx depEnv importedInterfaces file
    in
    (State.do (gatherTypeAliases ctx file) <|
        \ownAliases ->
            State.do (registerConstructorsAndPorts ctx file) <|
                \() ->
                    State.do (registerAnnotations ctx file) <|
                        \() ->
                            State.do (moduleResult ctx (Dict.union ownAliases ctx.inheritedAliases)) <|
                                \result ->
                                    State.pure result.interface
    )
        |> State.run (State.init { lexicalEnv = Dict.empty, globalEnv = ctx.globalEnv })
        |> Tuple.first
        |> Result.withDefault
            (Interface
                { moduleIndex = ctx.thisIndex
                , values = Dict.empty
                , typeAliases = ctx.inheritedAliases
                }
            )


moduleResult :
    ModuleCtx
    -> Dict GlobalKey TypeAlias
    ->
        TIState
            { table : TypeLookupTable
            , interface : Interface
            }
moduleResult ctx outgoingAliases =
    -- TODO translate from TypeI.Type to Type.Type before inserting into the dict
    State.do State.getNodeIds <|
        \nodeIds ->
            State.do State.getSubst <|
                \substitutionMap ->
                    State.do State.getGlobalEnv <|
                        \globalEnv ->
                            let
                                ( typesByRange, _ ) =
                                    nodeIds
                                        |> Dict.foldl
                                            (\rangeLike id ( accDict, accSubst ) ->
                                                let
                                                    ( monoType, accSubst1 ) =
                                                        SubstitutionMap.substituteMono accSubst (TypeI.id_ id)
                                                in
                                                ( Dict.insert rangeLike (TypeI.toPublicType { alreadyNormalized = False } monoType) accDict
                                                , accSubst1
                                                )
                                            )
                                            ( Dict.empty, substitutionMap )

                                exposedValues : Dict VarName TypeI.Type
                                exposedValues =
                                    ctx.thisIndex.exposedValues
                                        |> Set.foldl
                                            (\name acc ->
                                                case Dict.get ( "", ctx.thisModuleName, name ) globalEnv of
                                                    Just scheme ->
                                                        Dict.insert name scheme acc

                                                    Nothing ->
                                                        acc
                                            )
                                            Dict.empty
                            in
                            State.pure
                                { table = TypeLookupTable.Internal.TLT typesByRange
                                , interface =
                                    Interface
                                        { moduleIndex = ctx.thisIndex
                                        , values = exposedValues
                                        , typeAliases = outgoingAliases
                                        }
                                }



-- SOLVING ONE MODULE'S TOP-LEVEL DECLARATIONS


solveModule :
    { checks : Bool }
    -> ModuleCtx
    -> Dict GlobalKey TypeAlias
    -> File
    -> TIState ()
solveModule { checks } ctx typeAliases file =
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
                                case ModuleLookup.moduleOfVar ctx.index ctx.modules ctx.thisIndex (Maybe.andThen FullModuleName.fromModuleName maybeModuleName) varName of
                                    Ok (Just ( "", fullModuleName )) ->
                                        let
                                            ( resolvedModule, resolvedName ) =
                                                ModuleLookup.resolveOperatorFunction ctx.modules fullModuleName varName
                                                    |> Result.withDefault Nothing
                                                    |> Maybe.withDefault ( fullModuleName, varName )
                                        in
                                        -- Only this module's own declarations
                                        -- are being ordered here; everything
                                        -- else is already in `globalEnv`.
                                        if resolvedModule == ctx.thisModuleName && Set.member resolvedName nodeSet then
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
            , thisModuleName = ctx.thisModuleName
            , typeAliases = typeAliases
            , index = ctx.index
            , checks = checks
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
                            { typeAliases = typeAliases
                            , checks = checks
                            , internalChecks = not checks
                            , moduleName = ctx.thisModuleName
                            , declarationNames = group
                            }
                        )
            )
        |> State.map (always ())



-- REGISTERING A MODULE'S DECLARATIONS


gatherTypeAliases : ModuleCtx -> File -> TIState (Dict GlobalKey TypeAlias)
gatherTypeAliases ctx file =
    let
        resolver : TypeResolver
        resolver =
            ctx.resolver

        moduleName : FullModuleName
        moduleName =
            ctx.thisModuleName
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

                            type_ : TIState MonoType
                            type_ =
                                typeAlias.typeAnnotation
                                    |> Node.value
                                    |> TypeI.fromTypeAnnotation resolver
                                    |> Result.mapError (State.error << toError << TypeI.fromTypeAnnotationError)
                                    |> Result.map State.pure
                                    |> Result.Extra.merge

                            -- A record type alias also gets a constructor function
                            -- (eg. `type alias Foo = { a : Int }` lets you write `Foo 1`).
                            registerConstructor : MonoType -> TIState ()
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
                                                        ( "", moduleName, Node.value typeAlias.name )
                                                        (TypeI.closeOver ctorType)
                                                )

                                    _ ->
                                        State.pure ()
                        in
                        State.do type_ <|
                            \type__ ->
                                State.do (registerConstructor type__) <|
                                    \() ->
                                        State.pure <|
                                            Just
                                                ( ( "", moduleName, Node.value typeAlias.name )
                                                , { args = List.map Node.value typeAlias.generics
                                                  , type_ = type__
                                                  }
                                                )

                    _ ->
                        State.pure Nothing
            )
        |> State.map (Maybe.Extra.values >> Dict.fromList)


registerConstructorsAndPorts : ModuleCtx -> File -> TIState ()
registerConstructorsAndPorts ctx file =
    file.declarations
        |> State.traverse
            (\declNode ->
                case Node.value declNode of
                    Declaration.CustomTypeDeclaration customType ->
                        registerCustomType ctx.resolver ctx.thisModuleName customType

                    Declaration.PortDeclaration sig ->
                        registerPort ctx.resolver ctx.thisModuleName sig

                    _ ->
                        State.pure ()
            )
        |> State.map (always ())


{-| The no-inference path: take the explicit annotations at face value and put
them in `globalEnv`. Annotations that don't resolve are skipped rather than
failing the whole module -- this is already the degradation path.
-}
registerAnnotations : ModuleCtx -> File -> TIState ()
registerAnnotations ctx file =
    file.declarations
        |> State.traverse
            (\declNode ->
                case Node.value declNode of
                    Declaration.FunctionDeclaration fn ->
                        case fn.signature of
                            Nothing ->
                                State.pure ()

                            Just sigNode ->
                                case
                                    Node.value (Node.value sigNode).typeAnnotation
                                        |> TypeI.fromTypeAnnotation ctx.resolver
                                of
                                    Err _ ->
                                        State.pure ()

                                    Ok monoType ->
                                        State.addGlobalBinding
                                            ( "", ctx.thisModuleName, Elm.Syntax.Expression.Extra.functionName fn )
                                            (TypeI.closeOver monoType)

                    _ ->
                        State.pure ()
            )
        |> State.map (always ())


registerCustomType :
    TypeResolver
    -> FullModuleName
    -> SyntaxType.Type
    -> TIState ()
registerCustomType resolver moduleName customType =
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
                , moduleName = moduleName
                , name = typeName
                , args =
                    customType.generics
                        |> List.map
                            (\g ->
                                TypeVar
                                    ( TypeVar.Named (Node.value g)
                                    , TypeVar.Normal
                                    )
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
                            State.addGlobalBinding ( "", moduleName, ctorName ) (TypeI.closeOver ctorType)
                        )
                    |> Result.Extra.merge
            )
        |> State.map (always ())


registerPort : TypeResolver -> FullModuleName -> Signature -> TIState ()
registerPort resolver moduleName sig =
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
                    ( "", moduleName, Node.value sig.name )
                    (TypeI.closeOver t)
            )
        |> Result.Extra.merge
