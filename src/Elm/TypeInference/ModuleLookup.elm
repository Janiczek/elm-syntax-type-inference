module Elm.TypeInference.ModuleLookup exposing
    ( Index
    , buildIndex
    , findModuleOfVar
    , moduleOfVar
    , resolveOperatorFunction
    , typeResolverFor
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.VarName exposing (VarName)
import Elm.Type
import Elm.TypeInference.Dependencies exposing (Dependencies)
import Elm.TypeInference.Error exposing (ErrorDetails(..))
import Elm.TypeInference.Error.Internal exposing (ResolverAmbiguity)
import Elm.TypeInference.ImplicitImports as ImplicitImports
import Elm.TypeInference.ModuleIndex as ModuleIndex exposing (ImportIndex, ModuleIndex)
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.Type exposing (PackageName)
import Elm.TypeInference.Type.Internal exposing (TypeResolver)
import List.ExtraExtra
import Result.Extra
import Result.ExtraExtra
import Set


{-| A precomputed `module name -> value/type name -> packages defining it` index,
built once per `infer` run and reused for every name occurrence, instead of
rescanning every package × module × value on each lookup (see
`Elm.TypeInference.Dependencies.DependencyPackage`).

Search order matches iterating `Dict.toList deps` directly (packages in
alphabetical order, modules in their package's declared order), so
`AmbiguousModuleOwner` errors still list candidates in the same order as
before.

-}
type Index
    = Index
        { values : Dict String (Dict VarName (List PackageName))
        , types : Dict String (Dict VarName (List PackageName))
        }


buildIndex : Dependencies -> Index
buildIndex deps =
    deps
        |> Dict.foldl
            (\packageName pkg acc ->
                List.foldl (addModule packageName) acc pkg.modules
            )
            emptyIndex


addModule : PackageName -> Elm.Docs.Module -> Index -> Index
addModule packageName mod (Index idx) =
    Index
        { values = List.foldl (addName packageName mod.name) idx.values (valueNamesOf mod)
        , types = List.foldl (addName packageName mod.name) idx.types (typeNamesOf mod)
        }


valueNamesOf : Elm.Docs.Module -> List VarName
valueNamesOf mod =
    List.map .name mod.values
        ++ List.map .name mod.binops
        ++ List.ExtraExtra.fastConcatMap (\u -> List.map Tuple.first u.tags) mod.unions
        ++ (mod.aliases |> List.filter isRecordAlias |> List.map .name)


typeNamesOf : Elm.Docs.Module -> List VarName
typeNamesOf mod =
    List.map .name mod.unions ++ List.map .name mod.aliases


addName :
    PackageName
    -> String
    -> VarName
    -> Dict String (Dict VarName (List PackageName))
    -> Dict String (Dict VarName (List PackageName))
addName packageName moduleName name acc =
    Dict.update moduleName
        (\maybeInner ->
            Maybe.withDefault Dict.empty maybeInner
                |> Dict.update name
                    (\maybeOwners -> Just (Maybe.withDefault [] maybeOwners ++ [ packageName ]))
                |> Just
        )
        acc


ownersOf : Dict String (Dict VarName (List PackageName)) -> String -> VarName -> List PackageName
ownersOf index moduleNameStr name =
    Dict.get moduleNameStr index
        |> Maybe.andThen (Dict.get name)
        |> Maybe.withDefault []


emptyIndex : Index
emptyIndex =
    Index { values = Dict.empty, types = Dict.empty }


{-| We have roughly these options:

  - bar = >baz< (baz being defined elsewhere in this module)
  - import Foo exposing (baz); bar = >baz< (explicit unqualified)
  - something implicitly imported from `elm/core` prelude (`Just`, `+`, ...)
    -- explicit and implicit unqualified share one candidate set: Elm reports
    -- `AMBIGUOUS NAME` across the boundary (e.g. `Basics.identity` vs
    -- `Foo.identity`), so they must be merged, not tried in order.
  - import Foo; bar = >Foo.baz< (qualified, needs an unaliased import)
  - import Foo as F; bar = >F.baz< (aliased; `Cmd`/`Sub` also come from the
    implicit `Platform.Cmd as Cmd` / `Platform.Sub as Sub`)

In all these cases we need to find the package and full unaliased module name
of the var.

-}
moduleOfVar :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> Result ErrorDetails (Maybe ( PackageName, FullModuleName ))
moduleOfVar index modules thisModule maybeModuleName varName =
    Result.ExtraExtra.firstJustLazy
        [ \() -> unqualifiedVarInThisModule thisModule maybeModuleName varName
        , \() -> unqualifiedVarOutsideThisModule index modules thisModule maybeModuleName varName
        , \() -> qualifiedVar index modules thisModule maybeModuleName varName
        ]


findModuleOfVar :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> TIState ( PackageName, FullModuleName )
findModuleOfVar index modules thisModule maybeModuleName varName =
    case moduleOfVar index modules thisModule maybeModuleName varName of
        Err details ->
            State.error
                { moduleName = FullModuleName.toModuleName thisModule.moduleName
                , declarationNames = []
                , details = details
                }

        Ok Nothing ->
            let
                moduleName : ModuleName
                moduleName =
                    FullModuleName.toModuleName thisModule.moduleName
            in
            State.error
                { moduleName = moduleName
                , declarationNames = []
                , details =
                    VarNotFound
                        { varName = varName
                        , usedIn = moduleName
                        }
                }

        Ok (Just result) ->
            State.pure result


{-| `infix left 6 (+) = add` only gives unqualified `add`.
`add` could be defined in this module, or perhaps imported? (I didn't check what
the compiler allows as defining operators is pretty niche functionality only
reserved for elm/\* packages).
-}
resolveOperatorFunction :
    Dict FullModuleName ModuleIndex
    -> FullModuleName
    -> VarName
    -> Result ErrorDetails (Maybe ( FullModuleName, VarName ))
resolveOperatorFunction modules operatorModuleName operator =
    case Dict.get operatorModuleName modules of
        Nothing ->
            Ok Nothing

        Just operatorModule ->
            case Dict.get operator operatorModule.infixes of
                Nothing ->
                    Ok Nothing

                Just functionName ->
                    moduleOfVar emptyIndex modules operatorModule Nothing functionName
                        |> Result.map (Maybe.map (\( _, functionModuleName ) -> ( functionModuleName, functionName )))


unqualifiedVarInThisModule :
    ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> Result ErrorDetails (Maybe ( PackageName, FullModuleName ))
unqualifiedVarInThisModule thisModule maybeModuleName varName =
    Ok <|
        if maybeModuleName == Nothing && Set.member varName thisModule.declaredValues then
            Just ( "", thisModule.moduleName )

        else
            Nothing


{-| Unqualified lookup outside this module.

Explicit imports and the implicit `elm/core` prelude share one candidate set:
Elm reports `AMBIGUOUS NAME` across the boundary (e.g. an explicit
`import Foo exposing (identity)` clashes with the implicit
`Basics.identity`), so trying explicit first and falling back to implicit
would silently pick the wrong module. A repeated module (explicit
`import Basics` + implicit `Basics`) counts once.

-}
unqualifiedVarOutsideThisModule :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> Result ErrorDetails (Maybe ( PackageName, FullModuleName ))
unqualifiedVarOutsideThisModule index modules thisModule maybeModuleName varName =
    if maybeModuleName /= Nothing then
        -- we don't care about qualified vars in this function
        Ok Nothing

    else
        Result.Extra.combineMap
            (\import_ -> explicitImportDefinesValue index modules import_ varName)
            (List.filter (\import_ -> ModuleIndex.importCouldExposeValue import_ varName) thisModule.imports)
            |> Result.andThen
                (\explicitMatches ->
                    let
                        home =
                            ImplicitImports.implicitValueHome varName
                    in
                    dependencyModuleDefines index (FullModuleName.toString home) varName
                        |> Result.map (Maybe.map (\package -> ( package, home )))
                        |> Result.map
                            (\implicitMatch ->
                                List.filterMap identity explicitMatches
                                    ++ List.filterMap identity [ implicitMatch ]
                            )
                )
            |> Result.andThen
                (\allMatches ->
                    case dedupeOwners allMatches of
                        [] ->
                            Ok Nothing

                        [ single ] ->
                            Ok (Just single)

                        many ->
                            Err <|
                                AmbiguousName
                                    { varName = varName
                                    , usedIn = FullModuleName.toModuleName thisModule.moduleName
                                    , possibleModules = List.map (Tuple.second >> FullModuleName.toModuleName) many
                                    }
                )


explicitImportDefinesValue :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ImportIndex
    -> VarName
    -> Result ErrorDetails (Maybe ( PackageName, FullModuleName ))
explicitImportDefinesValue index modules import_ varName =
    case Dict.get import_.moduleName modules of
        Just importedModule ->
            Ok <|
                if Set.member varName importedModule.exposedValues then
                    Just ( "", import_.moduleName )

                else
                    Nothing

        Nothing ->
            dependencyModuleDefines index import_.dottedModuleName varName
                |> Result.map (Maybe.map (\package -> ( package, import_.moduleName )))


{-| Qualified lookup with Elm's actual scoping rules:

  - A single-segment qualifier resolves through aliases first: every explicit
    `import ... as Q` plus the implicit `Cmd`/`Sub` aliases. If several of
    them define the name it is ambiguous; if exactly one defines it, it wins
    over the literal module name (matching `typeResolverFor`).
  - The literal module name is only a fallback when it is imported unaliased
    or implicitly available (`List`, `Tuple`, ... -- but never a full
    `Platform.Cmd`, which is only implicit via `Cmd`). In particular
    `import Quantity.Interval as Interval` alone never falls back to an
    unrelated `Interval` module, and `Foo.bar` without `import Foo` is
    `VarNotFound`.
  - A multi-segment qualifier has no alias handling and needs an unaliased
    import.

-}
qualifiedVar :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> Result ErrorDetails (Maybe ( PackageName, FullModuleName ))
qualifiedVar index modules thisModule maybeModuleName varName =
    case maybeModuleName of
        Nothing ->
            Ok Nothing

        Just qualifier ->
            case qualifier of
                ( single, [] ) ->
                    let
                        aliasCandidates : List FullModuleName
                        aliasCandidates =
                            dedupeFullModuleNames
                                (ModuleIndex.modulesWithAlias thisModule single
                                    ++ (ImplicitImports.unaliasModule single
                                            |> Maybe.map List.singleton
                                            |> Maybe.withDefault []
                                       )
                                )
                    in
                    Result.Extra.combineMap
                        (\unaliased -> qualifiedModuleDefines index modules unaliased varName)
                        aliasCandidates
                        |> Result.andThen
                            (\aliasMatches ->
                                case dedupeOwners (List.filterMap identity aliasMatches) of
                                    [] ->
                                        let
                                            qualifierModuleName : ModuleName
                                            qualifierModuleName =
                                                FullModuleName.toModuleName qualifier
                                        in
                                        if
                                            ModuleIndex.isImportedUnaliased thisModule qualifierModuleName
                                                || ImplicitImports.isImplicitlyImportedModule qualifierModuleName
                                        then
                                            qualifiedModuleDefines index modules qualifier varName

                                        else
                                            Ok Nothing

                                    [ singleDef ] ->
                                        -- The alias wins even if the literal
                                        -- module also defines the name.
                                        Ok (Just singleDef)

                                    multiple ->
                                        Err <|
                                            AmbiguousName
                                                { varName = varName
                                                , usedIn = FullModuleName.toModuleName thisModule.moduleName
                                                , possibleModules = List.map (Tuple.second >> FullModuleName.toModuleName) multiple
                                                }
                            )

                _ ->
                    if ModuleIndex.isImportedUnaliased thisModule (FullModuleName.toModuleName qualifier) then
                        qualifiedModuleDefines index modules qualifier varName

                    else
                        Ok Nothing


qualifiedModuleDefines :
    Index
    -> Dict FullModuleName ModuleIndex
    -> FullModuleName
    -> VarName
    -> Result ErrorDetails (Maybe ( PackageName, FullModuleName ))
qualifiedModuleDefines index modules moduleName varName =
    case Dict.get moduleName modules of
        Just moduleIndex ->
            Ok <|
                if Set.member varName moduleIndex.declaredValues then
                    Just ( "", moduleName )

                else
                    Nothing

        Nothing ->
            dependencyModuleDefines index (FullModuleName.toString moduleName) varName
                |> Result.map (Maybe.map (\package -> ( package, moduleName )))


dedupeOwners : List ( PackageName, FullModuleName ) -> List ( PackageName, FullModuleName )
dedupeOwners pairs =
    List.foldl
        (\( package, mod ) ( seen, acc ) ->
            let
                key : ( PackageName, String )
                key =
                    ( package, FullModuleName.toString mod )
            in
            if List.member key seen then
                ( seen, acc )

            else
                ( key :: seen, acc ++ [ ( package, mod ) ] )
        )
        ( [], [] )
        pairs
        |> Tuple.second


dedupeFullModuleNames : List FullModuleName -> List FullModuleName
dedupeFullModuleNames names =
    List.foldl
        (\mod ( seen, acc ) ->
            let
                key : String
                key =
                    FullModuleName.toString mod
            in
            if List.member key seen then
                ( seen, acc )

            else
                ( key :: seen, acc ++ [ mod ] )
        )
        ( [], [] )
        names
        |> Tuple.second


dependencyModuleDefines : Index -> String -> VarName -> Result ErrorDetails (Maybe PackageName)
dependencyModuleDefines (Index index) moduleNameStr varName =
    let
        matches : List PackageName
        matches =
            ownersOf index.values moduleNameStr varName
    in
    case matches of
        [] ->
            Ok Nothing

        [ single ] ->
            Ok (Just single)

        _ :: _ :: _ ->
            Err (AmbiguousModuleOwner { moduleName = moduleNameStr, possiblePackages = matches })


isRecordAlias : Elm.Docs.Alias -> Bool
isRecordAlias alias_ =
    case alias_.tipe of
        Elm.Type.Record _ Nothing ->
            True

        _ ->
            False


dependencyModuleDefinesType : Index -> FullModuleName -> VarName -> Maybe ( PackageName, FullModuleName )
dependencyModuleDefinesType (Index index) moduleName typeName =
    ownersOf index.types (FullModuleName.toString moduleName) typeName
        |> List.head
        |> Maybe.map (\packageName -> ( packageName, moduleName ))


implicitTypeModule : ModuleName -> VarName -> Maybe ( PackageName, FullModuleName )
implicitTypeModule qualifier typeName =
    if not (List.isEmpty qualifier) then
        Nothing

    else
        ImplicitImports.moduleExposingType typeName
            |> Maybe.map (Tuple.pair ImplicitImports.elmCorePackage)


{-| A qualifier like `Parser.` can mean two different modules at once:

    import Elm.Parser as Parser
    import Parser

Elm accepts this as long as each individual name is unambiguous, so we can't
just resolve `Parser` to a single module and be done - we have to try the
modules the qualifier could stand for and pick the one that actually declares
the type.

-}
qualifierCandidates : ModuleIndex -> ModuleName -> List ModuleName
qualifierCandidates thisModule qualifier =
    let
        aliasedModules : List ModuleName
        aliasedModules =
            case qualifier of
                [ single ] ->
                    ModuleIndex.modulesWithAlias thisModule single
                        |> List.map FullModuleName.toModuleName

                _ ->
                    []

        implicitAlias : List ModuleName
        implicitAlias =
            case qualifier of
                [ single ] ->
                    case ImplicitImports.unaliasModule single of
                        Just m ->
                            [ FullModuleName.toModuleName m ]

                        Nothing ->
                            []

                _ ->
                    []

        aliasCandidates : List ModuleName
        aliasCandidates =
            List.foldl
                (\candidate acc ->
                    if List.member candidate acc then
                        acc

                    else
                        acc ++ [ candidate ]
                )
                []
                (explicitAliases ++ implicitAlias)

        literalAvailable : Bool
        literalAvailable =
            -- Unqualified lookup always runs: it checks local declarations,
            -- then explicit imports (last wins), then the implicit prelude.
            List.isEmpty qualifier
                || ModuleIndex.isImportedUnaliased thisModule qualifier
                || ImplicitImports.isImplicitlyImportedModule qualifier
    in
    if List.isEmpty aliasCandidates then
        if literalAvailable then
            [ qualifier ]

        else
            []

    else if literalAvailable then
        -- The alias(es) win if one of them declares the type, the literal
        -- module name is the fallback.
        aliasCandidates ++ [ qualifier ]

    else
        aliasCandidates


typeResolverFor : Index -> Dict FullModuleName ModuleIndex -> ModuleIndex -> TypeResolver
typeResolverFor ((Index index) as wrappedIndex) modules thisModule qualifier typeName =
    let
        candidates : List ModuleName
        candidates =
            qualifierCandidates thisModule qualifier

        firstParty : ModuleName -> Maybe ( PackageName, FullModuleName )
        firstParty unaliasedQualifier =
            if List.isEmpty unaliasedQualifier then
                if Set.member typeName thisModule.declaredTypes then
                    Just ( "", thisModule.moduleName )

                else
                    thisModule.imports
                        |> List.filterMap
                            (\import_ ->
                                if not (ModuleIndex.importExposesType import_ typeName) then
                                    Nothing

                                else
                                    case Dict.get import_.moduleName modules of
                                        Just importedModule ->
                                            if Set.member typeName importedModule.exposedTypes then
                                                Just ( "", import_.moduleName )

                                            else
                                                Nothing

                                        Nothing ->
                                            dependencyModuleDefinesType wrappedIndex import_.moduleName typeName
                            )
                        |> List.reverse
                        |> List.head

            else
                let
                    fullName : FullModuleName
                    fullName =
                        FullModuleName.fromModuleName_ unaliasedQualifier
                in
                Dict.get fullName modules
                    |> Maybe.andThen
                        (\moduleIndex ->
                            if Set.member typeName moduleIndex.declaredTypes then
                                Just ( "", fullName )

                            else
                                Nothing
                        )

        dependency : ModuleName -> Result ResolverAmbiguity (Maybe ( PackageName, FullModuleName ))
        dependency unaliasedQualifier =
            if List.isEmpty unaliasedQualifier then
                Ok Nothing

            else
                let
                    dottedQualifier : String
                    dottedQualifier =
                        unaliasedQualifier |> String.join "."

                    matchingPackages : List PackageName
                    matchingPackages =
                        ownersOf index.types dottedQualifier typeName
                in
                case matchingPackages of
                    [] ->
                        Ok Nothing

                    [ single ] ->
                        Ok (Just ( single, FullModuleName.fromDotted dottedQualifier ))

                    _ :: _ :: _ ->
                        Err
                            { moduleName = dottedQualifier
                            , possiblePackages = matchingPackages
                            }

        defaultQualifier : ModuleName
        defaultQualifier =
            candidates
                |> List.head
                |> Maybe.withDefault qualifier
    in
    candidates
        |> List.ExtraExtra.fastConcatMap
            (\candidate ->
                [ \() -> Ok (firstParty candidate)
                , \() -> dependency candidate
                ]
            )
        |> Result.ExtraExtra.firstJustLazy
        |> Result.map
            (\resolved ->
                case resolved of
                    Just found ->
                        Just found

                    Nothing ->
                        implicitTypeModule defaultQualifier typeName
            )
        |> Result.map
            (Maybe.withDefault
                ( ""
                , if List.isEmpty defaultQualifier then
                    thisModule.moduleName

                  else
                    FullModuleName.fromModuleName_ defaultQualifier
                )
            )
