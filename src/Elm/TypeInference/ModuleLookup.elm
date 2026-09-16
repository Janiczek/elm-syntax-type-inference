module Elm.TypeInference.ModuleLookup exposing
    ( Index
    , buildIndex
    , findModuleOfVar
    , moduleOfVar
    , resolveOperatorFunction
    , typeResolverFor
    )

{-| -}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Type
import Elm.TypeInference.Dependencies exposing (Dependencies)
import Elm.TypeInference.Error exposing (ErrorDetails(..))
import Elm.TypeInference.Error.Internal exposing (ResolverAmbiguity)
import Elm.TypeInference.ImplicitImports as ImplicitImports
import Elm.TypeInference.ModuleIndex as ModuleIndex exposing (ImportIndex, ModuleIndex)
import Elm.TypeInference.State as State exposing (StateM)
import Elm.TypeInference.Type exposing (PackageName, VarName)
import Elm.TypeInference.Type.Internal exposing (TypeResolver)
import List.ExtraExtra
import Result.Extra
import Result.ExtraExtra
import Set


{-| Precomputed index of `module name -> value/type name -> packages defining it`.
-}
type Index
    = Index
        { values : NameIndex
        , types : NameIndex
        }


type alias NameIndex =
    Dict String (Dict VarName (List PackageName))


buildIndex : Dependencies -> Index
buildIndex deps =
    Dict.foldl
        (\packageName pkg acc ->
            List.foldl (addModule packageName) acc pkg.modules
        )
        emptyIndex
        deps


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
        ++ List.filterMap
            (\a ->
                if isRecordAlias a then
                    Just a.name

                else
                    Nothing
            )
            mod.aliases


typeNamesOf : Elm.Docs.Module -> List VarName
typeNamesOf mod =
    List.map .name mod.unions ++ List.map .name mod.aliases


addName :
    PackageName
    -> String
    -> VarName
    -> NameIndex
    -> NameIndex
addName packageName moduleName name acc =
    Dict.update moduleName
        (\maybeInner ->
            Maybe.withDefault Dict.empty maybeInner
                |> Dict.update name
                    (\maybeOwners -> Just (Maybe.withDefault [] maybeOwners ++ [ packageName ]))
                |> Just
        )
        acc


ownersOf : NameIndex -> FullModuleName -> VarName -> List PackageName
ownersOf index moduleName name =
    Dict.get (FullModuleName.toString moduleName) index
        |> Maybe.andThen (Dict.get name)
        |> Maybe.withDefault []


emptyIndex : Index
emptyIndex =
    Index
        { values = Dict.empty
        , types = Dict.empty
        }


{-| Find the package and full unaliased module name of the var.
-}
moduleOfVar :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> Result ErrorDetails (Maybe ( PackageName, FullModuleName ))
moduleOfVar index modules thisModule maybeModuleName varName =
    case maybeModuleName of
        Nothing ->
            Result.ExtraExtra.firstJustLazy
                [ \() -> unqualifiedVarInThisModule thisModule varName
                , \() -> unqualifiedVarOutsideThisModule index modules thisModule varName
                ]

        Just qualifier ->
            qualifiedVar index modules thisModule qualifier varName


{-| StateM wrapper around moduleOfVar
-}
findModuleOfVar :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> StateM ( PackageName, FullModuleName )
findModuleOfVar index modules thisModule maybeModuleName varName =
    case moduleOfVar index modules thisModule maybeModuleName varName of
        Ok (Just result) ->
            State.pure result

        Ok Nothing ->
            -- Var not found
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

        Err details ->
            State.error
                { moduleName = FullModuleName.toModuleName thisModule.moduleName
                , declarationNames = []
                , details = details
                }


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
    -> VarName
    -> Result ErrorDetails (Maybe ( PackageName, FullModuleName ))
unqualifiedVarInThisModule thisModule varName =
    Ok <|
        if Set.member varName thisModule.declaredValues then
            Just ( "", thisModule.moduleName )

        else
            Nothing


unqualifiedVarOutsideThisModule :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ModuleIndex
    -> VarName
    -> Result ErrorDetails (Maybe ( PackageName, FullModuleName ))
unqualifiedVarOutsideThisModule index modules thisModule varName =
    Result.Extra.combineMap
        (\import_ -> explicitImportDefinesValue index modules import_ varName)
        (List.filter (\import_ -> ModuleIndex.importCouldExposeValue import_ varName) thisModule.imports)
        |> Result.andThen
            (\explicitMatches ->
                let
                    home : FullModuleName
                    home =
                        ImplicitImports.implicitValueHome varName
                in
                dependencyModuleDefines index home varName
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
            dependencyModuleDefines index import_.moduleName varName
                |> Result.map (Maybe.map (\package -> ( package, import_.moduleName )))


qualifiedVar :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ModuleIndex
    -> FullModuleName
    -> VarName
    -> Result ErrorDetails (Maybe ( PackageName, FullModuleName ))
qualifiedVar index modules thisModule qualifier varName =
    case qualifier of
        ( single, [] ) ->
            let
                aliasCandidates : List FullModuleName
                aliasCandidates =
                    dedupeFullModuleNames
                        (ModuleIndex.modulesWithAlias thisModule single
                            ++ (case ImplicitImports.unaliasModule single of
                                    Just m ->
                                        [ m ]

                                    Nothing ->
                                        []
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
            dependencyModuleDefines index moduleName varName
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


dependencyModuleDefines : Index -> FullModuleName -> VarName -> Result ErrorDetails (Maybe PackageName)
dependencyModuleDefines (Index index) moduleName varName =
    let
        matches : List PackageName
        matches =
            ownersOf index.values moduleName varName
    in
    case matches of
        [] ->
            Ok Nothing

        [ single ] ->
            Ok (Just single)

        _ :: _ :: _ ->
            Err (AmbiguousModuleOwner { moduleName = FullModuleName.toString moduleName, possiblePackages = matches })


isRecordAlias : Elm.Docs.Alias -> Bool
isRecordAlias alias_ =
    case alias_.tipe of
        Elm.Type.Record _ Nothing ->
            True

        _ ->
            False


dependencyModuleDefinesType : Index -> FullModuleName -> VarName -> Maybe ( PackageName, FullModuleName )
dependencyModuleDefinesType (Index index) moduleName typeName =
    ownersOf index.types moduleName typeName
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
                (aliasedModules ++ implicitAlias)

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
                    fullName : FullModuleName
                    fullName =
                        FullModuleName.fromModuleName_ unaliasedQualifier

                    matchingPackages : List PackageName
                    matchingPackages =
                        ownersOf index.types fullName typeName
                in
                case matchingPackages of
                    [] ->
                        Ok Nothing

                    [ single ] ->
                        Ok (Just ( single, fullName ))

                    _ :: _ :: _ ->
                        Err
                            { moduleName = FullModuleName.toString fullName
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
