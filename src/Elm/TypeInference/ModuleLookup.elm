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
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.ImplicitImports as ImplicitImports
import Elm.TypeInference.ModuleIndex as ModuleIndex exposing (ImportIndex, ModuleIndex)
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.Type exposing (PackageName)
import Elm.TypeInference.Type.Internal as TypeI exposing (TypeResolver)
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
  - import Foo exposing (baz); bar = >baz<
  - import Foo; bar = >Foo.baz<
  - import Foo as F; bar = >F.baz<
  - something implicitly imported from `elm/core` prelude (`Just`, `+`, ...)

In all these cases we need to find the package and full unaliased module name
of the var.

-}
moduleOfVar :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe ( PackageName, FullModuleName ))
moduleOfVar index modules thisModule maybeModuleName varName =
    Result.ExtraExtra.firstJustLazy
        [ \() -> unqualifiedVarInThisModule thisModule maybeModuleName varName
        , \() -> unqualifiedVarInImportedModule index modules thisModule maybeModuleName varName
        , -- Aliases must be resolved before a bare qualifier lookup: otherwise
          -- `import Quantity.Interval as Interval` would let a qualified
          -- `Interval.from` resolve to an unrelated, unimported `Interval`
          -- module that happens to share the alias's name.
          \() -> qualifiedVarInAliasedModule index modules thisModule maybeModuleName varName
        , \() -> qualifiedVarInImportedModule index modules maybeModuleName varName
        , \() -> implicitUnqualifiedValue index thisModule maybeModuleName varName
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
        Err err ->
            State.error err

        Ok Nothing ->
            State.error <|
                VarNotFound
                    { varName = varName
                    , usedIn = thisModule.moduleName
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
    -> Result Error (Maybe ( FullModuleName, VarName ))
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
    -> Result Error (Maybe ( PackageName, FullModuleName ))
unqualifiedVarInThisModule thisModule maybeModuleName varName =
    Ok <|
        if maybeModuleName == Nothing && Set.member varName thisModule.declaredValues then
            Just ( "", thisModule.moduleName )

        else
            Nothing


unqualifiedVarInImportedModule :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe ( PackageName, FullModuleName ))
unqualifiedVarInImportedModule index modules thisModule maybeModuleName varName =
    if maybeModuleName /= Nothing then
        -- we don't care about qualified vars in this function
        Ok Nothing

    else
        let
            importDefinesValue : ImportIndex -> Result Error Bool
            importDefinesValue import_ =
                case Dict.get import_.moduleName modules of
                    Just importedModule ->
                        Ok (Set.member varName importedModule.exposedValues)

                    Nothing ->
                        dependencyModuleDefines index import_.dottedModuleName varName
                            |> Result.map ((/=) Nothing)

            acceptableImports : Result Error (List ImportIndex)
            acceptableImports =
                thisModule.imports
                    |> List.filter (\import_ -> ModuleIndex.importCouldExposeValue import_ varName)
                    |> Result.ExtraExtra.combineFilter importDefinesValue
        in
        acceptableImports
            |> Result.andThen
                (\imports ->
                    case imports of
                        [] ->
                            Ok Nothing

                        [ acceptableImport ] ->
                            if Dict.member acceptableImport.moduleName modules then
                                Ok (Just ( "", acceptableImport.moduleName ))

                            else
                                dependencyModuleDefines index acceptableImport.dottedModuleName varName
                                    |> Result.map (Maybe.map (\package -> ( package, acceptableImport.moduleName )))

                        _ ->
                            Err <|
                                AmbiguousName
                                    { varName = varName
                                    , usedIn = thisModule.moduleName
                                    , possibleModules = List.map .moduleName imports
                                    }
                )


{-| We don't think about module `as` aliasing here.
-}
qualifiedVarInImportedModule :
    Index
    -> Dict FullModuleName ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe ( PackageName, FullModuleName ))
qualifiedVarInImportedModule index modules maybeModuleName varName =
    case maybeModuleName of
        Nothing ->
            Ok Nothing

        Just moduleName ->
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


qualifiedVarInAliasedModule :
    Index
    -> Dict FullModuleName ModuleIndex
    -> ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe ( PackageName, FullModuleName ))
qualifiedVarInAliasedModule index modules thisModule maybeModuleName varName =
    let
        {- The same alias can be given to more than one import (e.g.
           `import Svg as S` and `import Internal.Svg as S`), so a qualified
           reference like `S.Gradient` might belong to any of them. Try every
           aliased candidate, in import order, and use whichever one actually
           defines the name.
        -}
        unaliasedModuleNames : List FullModuleName
        unaliasedModuleNames =
            case maybeModuleName of
                Nothing ->
                    []

                Just ( single, [] ) ->
                    case ModuleIndex.modulesWithAlias thisModule single of
                        [] ->
                            ImplicitImports.unaliasModule single
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault []

                        found ->
                            found

                Just _ ->
                    []
    in
    unaliasedModuleNames
        |> List.map
            (\unaliasedModuleName () ->
                qualifiedVarInImportedModule
                    index
                    modules
                    (Just unaliasedModuleName)
                    varName
            )
        |> Result.ExtraExtra.firstJustLazy


implicitUnqualifiedValue :
    Index
    -> ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe ( PackageName, FullModuleName ))
implicitUnqualifiedValue index thisModule maybeModuleName varName =
    if maybeModuleName /= Nothing then
        Ok Nothing

    else
        ImplicitImports.modulesPossiblyExposingValue varName
            |> Result.Extra.combineMap
                (\moduleNameStr ->
                    dependencyModuleDefines index moduleNameStr varName
                        |> Result.map (Maybe.map (\package -> ( package, FullModuleName.fromDotted moduleNameStr )))
                )
            |> Result.andThen
                (\matches ->
                    case List.filterMap identity matches of
                        [] ->
                            Ok Nothing

                        [ single ] ->
                            Ok (Just single)

                        many ->
                            Err
                                (AmbiguousName
                                    { usedIn = thisModule.moduleName
                                    , varName = varName
                                    , possibleModules = List.map Tuple.second many
                                    }
                                )
                )


dependencyModuleDefines : Index -> String -> VarName -> Result Error (Maybe PackageName)
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

        implicitAliasedModule : List ModuleName
        implicitAliasedModule =
            case ( qualifier, aliasedModules ) of
                ( [ single ], [] ) ->
                    case ImplicitImports.unaliasModule single of
                        Just m ->
                            [ FullModuleName.toModuleName m ]

                        Nothing ->
                            []

                _ ->
                    []

        aliasCandidates : List ModuleName
        aliasCandidates =
            aliasedModules ++ implicitAliasedModule
    in
    if List.isEmpty aliasCandidates then
        [ qualifier ]

    else
        let
            isImportedUnaliased : Bool
            isImportedUnaliased =
                ModuleIndex.isImportedUnaliased thisModule qualifier
        in
        if isImportedUnaliased then
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
                                case Dict.get import_.moduleName modules of
                                    Just importedModule ->
                                        if Set.member typeName importedModule.exposedTypes then
                                            Just ( "", import_.moduleName )

                                        else
                                            Nothing

                                    Nothing ->
                                        if ModuleIndex.importExposesType import_ typeName then
                                            dependencyModuleDefinesType wrappedIndex import_.moduleName typeName

                                        else
                                            Nothing
                            )
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

        dependency : ModuleName -> Result TypeI.ResolverAmbiguity (Maybe ( PackageName, FullModuleName ))
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
