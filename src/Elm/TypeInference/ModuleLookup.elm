module Elm.TypeInference.ModuleLookup exposing
    ( findModuleOfVar
    , moduleOfVar
    , resolveOperatorFunction
    , typeResolverFor
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.File exposing (File)
import Elm.Syntax.File.Extra
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.VarName exposing (VarName)
import Elm.Type
import Elm.TypeInference.Dependencies exposing (Dependencies)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.ImplicitImports as ImplicitImports
import Elm.TypeInference.State as State exposing (PackageName, TIState)
import Elm.TypeInference.Type as Type exposing (TypeResolver)
import Maybe.Extra
import Result.Extra
import Result.ExtraExtra


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
    Dependencies
    -> Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe ( PackageName, FullModuleName ))
moduleOfVar deps files thisFile maybeModuleName varName =
    Result.ExtraExtra.firstJustLazy
        [ \() -> unqualifiedVarInThisModule thisFile maybeModuleName varName
        , \() -> unqualifiedVarInImportedModule deps files thisFile maybeModuleName varName
        , \() -> qualifiedVarInImportedModule deps files maybeModuleName varName
        , \() -> qualifiedVarInAliasedModule deps files thisFile maybeModuleName varName
        , \() -> implicitUnqualifiedValue deps maybeModuleName varName
        ]


findModuleOfVar :
    Dependencies
    -> Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> TIState ( PackageName, FullModuleName )
findModuleOfVar deps files thisFile maybeModuleName varName =
    case moduleOfVar deps files thisFile maybeModuleName varName of
        Err err ->
            State.error err

        Ok Nothing ->
            State.error <|
                VarNotFound
                    { varName = varName
                    , usedIn = Elm.Syntax.File.Extra.moduleName thisFile
                    }

        Ok (Just result) ->
            State.pure result


{-| `infix left 6 (+) = add` only gives unqualified `add`.
`add` could be defined in this module, or perhaps imported? (I didn't check what
the compiler allows as defining operators is pretty niche functionality only
reserved for elm/\* packages).
-}
resolveOperatorFunction :
    Dict FullModuleName File
    -> FullModuleName
    -> VarName
    -> Result Error (Maybe ( FullModuleName, VarName ))
resolveOperatorFunction files operatorModuleName operator =
    case Dict.get operatorModuleName files of
        Nothing ->
            Ok Nothing

        Just operatorFile ->
            case Elm.Syntax.File.Extra.resolveOperatorFunction operator operatorFile of
                Nothing ->
                    Ok Nothing

                Just functionName ->
                    moduleOfVar Dict.empty files operatorFile Nothing functionName
                        |> Result.map (Maybe.map (\( _, functionModuleName ) -> ( functionModuleName, functionName )))


unqualifiedVarInThisModule :
    File
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe ( PackageName, FullModuleName ))
unqualifiedVarInThisModule thisFile maybeModuleName varName =
    Ok <|
        if maybeModuleName == Nothing && Elm.Syntax.File.Extra.containsDeclaration varName thisFile then
            Just ( "", Elm.Syntax.File.Extra.moduleName thisFile )

        else
            Nothing


{-| Does this import's own `exposing` clause name this value/operator?
-}
importExposesValue : Import -> VarName -> Bool
importExposesValue import_ varName =
    import_.exposingList
        |> Maybe.map (Node.value >> Elm.Syntax.File.Extra.exposesInExposing varName)
        |> Maybe.withDefault False


unqualifiedVarInImportedModule :
    Dependencies
    -> Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe ( PackageName, FullModuleName ))
unqualifiedVarInImportedModule deps files thisFile maybeModuleName varName =
    if maybeModuleName /= Nothing then
        -- we don't care about qualified vars in this function
        Ok Nothing

    else
        let
            importDefinesValue : Import -> Result Error Bool
            importDefinesValue import_ =
                let
                    importName : FullModuleName
                    importName =
                        import_.moduleName
                            |> Node.value
                            |> FullModuleName.fromModuleName_
                in
                case Dict.get importName files of
                    Just file ->
                        Ok (Elm.Syntax.File.Extra.exposes varName file)

                    Nothing ->
                        dependencyModuleDefines deps (FullModuleName.toString importName) varName
                            |> Result.map ((/=) Nothing)

            acceptableImports : Result Error (List Import)
            acceptableImports =
                thisFile.imports
                    |> List.map Node.value
                    |> List.filter (\import_ -> importExposesValue import_ varName)
                    |> Result.ExtraExtra.combineFilter importDefinesValue
        in
        acceptableImports
            |> Result.andThen
                (\imports ->
                    case imports of
                        [] ->
                            Ok Nothing

                        [ acceptableImport ] ->
                            let
                                importModuleName : ModuleName
                                importModuleName =
                                    Node.value acceptableImport.moduleName

                                fullName : FullModuleName
                                fullName =
                                    FullModuleName.fromModuleName_ importModuleName
                            in
                            if Dict.member fullName files then
                                Ok (Just ( "", fullName ))

                            else
                                dependencyModuleDefines deps (FullModuleName.toString fullName) varName
                                    |> Result.map (Maybe.map (\package -> ( package, fullName )))

                        _ ->
                            Err <|
                                AmbiguousName
                                    { varName = varName
                                    , usedIn = Elm.Syntax.File.Extra.moduleName thisFile
                                    , possibleModules =
                                        imports
                                            |> List.map
                                                (.moduleName
                                                    >> Node.value
                                                    >> FullModuleName.fromModuleName_
                                                )
                                    }
                )


{-| We don't think about module `as` aliasing here.
-}
qualifiedVarInImportedModule :
    Dependencies
    -> Dict FullModuleName File
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe ( PackageName, FullModuleName ))
qualifiedVarInImportedModule deps files maybeModuleName varName =
    case maybeModuleName of
        Nothing ->
            Ok Nothing

        Just moduleName ->
            case Dict.get moduleName files of
                Just file ->
                    Ok <|
                        if Elm.Syntax.File.Extra.containsDeclaration varName file then
                            Just ( "", moduleName )

                        else
                            Nothing

                Nothing ->
                    dependencyModuleDefines deps (FullModuleName.toString moduleName) varName
                        |> Result.map (Maybe.map (\package -> ( package, moduleName )))


qualifiedVarInAliasedModule :
    Dependencies
    -> Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe ( PackageName, FullModuleName ))
qualifiedVarInAliasedModule deps files thisFile maybeModuleName varName =
    let
        unaliasedModuleName : Maybe FullModuleName
        unaliasedModuleName =
            case maybeModuleName of
                Nothing ->
                    Nothing

                Just ( single, [] ) ->
                    case Elm.Syntax.File.Extra.unalias thisFile single of
                        Just m ->
                            Just m

                        Nothing ->
                            ImplicitImports.unaliasModule single

                Just _ ->
                    Nothing
    in
    qualifiedVarInImportedModule
        deps
        files
        unaliasedModuleName
        varName


implicitUnqualifiedValue :
    Dependencies
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe ( PackageName, FullModuleName ))
implicitUnqualifiedValue deps maybeModuleName varName =
    if maybeModuleName /= Nothing then
        Ok Nothing

    else
        ImplicitImports.modulesPossiblyExposingValue varName
            |> Result.Extra.combineMap
                (\moduleNameStr ->
                    dependencyModuleDefines deps moduleNameStr varName
                        |> Result.map (Maybe.map (\package -> ( package, FullModuleName.fromDotted moduleNameStr )))
                )
            |> Result.map (List.filterMap identity >> List.head)


dependencyModuleDefines : Dependencies -> String -> VarName -> Result Error (Maybe PackageName)
dependencyModuleDefines deps moduleNameStr varName =
    let
        matches : List PackageName
        matches =
            Dict.toList deps
                |> List.filterMap
                    (\( packageName, pkg ) ->
                        pkg.modules
                            |> List.filter (\m -> m.name == moduleNameStr)
                            |> List.head
                            |> Maybe.andThen
                                (\mod ->
                                    if moduleDefinesValue mod varName then
                                        Just packageName

                                    else
                                        Nothing
                                )
                    )
    in
    case matches of
        [] ->
            Ok Nothing

        [ single ] ->
            Ok (Just single)

        _ :: _ :: _ ->
            Err (AmbiguousModuleOwner { moduleName = moduleNameStr, possiblePackages = matches })


moduleDefinesValue : Elm.Docs.Module -> VarName -> Bool
moduleDefinesValue mod varName =
    List.any (\v -> v.name == varName) mod.values
        || List.any (\b -> b.name == varName) mod.binops
        || List.any (\u -> List.any (\( ctor, _ ) -> ctor == varName) u.tags) mod.unions
        || List.any (\a -> a.name == varName && isRecordAlias a) mod.aliases


isRecordAlias : Elm.Docs.Alias -> Bool
isRecordAlias alias_ =
    case alias_.tipe of
        Elm.Type.Record _ Nothing ->
            True

        _ ->
            False


moduleDefinesType : Elm.Docs.Module -> VarName -> Bool
moduleDefinesType mod typeName =
    List.any (\u -> u.name == typeName) mod.unions
        || List.any (\a -> a.name == typeName) mod.aliases


implicitTypeModule : ModuleName -> VarName -> Maybe ( PackageName, FullModuleName )
implicitTypeModule qualifier typeName =
    if not (List.isEmpty qualifier) then
        Nothing

    else
        ImplicitImports.moduleExposingType typeName
            |> Maybe.map (Tuple.pair ImplicitImports.package)


typeResolverFor : Dependencies -> Dict FullModuleName File -> File -> TypeResolver
typeResolverFor deps files thisFile qualifier typeName =
    let
        unaliasedQualifier : ModuleName
        unaliasedQualifier =
            case qualifier of
                [ single ] ->
                    Elm.Syntax.File.Extra.unalias thisFile single
                        |> Maybe.Extra.orElseLazy (\() -> ImplicitImports.unaliasModule single)
                        |> Maybe.map FullModuleName.toModuleName
                        |> Maybe.withDefault qualifier

                _ ->
                    qualifier

        firstParty : Maybe ( PackageName, FullModuleName )
        firstParty =
            if List.isEmpty unaliasedQualifier then
                if Elm.Syntax.File.Extra.containsTypeDeclaration typeName thisFile then
                    Just ( "", Elm.Syntax.File.Extra.moduleName thisFile )

                else
                    thisFile.imports
                        |> List.map Node.value
                        |> List.filterMap
                            (\import_ ->
                                let
                                    importName =
                                        FullModuleName.fromModuleName_ (Node.value import_.moduleName)
                                in
                                Dict.get importName files
                                    |> Maybe.andThen
                                        (\file ->
                                            if Elm.Syntax.File.Extra.exposesType typeName file then
                                                Just ( "", importName )

                                            else
                                                Nothing
                                        )
                            )
                        |> List.head

            else
                let
                    fullName =
                        FullModuleName.fromModuleName_ unaliasedQualifier
                in
                Dict.get fullName files
                    |> Maybe.andThen
                        (\file ->
                            if Elm.Syntax.File.Extra.containsTypeDeclaration typeName file then
                                Just ( "", fullName )

                            else
                                Nothing
                        )

        dependency : Result Type.ResolverAmbiguity (Maybe ( PackageName, FullModuleName ))
        dependency =
            if List.isEmpty unaliasedQualifier then
                Ok Nothing

            else
                let
                    dottedQualifier =
                        unaliasedQualifier |> String.join "."

                    matches : List ( PackageName, FullModuleName )
                    matches =
                        Dict.toList deps
                            |> List.filterMap
                                (\( packageName, pkg ) ->
                                    pkg.modules
                                        |> List.filter (\m -> m.name == dottedQualifier && moduleDefinesType m typeName)
                                        |> List.head
                                        |> Maybe.map (\m -> ( packageName, FullModuleName.fromDotted m.name ))
                                )
                in
                case matches of
                    [] ->
                        Ok Nothing

                    [ single ] ->
                        Ok (Just single)

                    _ :: _ :: _ ->
                        Err
                            { moduleName = dottedQualifier
                            , possiblePackages = matches |> List.map Tuple.first
                            }
    in
    Result.ExtraExtra.firstJustLazy
        [ \() -> Ok firstParty
        , \() -> dependency
        ]
        |> Result.map
            (\resolved ->
                case resolved of
                    Just found ->
                        Just found

                    Nothing ->
                        implicitTypeModule unaliasedQualifier typeName
            )
        |> Result.map
            (Maybe.withDefault
                ( ""
                , if List.isEmpty unaliasedQualifier then
                    Elm.Syntax.File.Extra.moduleName thisFile

                  else
                    FullModuleName.fromModuleName_ unaliasedQualifier
                )
            )
