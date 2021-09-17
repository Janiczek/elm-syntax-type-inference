module Elm.TypeInference.State.VarModuleLookup exposing
    ( findModuleOfVar
    , moduleOfVar
    )

-- TODO rename this to DeclModuleLookup? It's not really about vars inside exprs...

import Dict exposing (Dict)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.File.Extra as File
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.State as State exposing (TIState)


{-| We have roughly these options:

  - bar = >baz< (baz being defined elsewhere in this module)
  - import Foo exposing (baz); bar = >baz<
  - import Foo; bar = >Foo.baz<
  - import Foo as F; bar = >F.baz<

In all these cases we need to find the full unaliased module name of the var.

-}
moduleOfVar :
    Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe FullModuleName)
moduleOfVar files thisFile maybeModuleName varName =
    let
        orElseLazy : (() -> Result Error (Maybe FullModuleName)) -> Result Error (Maybe FullModuleName) -> Result Error (Maybe FullModuleName)
        orElseLazy after before =
            case before of
                Err err ->
                    Err err

                Ok (Just name) ->
                    Ok (Just name)

                Ok Nothing ->
                    after ()
    in
    unqualifiedVarInThisModule thisFile maybeModuleName varName
        |> orElseLazy (\() -> unqualifiedVarInImportedModule files thisFile maybeModuleName varName)
        |> orElseLazy (\() -> qualifiedVarInImportedModule files maybeModuleName varName)
        |> orElseLazy (\() -> qualifiedVarInAliasedModule files thisFile maybeModuleName varName)


findModuleOfVar :
    Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> TIState FullModuleName
findModuleOfVar files thisFile maybeModuleName varName =
    case moduleOfVar files thisFile maybeModuleName varName of
        Err err ->
            State.error err

        Ok Nothing ->
            State.error <|
                VarNotFound
                    { varName = varName
                    , usedIn = File.moduleName thisFile
                    }

        Ok (Just moduleName) ->
            State.pure moduleName


unqualifiedVarInThisModule :
    File
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe FullModuleName)
unqualifiedVarInThisModule thisFile maybeModuleName varName =
    Ok <|
        if maybeModuleName == Nothing && File.containsDeclaration varName thisFile then
            Just <| File.moduleName thisFile

        else
            Nothing


unqualifiedVarInImportedModule :
    Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe FullModuleName)
unqualifiedVarInImportedModule files thisFile maybeModuleName varName =
    if maybeModuleName /= Nothing then
        -- we don't care about qualified vars in this function
        Ok Nothing

    else
        -- find a module which exposes that var
        let
            acceptableImports : List Import
            acceptableImports =
                thisFile.imports
                    |> List.filter
                        (\importNode ->
                            let
                                import_ : Import
                                import_ =
                                    Node.value importNode

                                importName : FullModuleName
                                importName =
                                    import_.moduleName
                                        |> Node.value
                                        |> FullModuleName.fromModuleName_
                            in
                            files
                                |> Dict.get importName
                                |> Maybe.map (File.exposes varName)
                                |> Maybe.withDefault False
                        )
                    |> List.map Node.value
        in
        case acceptableImports of
            [] ->
                Ok Nothing

            [ acceptableImport ] ->
                let
                    importModuleName : ModuleName
                    importModuleName =
                        Node.value acceptableImport.moduleName
                in
                importModuleName
                    |> FullModuleName.fromModuleName
                    |> Ok

            _ ->
                Err <|
                    AmbiguousName
                        { varName = varName
                        , usedIn = File.moduleName thisFile
                        , possibleModules =
                            acceptableImports
                                |> List.map
                                    (.moduleName
                                        >> Node.value
                                        >> FullModuleName.fromModuleName_
                                    )
                        }


{-| We don't think about module `as` aliasing here.
-}
qualifiedVarInImportedModule :
    Dict FullModuleName File
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe FullModuleName)
qualifiedVarInImportedModule files maybeModuleName varName =
    maybeModuleName
        |> Maybe.andThen (\moduleName -> Dict.get moduleName files)
        |> Maybe.andThen
            (\file ->
                if File.containsDeclaration varName file then
                    Just <| File.moduleName file

                else
                    Nothing
            )
        |> Ok


qualifiedVarInAliasedModule :
    Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> Result Error (Maybe FullModuleName)
qualifiedVarInAliasedModule files thisFile maybeModuleName varName =
    let
        unaliasedModuleName : Maybe FullModuleName
        unaliasedModuleName =
            case maybeModuleName of
                Nothing ->
                    Nothing

                Just ( single, [] ) ->
                    File.unalias thisFile single

                Just _ ->
                    Nothing
    in
    qualifiedVarInImportedModule
        files
        unaliasedModuleName
        varName
