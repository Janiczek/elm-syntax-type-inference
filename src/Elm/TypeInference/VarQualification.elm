module Elm.TypeInference.VarQualification exposing (findModuleOfVar)

import Dict exposing (Dict)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.File.Extra as File
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.State as State exposing (TIState)
import Maybe.Extra as Maybe
import Result.Extra as Result


{-| We have roughly these options:

  - bar = >baz< (baz being defined elsewhere in this module)
  - import Foo exposing (baz); bar = >baz<
  - import Foo; bar = >Foo.baz<
  - import Foo as F; bar = >F.baz<

In all these cases we need to find the full unaliased module name of the var.

-}
findModuleOfVar :
    Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> TIState FullModuleName
findModuleOfVar files thisFile maybeModuleName varName =
    unqualifiedVarInThisModule thisFile maybeModuleName varName
        |> State.orElse (unqualifiedVarInImportedModule files thisFile maybeModuleName varName)
        |> State.orElse (qualifiedVarInImportedModule files maybeModuleName varName)
        |> State.orElse (qualifiedVarInAliasedModule files thisFile maybeModuleName varName)
        |> State.andThen
            (\maybeFoundName ->
                case maybeFoundName of
                    Nothing ->
                        State.varNotFound
                            { varName = varName
                            , usedIn = File.moduleName thisFile
                            }

                    Just foundName ->
                        State.pure foundName
            )


unqualifiedVarInThisModule :
    File
    -> Maybe FullModuleName
    -> VarName
    -> TIState (Maybe FullModuleName)
unqualifiedVarInThisModule thisFile maybeModuleName varName =
    State.pure <|
        if maybeModuleName == Nothing && File.containsDeclaration varName thisFile then
            Just <| File.moduleName thisFile

        else
            Nothing


unqualifiedVarInImportedModule :
    Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> TIState (Maybe FullModuleName)
unqualifiedVarInImportedModule files thisFile maybeModuleName varName =
    if maybeModuleName /= Nothing then
        -- we don't care about qualified vars in this function
        State.pure Nothing

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
                State.pure Nothing

            [ acceptableImport ] ->
                let
                    importModuleName : ModuleName
                    importModuleName =
                        Node.value acceptableImport.moduleName
                in
                importModuleName
                    |> FullModuleName.fromModuleName
                    |> State.pure

            _ ->
                let
                    usedIn : FullModuleName
                    usedIn =
                        thisFile.moduleDefinition
                            |> Node.value
                            |> Module.moduleName
                            |> FullModuleName.fromModuleName_
                in
                State.ambiguousName
                    { varName = varName
                    , usedIn = usedIn
                    , possibleModules =
                        acceptableImports
                            |> List.map (.moduleName >> Node.value >> FullModuleName.fromModuleName_)
                    }


{-| We don't think about module `as` aliasing here.
-}
qualifiedVarInImportedModule :
    Dict FullModuleName File
    -> Maybe FullModuleName
    -> VarName
    -> TIState (Maybe FullModuleName)
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
        |> State.pure


qualifiedVarInAliasedModule :
    Dict FullModuleName File
    -> File
    -> Maybe FullModuleName
    -> VarName
    -> TIState (Maybe FullModuleName)
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
