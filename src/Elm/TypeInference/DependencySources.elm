module Elm.TypeInference.DependencySources exposing (aliases, neededPackages)

{-| Get type alias bodies from dependency source files.
We need the alias bodies to know if they're records or unions, for type inference later.
-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node
import Elm.Type
import Elm.TypeInference.Dependencies as Dependencies exposing (Dependencies)
import Elm.TypeInference.Error exposing (Error)
import Elm.TypeInference.ModuleIndex as ModuleIndex exposing (ModuleIndex)
import Elm.TypeInference.ModuleLookup as ModuleLookup
import Elm.TypeInference.State exposing (GlobalKey)
import Elm.TypeInference.Type exposing (PackageName)
import Elm.TypeInference.Type.Internal as TypeI
import Elm.TypeInference.TypeVar as TypeVar
import Elm.TypeInference.Unify exposing (TypeAlias)
import List.ExtraExtra
import Result.Extra
import Set exposing (Set)


aliases : Dependencies -> Dict PackageName (List File) -> Result Error (Dict GlobalKey TypeAlias)
aliases deps sources =
    sources
        |> Dict.toList
        |> Result.Extra.combineMap (\( package, files ) -> packageAliases deps package files)
        |> Result.map (List.foldl Dict.union Dict.empty)


{-| Which packages' `docs.json` types use unknown modules, or types that
aren't exposed (eg. a `type alias` used in an exposed function's signature,
but not itself in the module's `exposing` list)?

docs.json can't tell us the underlying (record) shape of such a type, so we
need the actual source to know whether it's a record we can unify
structurally against.

-}
neededPackages : Dependencies -> Dict PackageName (List File) -> List PackageName
neededPackages deps sources =
    let
        docsTypes : Dict String (Set String)
        docsTypes =
            deps
                |> Dict.values
                |> List.ExtraExtra.fastConcatMap .modules
                |> List.map (\mod -> ( mod.name, documentedTypeNames mod ))
                |> Dict.fromList
    in
    deps
        |> Dict.toList
        |> List.filterMap
            (\( package, pkg ) ->
                if Dict.member package sources then
                    Nothing

                else if List.any (not << isKnownRef docsTypes) (docsModuleRefs pkg.modules) then
                    Just package

                else
                    Nothing
            )


documentedTypeNames : Elm.Docs.Module -> Set String
documentedTypeNames mod =
    Set.fromList
        (List.map .name mod.unions ++ List.map .name mod.aliases)


isKnownRef : Dict String (Set String) -> ( String, String ) -> Bool
isKnownRef docsTypes ( moduleName, typeName ) =
    case Dict.get moduleName docsTypes of
        Nothing ->
            False

        Just typeNames ->
            Set.member typeName typeNames


docsModuleRefs : List Elm.Docs.Module -> List ( String, String )
docsModuleRefs modules =
    modules
        |> List.ExtraExtra.fastConcatMap
            (\mod ->
                List.ExtraExtra.fastConcatMap (.tipe >> docsTypeRefs) mod.values
                    ++ List.ExtraExtra.fastConcatMap (.tipe >> docsTypeRefs) mod.binops
                    ++ List.ExtraExtra.fastConcatMap (\union -> List.ExtraExtra.fastConcatMap (Tuple.second >> List.ExtraExtra.fastConcatMap docsTypeRefs) union.tags) mod.unions
                    ++ List.ExtraExtra.fastConcatMap (.tipe >> docsTypeRefs) mod.aliases
            )


docsTypeRefs : Elm.Type.Type -> List ( String, String )
docsTypeRefs tipe =
    case tipe of
        Elm.Type.Var _ ->
            []

        Elm.Type.Lambda from to ->
            docsTypeRefs from ++ docsTypeRefs to

        Elm.Type.Tuple parts ->
            List.ExtraExtra.fastConcatMap docsTypeRefs parts

        Elm.Type.Type qualifiedName args ->
            let
                ( moduleName, typeName ) =
                    Dependencies.splitLastDot qualifiedName
            in
            -- Skip elm/core stuff
            (if isPrimitiveRef moduleName typeName then
                []

             else
                modulePart qualifiedName
                    |> List.map (\m -> ( m, typeName ))
            )
                ++ List.ExtraExtra.fastConcatMap docsTypeRefs args

        Elm.Type.Record fields _ ->
            List.ExtraExtra.fastConcatMap (Tuple.second >> docsTypeRefs) fields


isPrimitiveRef : String -> String -> Bool
isPrimitiveRef moduleName typeName =
    case ( moduleName, typeName ) of
        ( "Basics", "Int" ) ->
            True

        ( "Basics", "Float" ) ->
            True

        ( "Basics", "Bool" ) ->
            True

        ( "Char", "Char" ) ->
            True

        ( "String", "String" ) ->
            True

        ( "List", "List" ) ->
            True

        _ ->
            False


{-|

     "Platform.Cmd.Cmd"
     --> ["Platform.Cmd"]

     "Int"
     --> []

-}
modulePart : String -> List String
modulePart qualifiedName =
    case Dependencies.splitLastDot qualifiedName of
        ( "", _ ) ->
            []

        ( moduleName, _ ) ->
            [ moduleName ]


packageAliases : Dependencies -> PackageName -> List File -> Result Error (Dict GlobalKey TypeAlias)
packageAliases deps package files =
    let
        modules : Dict FullModuleName ModuleIndex
        modules =
            files
                |> List.map
                    (\file ->
                        let
                            moduleIndex : ModuleIndex
                            moduleIndex =
                                ModuleIndex.fromFile file
                        in
                        ( moduleIndex.moduleName, moduleIndex )
                    )
                |> Dict.fromList

        visiblePackages : List PackageName
        visiblePackages =
            package :: (Dict.get package deps |> Maybe.map .dependencies |> Maybe.withDefault [])

        index : ModuleLookup.Index
        index =
            deps
                |> Dict.filter (\name _ -> List.member name visiblePackages)
                |> ModuleLookup.buildIndex
    in
    files
        |> Result.Extra.combineMap
            (\file ->
                let
                    thisModule : ModuleIndex
                    thisModule =
                        ModuleIndex.fromFile file

                    resolver : TypeI.TypeResolver
                    resolver qualifier name =
                        ModuleLookup.typeResolverFor index modules thisModule qualifier name
                            |> Result.map
                                (\( owner, moduleName ) ->
                                    ( if owner == "" then
                                        package

                                      else
                                        owner
                                    , moduleName
                                    )
                                )
                in
                file.declarations
                    |> List.filterMap
                        (\node ->
                            case Node.value node of
                                Declaration.AliasDeclaration alias_ ->
                                    Just
                                        (TypeI.fromTypeAnnotation resolver (Node.value alias_.typeAnnotation)
                                            |> Result.mapError
                                                (\err ->
                                                    { moduleName = String.split "." thisModule.dottedModuleName
                                                    , declarationNames = [ Node.value alias_.name ]
                                                    , details = TypeI.fromTypeAnnotationError err
                                                    }
                                                )
                                            |> Result.map
                                                (\body ->
                                                    ( ( package, thisModule.moduleName, Node.value alias_.name )
                                                    , { args = List.map (Node.value >> TypeVar.parse) alias_.generics
                                                      , type_ = body
                                                      }
                                                    )
                                                )
                                        )

                                _ ->
                                    Nothing
                        )
                    |> Result.Extra.combine
                    |> Result.map Dict.fromList
            )
        |> Result.map (List.foldl Dict.union Dict.empty)
