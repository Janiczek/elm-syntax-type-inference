module Elm.Syntax.FileV2 exposing
    ( TypedFile
    , map
    , moduleName
    , toTypeLookupTable
    , toTypeLookupTables
    )

import Dict exposing (Dict)
import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.DeclarationV2 as DeclarationV2 exposing (DeclarationV2)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.NodeV2 as NodeV2 exposing (LocatedNode, TypedMeta)
import TypeLookupTable
import TypeLookupTable.Internal exposing (TypeLookupTable)


type alias TypedFile =
    FileV2 TypedMeta


type alias FileV2 meta =
    { moduleDefinition : LocatedNode Module
    , imports : List (LocatedNode Import)
    , declarations : List (LocatedNode (DeclarationV2 meta))
    , comments : List (LocatedNode Comment)
    }


map : (meta1 -> meta2) -> FileV2 meta1 -> FileV2 meta2
map fn file =
    { moduleDefinition = file.moduleDefinition
    , imports = file.imports
    , comments = file.comments
    , declarations =
        file.declarations
            |> List.map (NodeV2.map (DeclarationV2.map fn))
    }


moduleName : FileV2 meta -> FullModuleName
moduleName file =
    file.moduleDefinition
        |> NodeV2.value
        |> Module.moduleName
        |> FullModuleName.fromModuleName_


toTypeLookupTables : Dict FullModuleName TypedFile -> Dict ModuleName TypeLookupTable
toTypeLookupTables typedFiles =
    typedFiles
        |> Dict.toList
        |> List.map
            (\( fullModuleName, typedFile ) ->
                ( FullModuleName.toModuleName fullModuleName
                , toTypeLookupTable typedFile
                )
            )
        |> Dict.fromList


toTypeLookupTable : FileV2 TypedMeta -> TypeLookupTable
toTypeLookupTable file =
    file.declarations
        |> List.concatMap DeclarationV2.toTypeLookupTable
        |> TypeLookupTable.union
