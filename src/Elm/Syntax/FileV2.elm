module Elm.Syntax.FileV2 exposing (TypedFile, map, moduleName)

import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.DeclarationV2 as DeclarationV2 exposing (DeclarationV2)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.NodeV2 as NodeV2 exposing (LocatedNode, TypedMeta)


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
