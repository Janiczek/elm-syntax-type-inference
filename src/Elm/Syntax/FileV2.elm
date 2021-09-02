module Elm.Syntax.FileV2 exposing (TypedFile)

import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.DeclarationV2 exposing (DeclarationV2)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.NodeV2 exposing (LocatedNode, TypedMeta)


type alias TypedFile =
    FileV2 TypedMeta


type alias FileV2 meta =
    { moduleDefinition : LocatedNode Module
    , imports : List (LocatedNode Import)
    , declarations : List (LocatedNode (DeclarationV2 meta))
    , comments : List (LocatedNode Comment)
    }
