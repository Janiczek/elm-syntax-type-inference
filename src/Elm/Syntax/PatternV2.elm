module Elm.Syntax.PatternV2 exposing
    ( LocatedPattern
    , PatternV2(..)
    , TypedPattern
    )

import Elm.Syntax.NodeV2 as NodeV2 exposing (LocatedNode, NodeV2(..))
import Elm.Syntax.Pattern as Pattern exposing (QualifiedNameRef)
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.Type exposing (TypeOrId)


type alias LocatedPattern =
    PatternWith { range : Range }


type alias TypedPattern =
    PatternWith { range : Range, type_ : TypeOrId }


type alias PatternWith meta =
    NodeV2 meta (PatternV2 meta)


type PatternV2 meta
    = AllPattern
    | UnitPattern
    | CharPattern Char
    | StringPattern String
    | IntPattern Int
    | HexPattern Int
    | FloatPattern Float
    | TuplePattern (List (PatternWith meta))
    | RecordPattern (List (LocatedNode String))
    | UnConsPattern (PatternWith meta) (PatternWith meta)
    | ListPattern (List (PatternWith meta))
    | VarPattern String
    | NamedPattern QualifiedNameRef (List (PatternWith meta))
    | AsPattern (PatternWith meta) (LocatedNode String)
    | ParenthesizedPattern (PatternWith meta)
