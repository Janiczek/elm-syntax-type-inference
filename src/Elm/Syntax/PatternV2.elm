module Elm.Syntax.PatternV2 exposing
    ( LocatedPattern
    , PatternV2(..)
    , PatternWith
    , TypedPattern
    , fromNodePattern
    , map
    )

import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.NodeV2 as NodeV2
    exposing
        ( LocatedMeta
        , LocatedNode
        , NodeV2(..)
        , TypedMeta
        )
import Elm.Syntax.Pattern as Pattern exposing (Pattern, QualifiedNameRef)


type alias LocatedPattern =
    PatternWith LocatedMeta


type alias TypedPattern =
    PatternWith TypedMeta


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


fromNodePattern : Node Pattern -> LocatedPattern
fromNodePattern node =
    let
        range =
            Node.range node

        pattern =
            Node.value node
    in
    NodeV2
        { range = range }
        (fromPattern pattern)


fromPattern : Pattern -> PatternV2 LocatedMeta
fromPattern pattern =
    let
        f =
            fromNodePattern
    in
    case pattern of
        Pattern.AllPattern ->
            AllPattern

        Pattern.UnitPattern ->
            UnitPattern

        Pattern.CharPattern a ->
            CharPattern a

        Pattern.StringPattern a ->
            StringPattern a

        Pattern.IntPattern a ->
            IntPattern a

        Pattern.HexPattern a ->
            HexPattern a

        Pattern.FloatPattern a ->
            FloatPattern a

        Pattern.TuplePattern patterns ->
            TuplePattern <| List.map f patterns

        Pattern.RecordPattern fields ->
            RecordPattern <| List.map NodeV2.fromNode fields

        Pattern.UnConsPattern p1 p2 ->
            UnConsPattern (f p1) (f p2)

        Pattern.ListPattern patterns ->
            ListPattern <| List.map f patterns

        Pattern.VarPattern a ->
            VarPattern a

        Pattern.NamedPattern a patterns ->
            NamedPattern a <| List.map f patterns

        Pattern.AsPattern p1 a ->
            AsPattern (f p1) (NodeV2.fromNode a)

        Pattern.ParenthesizedPattern p1 ->
            ParenthesizedPattern (f p1)


map : (meta1 -> meta2) -> PatternWith meta1 -> PatternWith meta2
map fn (NodeV2 meta pattern) =
    NodeV2 (fn meta) <|
        case pattern of
            AllPattern ->
                AllPattern

            UnitPattern ->
                UnitPattern

            CharPattern a ->
                CharPattern a

            StringPattern a ->
                StringPattern a

            IntPattern a ->
                IntPattern a

            HexPattern a ->
                HexPattern a

            FloatPattern a ->
                FloatPattern a

            TuplePattern patterns ->
                TuplePattern (List.map (map fn) patterns)

            RecordPattern a ->
                RecordPattern a

            UnConsPattern p1 p2 ->
                UnConsPattern (map fn p1) (map fn p2)

            ListPattern patterns ->
                ListPattern (List.map (map fn) patterns)

            VarPattern a ->
                VarPattern a

            NamedPattern ref patterns ->
                NamedPattern ref (List.map (map fn) patterns)

            AsPattern p1 name ->
                AsPattern (map fn p1) name

            ParenthesizedPattern p1 ->
                ParenthesizedPattern (map fn p1)
