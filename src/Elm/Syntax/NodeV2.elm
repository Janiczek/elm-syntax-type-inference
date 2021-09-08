module Elm.Syntax.NodeV2 exposing
    ( LocatedMeta
    , LocatedNode
    , NodeV2(..)
    , TypedMeta
    , TypedNode
    , combine
    , fromNode
    , map
    , mapMeta
    , meta
    , range
    , type_
    , value
    )

import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.TypeInference.Type exposing (TypeOrId)


type NodeV2 meta value
    = NodeV2 meta value


type alias LocatedMeta =
    { range : Range }


type alias TypedMeta =
    { range : Range
    , type_ : TypeOrId
    }


type alias LocatedNode value =
    NodeV2 LocatedMeta value


type alias TypedNode value =
    NodeV2 TypedMeta value


meta : NodeV2 meta value -> meta
meta (NodeV2 meta_ _) =
    meta_


value : NodeV2 meta value -> value
value (NodeV2 _ value_) =
    value_


range : NodeV2 { meta | range : Range } value -> Range
range (NodeV2 meta_ _) =
    meta_.range


type_ : NodeV2 { meta | type_ : TypeOrId } value -> TypeOrId
type_ (NodeV2 meta_ _) =
    meta_.type_


combine :
    (NodeV2 { meta_ | range : Range } a
     -> NodeV2 { meta_ | range : Range } b
     -> NodeV2 { meta_ | range : Range } c
    )
    -> NodeV2 { meta_ | range : Range } a
    -> NodeV2 { meta_ | range : Range } b
    -> NodeV2 { meta_ | range : Range } c
combine f ((NodeV2 m1 _) as a) ((NodeV2 m2 _) as b) =
    let
        newRange =
            Range.combine [ m1.range, m2.range ]
    in
    f a b
        |> mapMeta (\meta_ -> { meta_ | range = newRange })


mapMeta : (m1 -> m2) -> NodeV2 m1 a -> NodeV2 m2 a
mapMeta f (NodeV2 m a) =
    NodeV2 (f m) a


map : (a -> b) -> NodeV2 m a -> NodeV2 m b
map f (NodeV2 m a) =
    NodeV2 m (f a)


fromNode : Node a -> LocatedNode a
fromNode (Node range_ value_) =
    NodeV2 { range = range_ } value_
