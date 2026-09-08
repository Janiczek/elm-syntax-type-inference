module Elm.TypeInference.SCC exposing (stronglyConnectedComponents)

{-| Strongly Connected Components (Tarjan's algorithm)

Gives a list of components (lists of nodes) where each node is reachable from
every other node.

Used to find which bindings can be type-checked together and in what order -
important for mutual recursion (eg. isEven and isOdd defined in terms of each
other, either in top-level decls or in let bindings).

Each inner group can then be solved in isolation (with access to previously
solved groups).

It also helps let-polymorphism as a type can be generalized only after its
equations are fully collected. Each group is solved monomorphically before any
generalization.

-}

import Dict exposing (Dict)
import Set exposing (Set)


type alias Frame comparable =
    { node : comparable
    , remaining : List comparable
    }


type alias Acc comparable =
    { index : Dict comparable Int
    , lowlink : Dict comparable Int
    , onStack : Set comparable
    , nodeStack : List comparable
    , sccs : List (List comparable)
    , counter : Int
    }


stronglyConnectedComponents : List comparable -> (comparable -> List comparable) -> List (List comparable)
stronglyConnectedComponents nodes edges =
    let
        initAcc : Acc comparable
        initAcc =
            { index = Dict.empty
            , lowlink = Dict.empty
            , onStack = Set.empty
            , nodeStack = []
            , sccs = []
            , counter = 0
            }

        finalAcc : Acc comparable
        finalAcc =
            List.foldl (visit edges) initAcc nodes
    in
    List.reverse finalAcc.sccs


visit : (comparable -> List comparable) -> comparable -> Acc comparable -> Acc comparable
visit edges start acc =
    if Dict.member start acc.index then
        acc

    else
        runFrames
            edges
            [ { node = start, remaining = edges start } ]
            (initNode start acc)


initNode : comparable -> Acc comparable -> Acc comparable
initNode v acc =
    { index = Dict.insert v acc.counter acc.index
    , lowlink = Dict.insert v acc.counter acc.lowlink
    , onStack = Set.insert v acc.onStack
    , nodeStack = v :: acc.nodeStack
    , sccs = acc.sccs
    , counter = acc.counter + 1
    }


{-| DFS.
-}
runFrames : (comparable -> List comparable) -> List (Frame comparable) -> Acc comparable -> Acc comparable
runFrames edges frames acc =
    case frames of
        [] ->
            acc

        frame :: outerFrames ->
            case frame.remaining of
                [] ->
                    -- Done exploring `frame.node`'s neighbours.
                    let
                        v : comparable
                        v =
                            frame.node

                        vIndex : Int
                        vIndex =
                            Dict.get v acc.index |> Maybe.withDefault -1

                        vLowlink : Int
                        vLowlink =
                            Dict.get v acc.lowlink |> Maybe.withDefault -1

                        accAfterPop : Acc comparable
                        accAfterPop =
                            if vLowlink == vIndex then
                                let
                                    ( component, remainingStack ) =
                                        splitOffComponent v acc.nodeStack
                                in
                                { acc
                                    | sccs = component :: acc.sccs
                                    , nodeStack = remainingStack
                                    , onStack = List.foldl Set.remove acc.onStack component
                                }

                            else
                                acc
                    in
                    case outerFrames of
                        [] ->
                            runFrames edges outerFrames accAfterPop

                        parent :: _ ->
                            let
                                parentLowlink : Int
                                parentLowlink =
                                    Dict.get parent.node accAfterPop.lowlink |> Maybe.withDefault -1
                            in
                            runFrames edges
                                outerFrames
                                { accAfterPop
                                    | lowlink = Dict.insert parent.node (min parentLowlink vLowlink) accAfterPop.lowlink
                                }

                w :: ws ->
                    let
                        framesWithNextNeighbour : List (Frame comparable)
                        framesWithNextNeighbour =
                            { frame | remaining = ws } :: outerFrames
                    in
                    if not (Dict.member w acc.index) then
                        -- Tree edge: recurse into `w`.
                        runFrames edges
                            ({ node = w, remaining = edges w } :: framesWithNextNeighbour)
                            (initNode w acc)

                    else if Set.member w acc.onStack then
                        let
                            wIndex : Int
                            wIndex =
                                Dict.get w acc.index |> Maybe.withDefault -1

                            vLowlink : Int
                            vLowlink =
                                Dict.get frame.node acc.lowlink |> Maybe.withDefault -1
                        in
                        runFrames edges
                            framesWithNextNeighbour
                            { acc | lowlink = Dict.insert frame.node (min vLowlink wIndex) acc.lowlink }

                    else
                        -- `w` belongs to an already-completed component
                        runFrames edges framesWithNextNeighbour acc


splitOffComponent : comparable -> List comparable -> ( List comparable, List comparable )
splitOffComponent v stack =
    case stack of
        [] ->
            ( [], [] )

        x :: rest ->
            if x == v then
                ( [ x ], rest )

            else
                let
                    ( component, remaining ) =
                        splitOffComponent v rest
                in
                ( x :: component, remaining )
