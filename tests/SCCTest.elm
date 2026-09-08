module SCCTest exposing (suite)

import Dict exposing (Dict)
import Elm.TypeInference.SCC as SCC
import Expect
import Fuzz exposing (Fuzzer)
import List.Cartesian
import List.ExtraExtra as List
import Set exposing (Set)
import Test exposing (Test)


type alias Graph =
    { nodeCount : Int
    , edges : Dict Int (List Int)
    }


edgesOf : Graph -> Int -> List Int
edgesOf graph node =
    Dict.get node graph.edges
        |> Maybe.withDefault []


nodesOf : Graph -> List Int
nodesOf graph =
    List.range 0 (graph.nodeCount - 1)


graphFuzzer : Fuzzer Graph
graphFuzzer =
    Fuzz.intRange 1 8
        |> Fuzz.andThen
            (\nodeCount ->
                let
                    possibleEdges : List ( Int, Int )
                    possibleEdges =
                        List.Cartesian.map2 Tuple.pair
                            (List.range 0 (nodeCount - 1))
                            (List.range 0 (nodeCount - 1))
                            |> List.filter (\( from, to ) -> from /= to)
                in
                Fuzz.listOfLengthBetween 0 (List.length possibleEdges) (Fuzz.oneOfValues possibleEdges)
                    |> Fuzz.map
                        (\edgeList ->
                            { nodeCount = nodeCount
                            , edges =
                                edgeList
                                    |> List.foldl
                                        (\( from, to ) acc ->
                                            Dict.update from
                                                (\existing ->
                                                    Just (to :: Maybe.withDefault [] existing)
                                                )
                                                acc
                                        )
                                        Dict.empty
                            }
                        )
            )


{-| BFS reachability oracle
-}
reachable : Graph -> Int -> Set Int
reachable graph start =
    let
        go : List Int -> Set Int -> Set Int
        go frontier visited =
            case frontier of
                [] ->
                    visited

                node :: rest ->
                    let
                        next : List Int
                        next =
                            edgesOf graph node
                                |> List.filter (\n -> not (Set.member n visited))
                    in
                    go (next ++ rest) (Set.union (Set.fromList next) visited)
    in
    go [ start ] (Set.singleton start)


{-| To make unit test examples nicer
-}
type alias NamedGraph =
    List ( String, List String )


namedGraphEdges : NamedGraph -> String -> List String
namedGraphEdges graph name =
    graph
        |> List.filterMap
            (\( n, edgesFromN ) ->
                if n == name then
                    Just edgesFromN

                else
                    Nothing
            )
        |> List.fastConcat


namedGraphNodes : NamedGraph -> List String
namedGraphNodes graph =
    graph
        |> List.fastConcatMap (\( n, edgesFromN ) -> n :: edgesFromN)
        |> Set.fromList
        |> Set.toList


sccOf : NamedGraph -> List (List String)
sccOf graph =
    SCC.stronglyConnectedComponents (namedGraphNodes graph) (namedGraphEdges graph)


suite : Test
suite =
    Test.describe "Elm.TypeInference.SCC"
        [ Test.describe "concrete examples"
            [ Test.test "independent declarations: each gets its own group" <|
                \() ->
                    -- x = 1
                    -- y = 2
                    -- z = 3
                    sccOf
                        [ ( "x", [] )
                        , ( "y", [] )
                        , ( "z", [] )
                        ]
                        |> List.map List.sort
                        |> Expect.equalLists
                            [ [ "x" ]
                            , [ "y" ]
                            , [ "z" ]
                            ]
            , Test.test "a simple dependency chain comes back dependencies-first" <|
                \() ->
                    -- main = double x
                    -- double x = x + x
                    -- x = 1
                    sccOf
                        [ ( "main", [ "double" ] )
                        , ( "double", [ "x" ] )
                        , ( "x", [] )
                        ]
                        |> Expect.equalLists
                            [ [ "x" ]
                            , [ "double" ]
                            , [ "main" ]
                            ]
            , Test.test "mutual recursion: isEven/isOdd land in the same group" <|
                \() ->
                    -- isEven n = if n == 0 then True else isOdd n
                    -- isOdd n  = if n == 0 then False else isEven n
                    sccOf
                        [ ( "isEven", [ "isOdd" ] )
                        , ( "isOdd", [ "isEven" ] )
                        ]
                        |> List.map List.sort
                        |> Expect.equalLists
                            [ [ "isEven", "isOdd" ]
                            ]
            , Test.test "something using a mutually-recursive pair is its own group, solved after" <|
                \() ->
                    -- isEven n = if n == 0 then True else isOdd n
                    -- isOdd n  = if n == 0 then False else isEven n
                    -- main = isEven 4
                    sccOf
                        [ ( "isEven", [ "isOdd" ] )
                        , ( "isOdd", [ "isEven" ] )
                        , ( "main", [ "isEven" ] )
                        ]
                        |> List.map List.sort
                        |> Expect.equalLists
                            [ [ "isEven", "isOdd" ]
                            , [ "main" ]
                            ]
            , Test.test "self-recursion: a group of one, containing itself" <|
                \() ->
                    -- loop x = loop x
                    sccOf [ ( "loop", [ "loop" ] ) ]
                        |> Expect.equalLists
                            [ [ "loop" ] ]
            , Test.test "a diamond (no cycle) still comes back dependencies-first" <|
                \() ->
                    -- main   = combine a b
                    -- a      = helper 1
                    -- b      = helper 2
                    -- helper = identity
                    sccOf
                        [ ( "main", [ "a", "b" ] )
                        , ( "a", [ "helper" ] )
                        , ( "b", [ "helper" ] )
                        , ( "helper", [] )
                        ]
                        |> Expect.equalLists
                            [ [ "helper" ]
                            , [ "a" ]
                            , [ "b" ]
                            , [ "main" ]
                            ]
            , Test.test "a 3-cycle is one group, regardless of which node has the extra edge" <|
                \() ->
                    -- a = b
                    -- b = c
                    -- c = a
                    sccOf
                        [ ( "a", [ "b" ] )
                        , ( "b", [ "c" ] )
                        , ( "c", [ "a" ] )
                        ]
                        |> List.map List.sort
                        |> Expect.equalLists
                            [ [ "a", "b", "c" ]
                            ]
            , Test.test "two separate cycles that don't reference each other stay in separate groups" <|
                \() ->
                    -- a = b, b = a
                    -- x = y, y = x
                    sccOf
                        [ ( "a", [ "b" ] )
                        , ( "b", [ "a" ] )
                        , ( "x", [ "y" ] )
                        , ( "y", [ "x" ] )
                        ]
                        |> List.map List.sort
                        |> List.sort
                        |> Expect.equalLists
                            [ [ "a", "b" ]
                            , [ "x", "y" ]
                            ]
            ]
        , Test.fuzz graphFuzzer "every node appears exactly once" <|
            \graph ->
                let
                    sccs : List (List Int)
                    sccs =
                        SCC.stronglyConnectedComponents (nodesOf graph) (edgesOf graph)

                    allReturned : List Int
                    allReturned =
                        List.fastConcat sccs
                in
                allReturned
                    |> List.sort
                    |> Expect.equal (List.sort (nodesOf graph))
        , Test.fuzz graphFuzzer "every cross-group edge points backwards (dependencies first)" <|
            \graph ->
                let
                    sccs : List (List Int)
                    sccs =
                        SCC.stronglyConnectedComponents (nodesOf graph) (edgesOf graph)

                    groupIndex : Dict Int Int
                    groupIndex =
                        sccs
                            |> List.indexedMap (\i group -> List.map (\n -> ( n, i )) group)
                            |> List.fastConcat
                            |> Dict.fromList

                    indexOf : Int -> Int
                    indexOf n =
                        Dict.get n groupIndex |> Maybe.withDefault -1

                    violations : List ( Int, Int )
                    violations =
                        nodesOf graph
                            |> List.fastConcatMap
                                (\from ->
                                    edgesOf graph from
                                        |> List.filterMap
                                            (\to ->
                                                if indexOf from < indexOf to then
                                                    Just ( from, to )

                                                else
                                                    Nothing
                                            )
                                )
                in
                violations
                    |> Expect.equalLists []
        , Test.fuzz graphFuzzer "every group is strongly connected" <|
            \graph ->
                let
                    sccs : List (List Int)
                    sccs =
                        SCC.stronglyConnectedComponents (nodesOf graph) (edgesOf graph)

                    notStronglyConnected : List Int -> Bool
                    notStronglyConnected group =
                        case group of
                            [ _ ] ->
                                False

                            _ ->
                                group
                                    |> List.any
                                        (\u ->
                                            let
                                                reachableFromU : Set Int
                                                reachableFromU =
                                                    reachable graph u
                                            in
                                            group |> List.any (\v -> not (Set.member v reachableFromU))
                                        )

                    badGroups : List (List Int)
                    badGroups =
                        List.filter notStronglyConnected sccs
                in
                badGroups
                    |> Expect.equalLists []
        , Test.fuzz graphFuzzer "every two mutually-reachable nodes end up in the same group (maximality)" <|
            \graph ->
                let
                    sccs : List (List Int)
                    sccs =
                        SCC.stronglyConnectedComponents (nodesOf graph) (edgesOf graph)

                    groupIndex : Dict Int Int
                    groupIndex =
                        sccs
                            |> List.indexedMap (\i group -> List.map (\n -> ( n, i )) group)
                            |> List.fastConcat
                            |> Dict.fromList

                    indexOf : Int -> Int
                    indexOf n =
                        Dict.get n groupIndex |> Maybe.withDefault -1

                    nodes : List Int
                    nodes =
                        nodesOf graph

                    wronglySplit : List ( Int, Int )
                    wronglySplit =
                        nodes
                            |> List.fastConcatMap
                                (\u ->
                                    let
                                        reachableFromU : Set Int
                                        reachableFromU =
                                            reachable graph u
                                    in
                                    nodes
                                        |> List.filterMap
                                            (\v ->
                                                if
                                                    Set.member v reachableFromU
                                                        && Set.member u (reachable graph v)
                                                        && (indexOf u /= indexOf v)
                                                then
                                                    Just ( u, v )

                                                else
                                                    Nothing
                                            )
                                )
                in
                wronglySplit
                    |> Expect.equalLists []
        ]
