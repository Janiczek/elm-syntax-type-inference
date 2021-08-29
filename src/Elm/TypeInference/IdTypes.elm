module Elm.TypeInference.IdTypes exposing
    ( IdTypes
    , empty
    , get
    , insert
    , isEmpty
    )

import Dict exposing (Dict)
import Elm.TypeInference.Qualifiedness exposing (Qualified)
import Elm.TypeInference.Type exposing (Id, TypeOrId, TypeOrId_(..))


{-| A thin opaque wrapper around a dict from type variable IDs to inferred types.
Note ID can point to another ID (eg. dict entry `(1,Id 2)`) so you might need to
walk this dict multiple times.
-}
type IdTypes
    = IdTypes (Dict Id TypeOrId)


empty : IdTypes
empty =
    IdTypes Dict.empty


{-| Automatically walks the TypeOrId end of the mapping if it's an ID.

    insert 1 (Id 2) (IdTypes [(2,Type Unit)])
    --> IdTypes [(1,Type Unit),(2,Type Unit)]

-}
insert : Id -> TypeOrId -> IdTypes -> IdTypes
insert id typeOrId ((IdTypes typesForIds) as map) =
    case typeOrId of
        Id id_ ->
            case get id_ map of
                Nothing ->
                    IdTypes (Dict.insert id typeOrId typesForIds)

                Just another ->
                    insert id another map

        Type _ ->
            IdTypes (Dict.insert id typeOrId typesForIds)


get : Id -> IdTypes -> Maybe TypeOrId
get id (IdTypes typesForIds) =
    Dict.get id typesForIds


isEmpty : IdTypes -> Bool
isEmpty (IdTypes typesForIds) =
    Dict.isEmpty typesForIds
