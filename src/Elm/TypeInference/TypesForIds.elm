module Elm.TypeInference.TypesForIds exposing
    ( TypesForIds
    , empty
    , get
    , insert
    , isEmpty
    )

import Dict exposing (Dict)
import Elm.Qualifiedness exposing (Qualified)
import Elm.Type exposing (Id, TypeOrId(..))


{-| A thin opaque wrapper around a dict from type variable IDs to inferred types.
Note ID can point to another ID (eg. dict entry `(1,Id 2)`) so you might need to
walk this dict multiple times.
-}
type TypesForIds
    = TypesForIds (Dict Id (TypeOrId Qualified))


empty : TypesForIds
empty =
    TypesForIds Dict.empty


{-| Automatically walks the TypeOrId end of the mapping if it's an ID.

    insert 1 (Id 2) (SubstitutionMap [(2,Type Unit)])
    --> SubstitutionMap [(1,Type Unit),(2,Type Unit)]

-}
insert : Id -> TypeOrId Qualified -> TypesForIds -> TypesForIds
insert id typeOrId ((TypesForIds typesForIds) as map) =
    case typeOrId of
        Id id_ ->
            case get id_ map of
                Nothing ->
                    TypesForIds (Dict.insert id typeOrId typesForIds)

                Just another ->
                    insert id another map

        Type _ ->
            TypesForIds (Dict.insert id typeOrId typesForIds)


get : Id -> TypesForIds -> Maybe (TypeOrId Qualified)
get id (TypesForIds typesForIds) =
    Dict.get id typesForIds


isEmpty : TypesForIds -> Bool
isEmpty (TypesForIds typesForIds) =
    Dict.isEmpty typesForIds
