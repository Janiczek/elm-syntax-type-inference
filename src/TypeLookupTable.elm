module TypeLookupTable exposing (TypeLookupTable, get, union)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.Type exposing (Type)
import TypeLookupTable.Internal as Internal


type alias TypeLookupTable =
    Internal.TypeLookupTable


get : Range -> TypeLookupTable -> Maybe Type
get range (Internal.TypeLookupTable _ dict) =
    Dict.get (Internal.toRangeLike range) dict


union : List TypeLookupTable -> TypeLookupTable
union tables =
    List.foldl Internal.unionSingle Internal.empty tables
