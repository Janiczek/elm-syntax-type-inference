module TypeLookupTable exposing (TypeLookupTable, empty, get, union)

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.Type exposing (Type)
import NonemptyList exposing (NonemptyList)
import TypeLookupTable.Internal as Internal


type alias TypeLookupTable =
    Internal.TypeLookupTable


get : Range -> TypeLookupTable -> Maybe Type
get range (Internal.TypeLookupTable _ dict) =
    Dict.get (Internal.toRangeLike range) dict


union : NonemptyList TypeLookupTable -> TypeLookupTable
union ( fst, rest ) =
    List.foldl Internal.unionSingle fst rest


empty : ModuleName -> TypeLookupTable
empty moduleName =
    Internal.empty moduleName
