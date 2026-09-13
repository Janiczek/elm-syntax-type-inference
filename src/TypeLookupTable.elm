module TypeLookupTable exposing (TypeLookupTable, fromList, get)

{-| Types inferred for source ranges in a given Elm module (its name is not tracked here).

@docs TypeLookupTable, fromList, get

-}

import Dict
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.Type exposing (Type)
import RangeLike exposing (RangeLike)
import TypeLookupTable.Internal as Internal


type alias TypeLookupTable =
    Internal.TypeLookupTable


fromList : List ( Range, Type ) -> TypeLookupTable
fromList list =
    list
        |> List.map (\( range, type_ ) -> ( RangeLike.fromRange range, type_ ))
        |> Dict.fromList
        |> Internal.TLT


{-| Look up the inferred type for a source range.
-}
get : Range -> TypeLookupTable -> Maybe Type
get range (Internal.TLT tlt) =
    Dict.get (RangeLike.fromRange range) tlt
