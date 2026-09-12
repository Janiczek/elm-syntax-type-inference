module TypeLookupTable exposing (TypeLookupTable, fromDict, get)

{-| Types inferred for source ranges in one Elm module.

@docs TypeLookupTable, fromDict, get

-}

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.Type exposing (Type)
import RangeLike exposing (RangeLike)


{-| The inferred type associated with each source range in one module.
-}
type TypeLookupTable
    = TypeLookupTable (Dict RangeLike Type)


{-| Look up the inferred type for a source range.
-}
get : Range -> TypeLookupTable -> Maybe Type
get range (TypeLookupTable dict) =
    Dict.get (RangeLike.fromRange range) dict


{-| Build a lookup table from its internal range map.
-}
fromDict : Dict RangeLike Type -> TypeLookupTable
fromDict dict =
    TypeLookupTable dict
