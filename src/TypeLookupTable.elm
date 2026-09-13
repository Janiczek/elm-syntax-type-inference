module TypeLookupTable exposing (TypeLookupTable, get)

{-| Types inferred for source ranges in one Elm module.

@docs TypeLookupTable, get

-}

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.Type exposing (Type)
import RangeLike exposing (RangeLike)


{-| The inferred type associated with each source range in one module.
-}
type alias TypeLookupTable =
    Dict RangeLike Type


{-| Look up the inferred type for a source range.
-}
get : Range -> TypeLookupTable -> Maybe Type
get range tlt =
    Dict.get (RangeLike.fromRange range) tlt
