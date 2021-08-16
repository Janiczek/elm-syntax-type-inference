module Dict.ExtraExtra exposing (combineResult)

import Dict exposing (Dict)


{-| Similar to Result.Extra.combine which works for Lists.
-}
combineResult : Dict comparable (Result err v) -> Result err (Dict comparable v)
combineResult dictOfResults =
    dictOfResults
        |> Dict.foldr
            {- Is this readable enough? We're creating a new dict with the same
               key but the contents of the Results, not the Results themselves.
            -}
            (Result.map2 << Dict.insert)
            (Ok Dict.empty)
