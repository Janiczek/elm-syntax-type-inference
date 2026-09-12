module List.ExtraExtra exposing (fastConcatMap)

{-| -}


{-| `foldr`, not `foldl`: folding from the left and consing `fn item ++ acc`
builds the result with the groups in _reverse_ order, which is not what a
`concatMap` is allowed to do. `List.foldr` is stack-safe in `elm/core`.
-}
fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap fn list =
    List.foldr (\item acc -> fn item ++ acc) [] list
