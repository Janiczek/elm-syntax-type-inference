module List.ExtraExtra exposing
    ( consecutivePairs
    , fastConcat
    , fastConcatMap
    , mapConsecutivePairs
    )


fastConcat : List (List a) -> List a
fastConcat =
    List.foldr (++) []


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap f =
    List.foldr (f >> (++)) []


consecutivePairs : List a -> List ( a, a )
consecutivePairs list =
    mapConsecutivePairs Tuple.pair list


mapConsecutivePairs : (a -> a -> b) -> List a -> List b
mapConsecutivePairs fn list =
    List.map2 fn
        list
        (List.drop 1 list)
