module List.ExtraExtra exposing
    ( fastConcat
    , fastConcatMap
    )


fastConcat : List (List a) -> List a
fastConcat =
    List.foldr (++) []


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap f =
    List.foldr (f >> (++)) []
