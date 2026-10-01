module List.ExtraExtra exposing
    ( fastConcatMap
    , fastConcatMapWithInitial
    , findLastMap
    )

{-| -}


{-| This particular variant doesn't preserve the order like List.concatMap would, but it's marginally faster. We've adjusted the code using it.

<https://github.com/jfmengels/elm-benchmarks/blob/main/src/ListOrderingExploration/ListConcatMap.elm>
<https://github.com/jfmengels/elm-benchmarks/blob/main/src/ListOrderingExploration/ListConcatMap-Results-Chrome.png>

-}
fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap fn list =
    fastConcatMapWithInitial fn list []


fastConcatMapWithInitial : (a -> List b) -> List a -> List b -> List b
fastConcatMapWithInitial fn list initial =
    List.foldr (\item acc -> fn item ++ acc) initial list


findLastMap : (a -> Maybe b) -> List a -> Maybe b
findLastMap f list =
    List.foldr
        (\item acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    f item
        )
        Nothing
        list
