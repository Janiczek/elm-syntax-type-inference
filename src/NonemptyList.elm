module NonemptyList exposing
    ( NonemptyList
    , fromList
    , singleton
    )


type alias NonemptyList a =
    ( a, List a )


fromList : List a -> Maybe (NonemptyList a)
fromList list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, xs )


singleton : a -> NonemptyList a
singleton a =
    ( a, [] )
