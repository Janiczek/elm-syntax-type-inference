module Result.ExtraExtra exposing (combineFilter, firstJustLazy)

{-| -}


{-| Try each thunk in order, stopping at the first Ok Just or Err.
Otherwise return Ok Nothing.
-}
firstJustLazy : List (() -> Result e (Maybe a)) -> Result e (Maybe a)
firstJustLazy lookups =
    case lookups of
        [] ->
            Ok Nothing

        lookup :: rest ->
            case lookup () of
                Ok Nothing ->
                    firstJustLazy rest

                found ->
                    found


{-| Like `List.filter`, but the predicate can fail and short-circuit.
-}
combineFilter : (a -> Result e Bool) -> List a -> Result e (List a)
combineFilter predicate list =
    case list of
        [] ->
            Ok []

        x :: rest ->
            Result.andThen
                (\keep ->
                    combineFilter predicate rest
                        |> Result.map
                            (\restFiltered ->
                                if keep then
                                    x :: restFiltered

                                else
                                    restFiltered
                            )
                )
                (predicate x)
