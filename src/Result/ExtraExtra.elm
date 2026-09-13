module Result.ExtraExtra exposing (combineFilter, firstJustLazy)

{-| -}


{-| Try thunks until we find something other than Ok Nothing.
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


{-| Like `List.filter`, but the predicate can fail and short-circuit the whole filter traversal.
-}
combineFilter : (a -> Result e Bool) -> List a -> Result e (List a)
combineFilter predicate list =
    case list of
        [] ->
            Ok []

        x :: rest ->
            case predicate x of
                Err err ->
                    Err err

                Ok ok ->
                    combineFilter predicate rest
                        |> Result.map
                            (\restFiltered ->
                                if ok then
                                    x :: restFiltered

                                else
                                    restFiltered
                            )
