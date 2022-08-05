module Result.ExtraExtra exposing (orElseLazy)


orElseLazy : (() -> Result e (Maybe a)) -> Result e (Maybe a) -> Result e (Maybe a)
orElseLazy after before =
    case before of
        Err e ->
            Err e

        Ok (Just a) ->
            Ok (Just a)

        Ok Nothing ->
            after ()
