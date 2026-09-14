module Elm.Syntax.VarName exposing (VarName)

{-| An alias for var names (eg. "foobar" in `\foobar -> foobar + 1`).

@docs VarName

-}


{-| An alias a day keeps [primitive obsession](https://wiki.c2.com/?PrimitiveObsession) away.
-}
type alias VarName =
    String
