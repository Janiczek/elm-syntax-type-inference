module Elm.Syntax.ModuleName.Extra exposing
    ( dottedToFilePath
    , fromDotted
    , isSegment
    , splitLastDot
    , toString
    )

import Elm.Syntax.ModuleName exposing (ModuleName)
import String.ExtraExtra


{-|

    ["Foo", "Bar"] -> "Foo.Bar"
    ["Foo"] -> "Foo"
    [] -> ""

-}
toString : ModuleName -> String
toString moduleName =
    String.join "." moduleName


{-|

    "Foo.Bar" -> ["Foo", "Bar"]
    "Foo" -> ["Foo"]

-}
fromDotted : String -> ModuleName
fromDotted dotted =
    String.split "." dotted


{-|

    [ "Css", "Internal" ] --> "src/Css/Internal.elm"

-}
toFilePath : ModuleName -> String
toFilePath moduleName =
    "src/" ++ String.join "/" moduleName ++ ".elm"


{-|

    "Css.Internal" --> "src/Css/Internal.elm"

-}
dottedToFilePath : String -> String
dottedToFilePath dotted =
    toFilePath (fromDotted dotted)


{-|

    "Foo" -> True
    "foo" -> False
    "" -> False

-}
isSegment : String -> Bool
isSegment =
    String.ExtraExtra.firstCharIsUpper


{-|

    "Platform.Cmd.Cmd" -> ("Platform.Cmd", "Cmd")
    "Int" -> ("", "Int")

-}
splitLastDot : String -> ( String, String )
splitLastDot qualifiedName =
    case listDropBeforeLast (String.indexes "." qualifiedName) of
        [] ->
            ( "", qualifiedName )

        lastDotIndex :: _ ->
            ( String.left lastDotIndex qualifiedName, String.dropLeft (lastDotIndex + 1) qualifiedName )


listDropBeforeLast : List a -> List a
listDropBeforeLast list =
    case list of
        _ :: ((_ :: _) as tail) ->
            listDropBeforeLast tail

        -- [] | [ _ ]
        _ ->
            list
