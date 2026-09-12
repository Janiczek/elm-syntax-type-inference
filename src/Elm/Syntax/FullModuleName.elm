module Elm.Syntax.FullModuleName exposing (FullModuleName, fromDotted, fromModuleName, fromModuleName_, toModuleName, toString)

{-|

@docs FullModuleName, fromDotted, fromModuleName, fromModuleName_, toModuleName, toString

-}

import Elm.Syntax.ModuleName exposing (ModuleName)
import NonemptyList exposing (NonemptyList)


{-| TODO docs
-}
type alias FullModuleName =
    NonemptyList String


{-| TODO docs
-}
fromModuleName : ModuleName -> Maybe FullModuleName
fromModuleName moduleName =
    NonemptyList.fromList moduleName


{-| TODO docs
-}
fromString : String -> FullModuleName
fromString string =
    NonemptyList.singleton string


{-| TODO docs
-}
fromModuleName_ : ModuleName -> FullModuleName
fromModuleName_ moduleName =
    moduleName
        |> fromModuleName
        |> Maybe.withDefault (fromString "<BUG> The file didn't have a proper module name")


{-| "Platform.Cmd" -> ("Platform", ["Cmd"])
-}
fromDotted : String -> FullModuleName
fromDotted dotted =
    dotted
        |> String.split "."
        |> fromModuleName_


{-| TODO docs
-}
toModuleName : FullModuleName -> ModuleName
toModuleName fullModuleName =
    NonemptyList.toList fullModuleName


{-| TODO docs
-}
toString : FullModuleName -> String
toString moduleName =
    moduleName
        |> NonemptyList.toList
        |> String.join "."
