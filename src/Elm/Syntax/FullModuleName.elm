module Elm.Syntax.FullModuleName exposing
    ( FullModuleName
    , fromModuleName
    , fromModuleName_
    , toModuleName
    , toString
    )

import Elm.Syntax.ModuleName exposing (ModuleName)
import NonemptyList exposing (NonemptyList)


type alias FullModuleName =
    NonemptyList String


fromModuleName : ModuleName -> Maybe FullModuleName
fromModuleName moduleName =
    NonemptyList.fromList moduleName


fromString : String -> FullModuleName
fromString string =
    NonemptyList.singleton string


fromModuleName_ : ModuleName -> FullModuleName
fromModuleName_ moduleName =
    moduleName
        |> fromModuleName
        |> Maybe.withDefault (fromString "<BUG> The file didn't have a proper module name")


toModuleName : FullModuleName -> ModuleName
toModuleName fullModuleName =
    NonemptyList.toList fullModuleName


toString : FullModuleName -> String
toString moduleName =
    moduleName
        |> NonemptyList.toList
        |> String.join "."
