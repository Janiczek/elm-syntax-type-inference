module Elm.Syntax.FullModuleName exposing
    ( FullModuleName
    , fromModuleName
    , fromModuleName_
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
