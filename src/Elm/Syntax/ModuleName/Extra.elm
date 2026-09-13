module Elm.Syntax.ModuleName.Extra exposing (toString)

import Elm.Syntax.ModuleName exposing (ModuleName)


toString : ModuleName -> String
toString moduleName =
    String.join "." moduleName
