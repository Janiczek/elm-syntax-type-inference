module Elm.Syntax.Expression.Extra exposing (functionName)

import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Node as Node


functionName : Function -> String
functionName function =
    function.declaration
        |> Node.value
        |> .name
        |> Node.value
