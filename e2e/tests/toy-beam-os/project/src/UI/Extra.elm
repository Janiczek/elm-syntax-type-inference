module UI.Extra exposing (cssVars)

import Html
import Html.Attributes


cssVars : List ( String, String ) -> Html.Attribute msg
cssVars vars =
    Html.Attributes.attribute "style"
        (vars
            |> List.map (\( name, value ) -> name ++ ": " ++ value ++ ";")
            |> String.concat
        )
