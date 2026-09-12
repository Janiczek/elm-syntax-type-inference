module UI.Divider exposing (vertical)

import Html exposing (Html)
import Html.Attributes
import UI.Color exposing (color)


vertical : Html msg
vertical =
    Html.div
        [ Html.Attributes.style "width" "2px"
        , Html.Attributes.style "display" "flex"
        ]
        [ Html.div
            [ Html.Attributes.style "width" "1px"
            , Html.Attributes.style "background-color" color.dividerBg
            , Html.Attributes.style "display" "flex"
            ]
            []
        , Html.div
            [ Html.Attributes.style "width" "1px"
            , Html.Attributes.style "border-top" "1px solid transparent"
            , Html.Attributes.style "height" "calc(100% - 1px)"
            , Html.Attributes.style "margin-top" "1px"
            , Html.Attributes.style "background-color" color.dividerShadow
            , Html.Attributes.style "display" "flex"
            ]
            []
        ]
