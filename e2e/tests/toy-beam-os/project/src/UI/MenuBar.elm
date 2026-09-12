module UI.MenuBar exposing (view)

import Html exposing (Html)
import Html.Attributes
import UI.Icon


view : Html msg
view =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "font-family" "Charcoal, sans-serif"
        ]
        [ Html.div
            [ Html.Attributes.style "background-image" "url(imgs/menu-bar.svg)"
            , Html.Attributes.style "width" "8px"
            , Html.Attributes.style "height" "20px"
            , Html.Attributes.style "background-position" "0 0"
            ]
            []
        , Html.div
            [ Html.Attributes.style "background-image" "url(imgs/menu-bar-middle.svg)"
            , Html.Attributes.style "flex" "1"
            , Html.Attributes.style "height" "20px"
            , Html.Attributes.style "background-repeat" "repeat-x"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "padding" "0 6px"
            , Html.Attributes.style "gap" "12px" -- TODO make menu bar buttons instead, they'll have their own padding etc, and the final gap here should supposedly be 6px
            ]
            [ UI.Icon.view_16_16 .logo
            , Html.div [] [ Html.text "File" ]
            ]
        , Html.div
            [ Html.Attributes.style "background-image" "url(imgs/menu-bar.svg)"
            , Html.Attributes.style "width" "8px"
            , Html.Attributes.style "height" "20px"
            , Html.Attributes.style "background-position" "8px 0"
            ]
            []
        ]
