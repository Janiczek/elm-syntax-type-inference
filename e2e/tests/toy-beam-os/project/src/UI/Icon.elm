module UI.Icon exposing (Icon_16_16, view_16_16)

import Html exposing (Html)
import Html.Attributes


type alias Icon_16_16 =
    { logo : String
    }


icon_16_16 : Icon_16_16
icon_16_16 =
    { logo = "imgs/logo_16x16.png"
    }


view_16_16 : (Icon_16_16 -> String) -> Html msg
view_16_16 get =
    Html.div
        [ Html.Attributes.style "background-image" ("url(" ++ get icon_16_16 ++ ")")
        , Html.Attributes.style "width" "16px"
        , Html.Attributes.style "height" "16px"
        , Html.Attributes.style "background-size" "contain"
        , Html.Attributes.style "background-position" "center"
        , Html.Attributes.style "background-repeat" "no-repeat"
        ]
        []
