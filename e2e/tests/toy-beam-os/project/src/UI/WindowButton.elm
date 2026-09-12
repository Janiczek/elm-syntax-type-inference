module UI.WindowButton exposing (Config, close, graph)

import Html exposing (Html)
import Html.Attributes
import Html.Attributes.Extra
import Html.Events


type alias Config msg =
    { onClick : msg
    , isDimmed : Bool
    }


close : Config msg -> Html msg
close config =
    windowButton "imgs/close-buttons.svg" config


graph : { onClick : msg, isDimmed : Bool } -> Html msg
graph config =
    windowButton "imgs/graph-buttons.svg" config


windowButton : String -> { onClick : msg, isDimmed : Bool } -> Html msg
windowButton url { onClick, isDimmed } =
    Html.button
        [ -- see .window-button in style.css
          Html.Attributes.class "window-button"
        , Html.Attributes.style "background-image" ("url(" ++ url ++ ")")
        , Html.Attributes.style "cursor" "pointer"
        , Html.Attributes.style "width" "13px"
        , Html.Attributes.style "height" "13px"
        , Html.Attributes.style "border" "0"
        , Html.Attributes.Extra.attributeIf isDimmed (Html.Attributes.style "opacity" "0.25")
        , Html.Events.onClick onClick
        ]
        []
