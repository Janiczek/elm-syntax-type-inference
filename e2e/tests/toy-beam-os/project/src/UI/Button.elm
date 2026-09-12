module UI.Button exposing (State(..), view)

import Html exposing (Html)
import Html.Attributes
import Html.Attributes.Extra
import UI.Color exposing (color)


type State
    = Default
    | Active
    | Disabled


view : { state : State } -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
view config attrs content =
    let
        { imgSides, imgMiddle, fontColor, cursor } =
            case config.state of
                Default ->
                    { imgSides = "button-default.svg"
                    , imgMiddle = "button-default-middle.svg"
                    , fontColor = color.defaultButtonText
                    , cursor = "pointer"
                    }

                Active ->
                    { imgSides = "button-active.svg"
                    , imgMiddle = "button-active-middle.svg"
                    , fontColor = color.activeButtonText
                    , cursor = "pointer"
                    }

                Disabled ->
                    { imgSides = "button-disabled.svg"
                    , imgMiddle = "button-disabled-middle.svg"
                    , fontColor = color.disabledButtonText
                    , cursor = "not-allowed"
                    }
    in
    Html.button
        ([ [ Html.Attributes.style "font-family" "Charcoal, sans-serif"
           , Html.Attributes.style "font-size" "16px"
           , Html.Attributes.style "border" "none"
           , Html.Attributes.style "background" "transparent"
           , Html.Attributes.style "padding" "0"
           , Html.Attributes.style "display" "flex"
           , Html.Attributes.style "color" fontColor
           , Html.Attributes.style "cursor" cursor
           , Html.Attributes.class "ui-button"
           , Html.Attributes.Extra.attributeIf (config.state == Disabled) (Html.Attributes.attribute "disabled" "true")
           ]
         , attrs
         ]
            |> List.concat
        )
        [ Html.div
            [ Html.Attributes.style "background-image" ("url(imgs/" ++ imgSides ++ ")")
            , Html.Attributes.style "width" "6px"
            , Html.Attributes.style "height" "20px"
            , Html.Attributes.style "background-position" "0 0"
            , Html.Attributes.class "ui-button-side"
            ]
            []
        , Html.div
            [ Html.Attributes.style "background-image" ("url(imgs/" ++ imgMiddle ++ ")")
            , Html.Attributes.style "flex" "1"
            , Html.Attributes.style "height" "20px"
            , Html.Attributes.style "background-repeat" "repeat-x"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "padding" "0 4px"
            , Html.Attributes.class "ui-button-middle"
            ]
            content
        , Html.div
            [ Html.Attributes.style "background-image" ("url(imgs/" ++ imgSides ++ ")")
            , Html.Attributes.style "width" "6px"
            , Html.Attributes.style "height" "20px"
            , Html.Attributes.style "background-position" "6px 0"
            , Html.Attributes.class "ui-button-side"
            ]
            []
        ]
