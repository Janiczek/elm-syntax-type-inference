module JsonUI exposing (JsonUI(..), OnEvent, decoder, view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import Set exposing (Set)
import UI.Button


type alias OnEvent msg =
    { eventType : String
    , identifier : String
    }
    -> msg


type JsonUI
    = Row { attributes : Dict String String, children : List JsonUI, events : List ( String, String ) }
    | Column { attributes : Dict String String, children : List JsonUI, events : List ( String, String ) }
    | Text { attributes : Dict String String, content : String }
    | ButtonWithContent { attributes : Dict String String, content : String, events : List ( String, String ) }
    | ButtonWithChildren { attributes : Dict String String, children : List JsonUI, events : List ( String, String ) }


decoder : Decoder JsonUI
decoder =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen nodeDecoder


nodeDecoder : String -> Decoder JsonUI
nodeDecoder nodeType =
    case nodeType of
        "row" ->
            Json.Decode.map3
                (\attrs children events ->
                    Row
                        { attributes = attrs
                        , children = children
                        , events = events
                        }
                )
                attributesFieldDecoder
                childrenFieldDecoder
                eventsFieldDecoder

        "column" ->
            Json.Decode.map3
                (\attrs children events ->
                    Column
                        { attributes = attrs
                        , children = children
                        , events = events
                        }
                )
                attributesFieldDecoder
                childrenFieldDecoder
                eventsFieldDecoder

        "text" ->
            Json.Decode.map2
                (\attrs content ->
                    Text
                        { attributes = attrs
                        , content = content
                        }
                )
                attributesFieldDecoder
                contentFieldDecoder

        "button" ->
            Json.Decode.map4
                (\attrs content children events ->
                    if content /= "" && children /= [] then
                        Json.Decode.fail "Button cannot have both content and children"

                    else if content /= "" then
                        ButtonWithContent
                            { attributes = attrs
                            , content = content
                            , events = events
                            }
                            |> Json.Decode.succeed

                    else
                        ButtonWithChildren
                            { attributes = attrs
                            , children = children
                            , events = events
                            }
                            |> Json.Decode.succeed
                )
                attributesFieldDecoder
                contentFieldDecoder
                childrenFieldDecoder
                eventsFieldDecoder
                |> Json.Decode.andThen identity

        _ ->
            Json.Decode.fail ("Unknown node type: " ++ nodeType)


attributesFieldDecoder : Decoder (Dict String String)
attributesFieldDecoder =
    Json.Decode.maybe (Json.Decode.field "attributes" attributesDecoder)
        |> Json.Decode.map (Maybe.withDefault Dict.empty)


{-| Example:

```json
{
    "align-items": "center",
    "justify-content": "start",
    "gap": "8px"
}
```

-}
attributesDecoder : Decoder (Dict String String)
attributesDecoder =
    Json.Decode.dict Json.Decode.string


childrenFieldDecoder : Decoder (List JsonUI)
childrenFieldDecoder =
    Json.Decode.maybe (Json.Decode.field "children" (Json.Decode.list (Json.Decode.lazy (\_ -> decoder))))
        |> Json.Decode.map (Maybe.withDefault [])


{-| Example:

```json
{"click": "button-pressed"}
```

-}
eventDecoder : Decoder ( String, String )
eventDecoder =
    Json.Decode.keyValuePairs Json.Decode.string
        |> Json.Decode.andThen
            (\pairs ->
                case pairs of
                    [ pair ] ->
                        Json.Decode.succeed pair

                    _ ->
                        Json.Decode.fail "Event object must have exactly one key-value pair"
            )


eventsFieldDecoder : Decoder (List ( String, String ))
eventsFieldDecoder =
    Json.Decode.maybe (Json.Decode.field "events" (Json.Decode.list eventDecoder))
        |> Json.Decode.map (Maybe.withDefault [])


contentFieldDecoder : Decoder String
contentFieldDecoder =
    Json.Decode.maybe (Json.Decode.field "content" Json.Decode.string)
        |> Json.Decode.map (Maybe.withDefault "")


contentDecoder : Decoder String
contentDecoder =
    Json.Decode.string


view : OnEvent msg -> JsonUI -> Html msg
view onEvent jsonUi =
    case jsonUi of
        Row { attributes, children, events } ->
            Html.div
                (eventsToHtmlEvents onEvent events
                    ++ [ Html.Attributes.style "display" "flex"
                       , Html.Attributes.style "flex-direction" "row"
                       ]
                    ++ attributesToHtmlAttributes attributes
                )
                (List.map (view onEvent) children)

        Column { attributes, children, events } ->
            Html.div
                (eventsToHtmlEvents onEvent events
                    ++ [ Html.Attributes.style "display" "flex"
                       , Html.Attributes.style "flex-direction" "column"
                       ]
                    ++ attributesToHtmlAttributes attributes
                )
                (List.map (view onEvent) children)

        Text { attributes, content } ->
            Html.span (attributesToHtmlAttributes attributes) [ Html.text content ]

        ButtonWithContent { attributes, content, events } ->
            UI.Button.view
                { state = UI.Button.Default }
                (eventsToHtmlEvents onEvent events
                    ++ attributesToHtmlAttributes attributes
                )
                [ Html.text content ]

        ButtonWithChildren { attributes, children, events } ->
            UI.Button.view
                { state = UI.Button.Default }
                (eventsToHtmlEvents onEvent events
                    ++ attributesToHtmlAttributes attributes
                )
                (List.map (view onEvent) children)


attributesToHtmlAttributes : Dict String String -> List (Html.Attribute msg)
attributesToHtmlAttributes attributes =
    attributes
        |> Dict.toList
        |> List.filterMap attributeToHtmlAttribute


allowedAttributes : Set String
allowedAttributes =
    Set.fromList
        [ "font-weight"
    , "justify-content"
        , "align-items"
        , "gap"
        ]


attributeToHtmlAttribute : ( String, String ) -> Maybe (Html.Attribute msg)
attributeToHtmlAttribute ( name, value ) =
    if Set.member name allowedAttributes then
        Just <| Html.Attributes.style name value

    else
        let
            _ =
                Debug.log "Unknown attribute" ( name, value )
        in
        Nothing


eventsToHtmlEvents : OnEvent msg -> List ( String, String ) -> List (Html.Attribute msg)
eventsToHtmlEvents onEvent events =
    events
        |> List.filterMap (eventToHtmlEvent onEvent)


eventToHtmlEvent : OnEvent msg -> ( String, String ) -> Maybe (Html.Attribute msg)
eventToHtmlEvent onEvent ( event, identifier ) =
    case event of
        "click" ->
            Just <|
                Html.Events.onClick
                    (onEvent
                        { eventType = event
                        , identifier = identifier
                        }
                    )

        _ ->
            let
                _ =
                    Debug.log "Unknown event" ( event, identifier )
            in
            Nothing
