module ScreenManager exposing
    ( Config, Model, Msg(..), init, update, view, subscriptions
    , onProcessStopped, setProcessView
    , Dragging(..), Window
    )

{-|

@docs Config, Model, Msg, init, update, view, subscriptions
@docs onProcessStopped, setProcessView
@docs Dragging, Window

-}

import Browser.Events
import Cmd.Extra
import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode
import JsonUI exposing (JsonUI)
import PID exposing (PID)
import Set exposing (Set)
import UI.Screen
import UI.Window
import XY exposing (XY)


type alias Config msg =
    { msg : Msg -> msg
    , onCloseWindow : PID -> msg
    , onJsonUIEvent :
        { pid : PID
        , eventType : String
        , identifier : String
        }
        -> msg
    }


type alias Model =
    { windows : Dict PID (Window Msg)
    , zOrder : List PID
    , dragging : Dragging
    }


type Msg
    = CloseWindow PID
    | WindowFocus PID
    | WindowDragStart PID XY
    | DragMove XY
    | DragEnd


type Dragging
    = NoDragging
    | DraggingWindow
        { windowPid : PID
        , startClientXY : XY
        , currentClientXY : XY
        }


type alias Window msg =
    { position : XY
    , pid : PID
    , title : String
    , jsonUi : JsonUI
    , statusBar : List (UI.Window.StatusBarItem msg)
    , closable : Bool
    , onGraph : Maybe msg
    }


init : Model
init =
    { windows = Dict.empty
    , zOrder = []
    , dragging = NoDragging
    }


view : Config msg -> Model -> Html msg
view config model =
    UI.Screen.view
        { windows =
            model.zOrder
                |> List.filterMap
                    (\pid ->
                        Dict.get pid model.windows
                            |> Maybe.map
                                (\window ->
                                    ( window.position
                                    , { id = window.pid
                                      , title = window.title
                                      , content =
                                            window.jsonUi
                                                |> JsonUI.view
                                                    (\event ->
                                                        config.onJsonUIEvent
                                                            { pid = pid
                                                            , eventType = event.eventType
                                                            , identifier = event.identifier
                                                            }
                                                    )
                                      , statusBar =
                                            window.statusBar
                                                |> List.map (UI.Window.mapStatusBarItem config.msg)
                                      , onClose =
                                            if window.closable then
                                                Just (config.msg <| CloseWindow window.pid)

                                            else
                                                Nothing
                                      , onGraph =
                                            window.onGraph
                                                |> Maybe.map config.msg
                                      }
                                    )
                                )
                    )
        , onWindowDragStart = \pid position -> config.msg <| WindowDragStart pid position
        , onWindowFocus = \pid -> config.msg <| WindowFocus pid
        , draggingWindow =
            case model.dragging of
                NoDragging ->
                    Nothing

                DraggingWindow draggingWindow ->
                    Just
                        ( draggingWindow.windowPid
                        , XY.sub
                            draggingWindow.currentClientXY
                            draggingWindow.startClientXY
                        )
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.dragging /= NoDragging then
        Sub.batch
            [ Browser.Events.onMouseMove
                (Json.Decode.map2 (\x y -> DragMove ( x, y ))
                    (Json.Decode.field "clientX" Json.Decode.int)
                    (Json.Decode.field "clientY" Json.Decode.int)
                )
            , Browser.Events.onMouseUp (Json.Decode.succeed DragEnd)
            ]

    else
        Sub.none


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config msg model =
    case msg of
        CloseWindow pid ->
            ( { model
                | windows = Dict.remove pid model.windows
                , zOrder = List.filter ((/=) pid) model.zOrder
              }
            , config.onCloseWindow pid
                |> Cmd.Extra.perform
            )

        WindowFocus pid ->
            ( model
                |> focusWindow pid
            , Cmd.none
            )

        WindowDragStart pid position ->
            if model.dragging == NoDragging then
                ( { model
                    | dragging =
                        DraggingWindow
                            { windowPid = pid
                            , startClientXY = position
                            , currentClientXY = position
                            }
                  }
                    |> focusWindow pid
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        DragMove newClientXY ->
            ( { model
                | dragging =
                    case model.dragging of
                        NoDragging ->
                            NoDragging

                        DraggingWindow draggingWindow ->
                            DraggingWindow { draggingWindow | currentClientXY = newClientXY }
              }
            , Cmd.none
            )

        DragEnd ->
            case model.dragging of
                NoDragging ->
                    ( model, Cmd.none )

                DraggingWindow draggingWindow ->
                    let
                        updatePosition window =
                            if window.pid == draggingWindow.windowPid then
                                { window
                                    | position =
                                        XY.add
                                            window.position
                                            (XY.sub
                                                draggingWindow.currentClientXY
                                                draggingWindow.startClientXY
                                            )
                                }

                            else
                                window
                    in
                    ( { model
                        | dragging = NoDragging
                        , windows =
                            Dict.update
                                draggingWindow.windowPid
                                (Maybe.map updatePosition)
                                model.windows
                      }
                    , Cmd.none
                    )


focusWindow : PID -> Model -> Model
focusWindow pid model =
    let
        withoutPid =
            List.filter ((/=) pid) model.zOrder
    in
    { model | zOrder = pid :: withoutPid }


onProcessStopped : PID -> Model -> Model
onProcessStopped pid model =
    { model
        | windows = Dict.remove pid model.windows
        , zOrder = List.filter ((/=) pid) model.zOrder
    }


setProcessView : PID -> String -> JsonUI -> Model -> Model
setProcessView pid title jsonUi model =
    case Dict.get pid model.windows of
        Nothing ->
            model
                |> addProcessView pid title jsonUi

        Just window ->
            model
                |> replaceProcessView pid title jsonUi window


addProcessView : PID -> String -> JsonUI -> Model -> Model
addProcessView pid title jsonUi model =
    { model
        | windows =
            model.windows
                |> Dict.insert pid (initWindow pid title jsonUi model.windows)
        , zOrder = pid :: model.zOrder -- by the virtue of adding it from the front, we incidentally focused it
    }


replaceProcessView : PID -> String -> JsonUI -> Window Msg -> Model -> Model
replaceProcessView pid title jsonUi window model =
    { model
        | windows =
            model.windows
                |> Dict.insert pid { window | title = title, jsonUi = jsonUi }
    }


initWindow : PID -> String -> JsonUI -> Dict PID (Window Msg) -> Window Msg
initWindow pid title jsonUi windows =
    let
        positions : Set XY
        positions =
            windows
                |> Dict.values
                |> List.map .position
                |> Set.fromList

        idealPosition : XY
        idealPosition =
            ( 24, 24 )

        step : XY
        step =
            ( 12, 12 )

        finalPosition : XY
        finalPosition =
            nextPosition idealPosition

        nextPosition : XY -> XY
        nextPosition position =
            if Set.member position positions then
                nextPosition (XY.add step position)

            else
                position
    in
    { position = finalPosition
    , pid = pid
    , title = title
    , jsonUi = jsonUi
    , statusBar =
        [ { label = "PID: " ++ String.fromInt pid, onClick = Nothing }
        ]
    , closable = True
    , onGraph = Nothing
    }
