port module Main exposing (Flags, Model, Msg, main)

import Browser
import Html exposing (Html)
import Json.Decode
import Json.Encode
import JsonUI exposing (JsonUI)
import PID exposing (PID)
import ScreenManager


port setProcessView :
    ({ pid : PID
     , view :
        { title : String
        , body : Json.Encode.Value
        }
     }
     -> msg
    )
    -> Sub msg


port processHasStopped : (PID -> msg) -> Sub msg


port userClosedWindow : PID -> Cmd msg


port jsonUiEvent : { pid : PID, eventType : String, identifier : String } -> Cmd msg


type alias Flags =
    ()


type alias Model =
    { screenManager : ScreenManager.Model
    }


type Msg
    = ScreenManagerMsg ScreenManager.Msg
    | SetProcessView
        { pid : PID
        , view :
            { title : String
            , body : Json.Encode.Value
            }
        }
    | ProcessHasStopped PID
    | UserClosedWindow PID
    | JsonUIEvent
        { pid : PID
        , eventType : String
        , identifier : String
        }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { screenManager = ScreenManager.init }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScreenManagerMsg subMsg ->
            let
                ( newScreenManager, cmd ) =
                    model.screenManager
                        |> ScreenManager.update
                            { msg = ScreenManagerMsg
                            , onCloseWindow = UserClosedWindow
                            , onJsonUIEvent = JsonUIEvent
                            }
                            subMsg
            in
            ( { model | screenManager = newScreenManager }
            , cmd
            )

        SetProcessView data ->
            case Json.Decode.decodeValue JsonUI.decoder data.view.body of
                Err error ->
                    Debug.todo
                        (Debug.toString
                            ( "SetProcessView"
                            , data.view
                            , error
                            )
                        )

                Ok jsonUi ->
                    ( { model
                        | screenManager =
                            model.screenManager
                                |> ScreenManager.setProcessView data.pid data.view.title jsonUi
                      }
                    , Cmd.none
                    )

        ProcessHasStopped pid ->
            ( { model
                | screenManager =
                    model.screenManager
                        |> ScreenManager.onProcessStopped pid
              }
            , Cmd.none
            )

        UserClosedWindow pid ->
            ( model
            , userClosedWindow pid
            )

        JsonUIEvent data ->
            ( model
            , jsonUiEvent data
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ setProcessView SetProcessView
        , processHasStopped ProcessHasStopped
        , ScreenManager.subscriptions model.screenManager
            |> Sub.map ScreenManagerMsg
        ]


view : Model -> Html Msg
view model =
    ScreenManager.view
        { msg = ScreenManagerMsg
        , onCloseWindow = UserClosedWindow
        , onJsonUIEvent = JsonUIEvent
        }
        model.screenManager
