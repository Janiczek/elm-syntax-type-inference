module Debug.Extra exposing (blue, red)


red : String -> String
red str =
    str
        |> format.bold
        |> bg.red
        |> fg.white


blue : String -> String
blue str =
    str
        |> format.bold
        |> bg.blue
        |> fg.white


format =
    { bold = wrapIn 1 22
    }


fg =
    { white = wrapIn 37 39
    }


bg =
    { blue = wrapIn 44 49
    , red = wrapIn 41 49
    }


wrapIn : Int -> Int -> String -> String
wrapIn start end str =
    let
        esc code =
            "\u{001B}[" ++ String.fromInt code ++ "m"
    in
    String.join "" [ esc start, str, esc end ]
