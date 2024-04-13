module Debug.Extra exposing
    ( standOutErr
    , standOutInfo
    )


standOutErr : String -> String
standOutErr str =
    str
        |> bold
        |> bgRed
        |> white


standOutInfo : String -> String
standOutInfo str =
    str
        |> bold
        |> bgBlue
        |> white


bold : String -> String
bold str =
    String.join "" [ "\u{001B}[1m", str, "\u{001B}[22m" ]


white : String -> String
white str =
    String.join "" [ "\u{001B}[37m", str, "\u{001B}[39m" ]


bgBlue : String -> String
bgBlue str =
    String.join "" [ "\u{001B}[44m", str, "\u{001B}[49m" ]


bgRed : String -> String
bgRed str =
    String.join "" [ "\u{001B}[41m", str, "\u{001B}[49m" ]
