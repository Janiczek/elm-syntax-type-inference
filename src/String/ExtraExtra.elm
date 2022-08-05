module String.ExtraExtra exposing (indent, multilineInput)

import String.Extra as String


indent : Int -> String -> String
indent n string =
    let
        spaces : String
        spaces =
            String.repeat n " "
    in
    string
        |> String.lines
        |> List.map (\line -> spaces ++ line)
        |> String.join "\n"


{-| Allows us to have nicely formatted multi-line strings in parser tests etc.
-}
multilineInput : String -> String
multilineInput string =
    string
        |> String.unindent
        |> removeNewlinesAtEnds


removeNewlinesAtEnds : String -> String
removeNewlinesAtEnds string =
    if String.startsWith "\n" string then
        removeNewlinesAtEnds (String.dropLeft 1 string)

    else if String.endsWith "\n" string then
        removeNewlinesAtEnds (String.dropRight 1 string)

    else
        string
