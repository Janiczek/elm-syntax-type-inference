module Main exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Alternative exposing (rank)
import Benchmark.Runner.Alternative as BenchmarkRunner
import Bitwise


main : BenchmarkRunner.Program
main =
    describe "for Janiczek/elm-syntax-type-inference"
        [ rank "String.ExtraExtra.firstCharIsUpper"
            (\f -> exampleStringList |> List.foldl (\exampleString _ -> f exampleString) False)
            [ ( "String.uncons", firstCharIsUpperUncons )
            , ( "String.toList", firstCharIsUpperToList )
            , ( "String.left 2 >> String.toList", firstCharIsUpperLeftToList )
            , ( "String.slice 0 2 >> String.toList", firstCharIsUpperSliceToList )
            , ( "String.slice 0 char-width >> String.any", firstCharIsUpperSliceAny )
            , ( "String.left char-width >> String.any", firstCharIsUpperLeftAny )
            , ( "String.left char-width (reused) >> String.any", firstCharIsUpperLeftAnyReuseSlice )
            , ( "String.left char-width (try 1 first, reused) >> String.any", firstCharIsUpperLeftAnyShortcutReuseSlice )
            ]
        , rank "Elm.Syntax.ModuleName.Extra.splitLastDot"
            (\f -> exampleModuleNames |> List.foldl (\exampleModuleName _ -> f exampleModuleName) ( "", "" ))
            [ ( "String.split, List.reverse twice", splitLastDotSplitListReverseTwice )
            , ( "String.indexes, last Maybe, String.left/dropLeft", splitLastDotIndexesLastMaybeLeftDropLeft )
            , ( "String.indexes, non-empty last, String.left/dropLeft", splitLastDotIndexesNonEmptyLastLeftDropLeft )
            , ( "String.indexes, drop before last, String.left/dropLeft", splitLastDotIndexesDropBeforeLastLeftDropLeft )
            , ( "String.foldl find . string, String.left/dropLeft", splitLastDotFoldrFindStringLeftDropLeft )
            , ( "String.foldl find . code, String.left/dropLeft", splitLastDotFoldrFindCodeLeftDropLeft )
            ]
        , rank "Elm.Syntax.ModuleName.Extra.dottedToFilePath"
            (\f -> exampleModuleNames |> List.foldl (\example _ -> f example) "")
            [ ( "split, join", dottedToFilePathSplitJoin )
            , ( "replace", dottedToFilePathReplace )
            ]
            rank
            "Elm.Syntax.FullModuleName.toString"
            (\f -> exampleFullModuleNames |> List.foldl (\example _ -> f example) "")
            [ ( "join", fullModuleNameToStringJoin )
            , ( "++ join", fullModuleNameToStringAppendJoin )
            ]
        , rank "fromRange"
            (\f -> exampleRanges |> List.foldl (\example _ -> f example) exampleRangeLike)
            [ ( "r<<16 + c", fromRangeAdd )
            , ( "r<<16 | c", fromRangeBitOr )
            ]
        ]
        |> BenchmarkRunner.program


exampleStringList : List String
exampleStringList =
    [ "Dict", "size", "List", "length", "identity", "", "", "VeryLongModuleNameYouWouldNotBelieveHowLongItIs", "NonEmpty", "map", "foldl", "foldr", "", "Internal", "CompanyName", "view", "update", "subscriptions" ]


exampleModuleNames : List String
exampleModuleNames =
    [ "Dict.Dict", "List.List", "TypeAliasWithoutRecordConstructorFunction", "Expect.VeryLongModuleNameYouWouldNotBelieveHowLongItIs", "VeryLongModuleNameYouWouldNotBelieveHowLongItIs.Test", "List.NonEmpty.NonEmpty", "Internal", "CompanyName", "App", "Mode", "Balance.Balanced", "Elm.Syntax.Expression.Expression", "List.ExtraExtra", "Elm.TypeInference.FullModuleName.FullModuleName" ]


exampleModuleNameSegments : List (List String)
exampleModuleNameSegments =
    exampleModuleNames |> List.map (\moduleName -> moduleName |> String.split ".")


exampleFullModuleNames : List ( String, List String )
exampleFullModuleNames =
    exampleModuleNames |> List.map (\moduleName -> ( "Example", moduleName |> String.split "." ))


type alias Range =
    { start : { row : Int, column : Int }
    , end : { row : Int, column : Int }
    }


type alias RangeLike =
    ( Int, Int )


exampleRanges : List Range
exampleRanges =
    List.map4
        (\sr sc er ec ->
            { start = { row = sr * 10, column = sc + 1 }
            , end = { row = er * 4, column = ec // 2 }
            }
        )
        (List.range 1 100)
        (List.range 1 100)
        (List.range 1 100)
        (List.range 1 100)


exampleRangeLike : RangeLike
exampleRangeLike =
    ( 123, 456 )


firstCharIsUpperUncons : String -> Bool
firstCharIsUpperUncons str =
    case String.uncons str of
        Just ( firstChar, _ ) ->
            Char.isUpper firstChar

        Nothing ->
            False


firstCharIsUpperToList : String -> Bool
firstCharIsUpperToList str =
    case String.toList str of
        firstChar :: _ ->
            Char.isUpper firstChar

        [] ->
            False


firstCharIsUpperLeftToList : String -> Bool
firstCharIsUpperLeftToList str =
    case String.toList (String.left 2 str) of
        firstChar :: _ ->
            Char.isUpper firstChar

        [] ->
            False


firstCharIsUpperSliceToList : String -> Bool
firstCharIsUpperSliceToList str =
    case String.toList (String.slice 0 2 str) of
        firstChar :: _ ->
            Char.isUpper firstChar

        [] ->
            False


firstCharIsUpperSliceAny : String -> Bool
firstCharIsUpperSliceAny str =
    String.any Char.isUpper
        (String.slice 0
            (if String.any charIsUtf8Surrogate (String.slice 0 1 str) then
                2

             else
                1
            )
            str
        )


firstCharIsUpperLeftAny : String -> Bool
firstCharIsUpperLeftAny str =
    String.any Char.isUpper
        (String.left
            (if String.any charIsUtf8Surrogate (String.left 1 str) then
                2

             else
                1
            )
            str
        )


firstCharIsUpperLeftAnyReuseSlice : String -> Bool
firstCharIsUpperLeftAnyReuseSlice str =
    let
        firstCodeUnit : String
        firstCodeUnit =
            String.left 1 str
    in
    String.any Char.isUpper
        (if String.any charIsUtf8Surrogate firstCodeUnit then
            String.left 2 str

         else
            firstCodeUnit
        )


firstCharIsUpperLeftAnyShortcutReuseSlice : String -> Bool
firstCharIsUpperLeftAnyShortcutReuseSlice str =
    let
        firstCodeUnit : String
        firstCodeUnit =
            String.left 1 str
    in
    String.any Char.isUpper firstCodeUnit
        || (String.any charIsUtf8Surrogate firstCodeUnit
                && String.any Char.isUpper (String.left 2 str)
           )


{-| Some code points like 🔧 are represented as 2 consecutive UTF-16 codes
within js strings.

So when we use `String.slice`, the resulting String might only contain
one of these halves which are called surrogates.

To check for that, the only way to tell whether you've encountered
a surrogate (that I can imagine at least) is by (ab)using that Char.toCode
accesses it's first _2_ indexes if the code at the first index indicates there must be a second half,
leading to NaN being returned.

-}
charIsUtf8Surrogate : Char -> Bool
charIsUtf8Surrogate char =
    Basics.isNaN (Basics.toFloat (Char.toCode char))


splitLastDotSplitListReverseTwice : String -> ( String, String )
splitLastDotSplitListReverseTwice qualifiedName =
    case List.reverse (String.split "." qualifiedName) of
        [] ->
            ( "", qualifiedName )

        [ single ] ->
            ( "", single )

        last :: rest ->
            ( String.join "." (List.reverse rest), last )


splitLastDotIndexesLastMaybeLeftDropLeft : String -> ( String, String )
splitLastDotIndexesLastMaybeLeftDropLeft qualifiedName =
    case listLast (String.indexes "." qualifiedName) of
        Nothing ->
            ( "", qualifiedName )

        Just lastDotIndex ->
            ( String.left lastDotIndex qualifiedName, String.dropLeft (lastDotIndex + 1) qualifiedName )


listLast : List a -> Maybe a
listLast list =
    case list of
        [] ->
            Nothing

        [ onlyElement ] ->
            Just onlyElement

        _ :: tail ->
            listLast tail


splitLastDotIndexesNonEmptyLastLeftDropLeft : String -> ( String, String )
splitLastDotIndexesNonEmptyLastLeftDropLeft qualifiedName =
    case String.indexes "." qualifiedName of
        [] ->
            ( "", qualifiedName )

        dotIndex0 :: dotIndex1Up ->
            let
                lastDotIndex : Int
                lastDotIndex =
                    listNonEmptyLast dotIndex0 dotIndex1Up
            in
            ( String.left lastDotIndex qualifiedName, String.dropLeft (lastDotIndex + 1) qualifiedName )


listNonEmptyLast : a -> List a -> a
listNonEmptyLast el0 el1Up =
    case el1Up of
        [] ->
            el0

        el1 :: el2Up ->
            listNonEmptyLast el1 el2Up


splitLastDotIndexesDropBeforeLastLeftDropLeft : String -> ( String, String )
splitLastDotIndexesDropBeforeLastLeftDropLeft qualifiedName =
    case listDropBeforeLast (String.indexes "." qualifiedName) of
        [] ->
            ( "", qualifiedName )

        lastDotIndex :: _ ->
            ( String.left lastDotIndex qualifiedName, String.dropLeft (lastDotIndex + 1) qualifiedName )


listDropBeforeLast : List a -> List a
listDropBeforeLast list =
    case list of
        _ :: ((_ :: _) as tail) ->
            listDropBeforeLast tail

        -- [] | [ _ ]
        _ ->
            list


splitLastDotFoldrFindStringLeftDropLeft : String -> ( String, String )
splitLastDotFoldrFindStringLeftDropLeft qualifiedName =
    case
        String.foldr
            (\c acc ->
                case acc of
                    (Ok _) as ok ->
                        ok

                    Err i ->
                        if String.fromChar c == "." then
                            Ok i

                        else
                            Err (i - 1)
            )
            (Err (String.length qualifiedName - 1))
            qualifiedName
    of
        Err _ ->
            ( "", qualifiedName )

        Ok lastDotIndex ->
            ( String.left lastDotIndex qualifiedName, String.right (lastDotIndex + 1) qualifiedName )


splitLastDotFoldrFindCodeLeftDropLeft : String -> ( String, String )
splitLastDotFoldrFindCodeLeftDropLeft qualifiedName =
    case
        String.foldr
            (\c acc ->
                case acc of
                    (Ok _) as ok ->
                        ok

                    Err i ->
                        -- == '.'
                        if Char.toCode c == 46 then
                            Ok i

                        else
                            Err (i - 1)
            )
            (Err (String.length qualifiedName - 1))
            qualifiedName
    of
        Err _ ->
            ( "", qualifiedName )

        Ok lastDotIndex ->
            ( String.left lastDotIndex qualifiedName, String.right (lastDotIndex + 1) qualifiedName )


dottedToFilePathSplitJoin : String -> String
dottedToFilePathSplitJoin dotted =
    "src/" ++ String.join "/" (String.split "." dotted) ++ ".elm"


dottedToFilePathReplace : String -> String
dottedToFilePathReplace dotted =
    "src/" ++ String.replace "." "/" dotted ++ ".elm"


fullModuleNameToStringJoin : ( String, List String ) -> String
fullModuleNameToStringJoin ( fullModuleNameSegment0, fullModuleNameSegment1Up ) =
    String.join "." (fullModuleNameSegment0 :: fullModuleNameSegment1Up)


fullModuleNameToStringAppendJoin : ( String, List String ) -> String
fullModuleNameToStringAppendJoin ( fullModuleNameSegment0, fullModuleNameSegment1Up ) =
    case fullModuleNameSegment1Up of
        [] ->
            fullModuleNameSegment0

        _ ->
            fullModuleNameSegment0 ++ "." ++ String.join "." fullModuleNameSegment1Up


fromRangeBitOr : Range -> RangeLike
fromRangeBitOr { start, end } =
    ( Bitwise.or (Bitwise.shiftLeftBy 16 start.row) start.column
    , Bitwise.or (Bitwise.shiftLeftBy 16 end.row) end.column
    )


fromRangeAdd : Range -> RangeLike
fromRangeAdd { start, end } =
    ( Bitwise.or (Bitwise.shiftLeftBy 16 start.row) start.column
    , Bitwise.or (Bitwise.shiftLeftBy 16 end.row) end.column
    )
