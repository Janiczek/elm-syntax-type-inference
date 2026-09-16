port module Runner exposing (main)

{-| Used by e2e/run.mjs.

Reads Elm project's source files and dependency docs.json files, parses
everything, builds a `DependencyEnv`, runs `Elm.TypeInference.inferProject`
with `canSkipChecks = False` (full validation) and reports back via a port.

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Parser
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module
import Elm.Syntax.Node as Node
import Elm.TypeInference exposing (Dependency)
import Elm.TypeInference.Error as Error
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (Step(..))
import Parser


port result : Encode.Value -> Cmd msg


type alias Flags =
    { sources : List SourceFile
    , directDependencies : List String
    , allDependencies : List RawDependency
    }


type alias SourceFile =
    { path : String
    , source : String
    }


type alias RawDependency =
    { name : String
    , dependsOn : List String
    , docsJson : Decode.Value
    }


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map3 Flags
        (Decode.field "sources" (Decode.list sourceFileDecoder))
        (Decode.field "directDependencies" (Decode.list Decode.string))
        (Decode.field "allDependencies" (Decode.list dependencyDecoder))


sourceFileDecoder : Decode.Decoder SourceFile
sourceFileDecoder =
    Decode.map2 SourceFile
        (Decode.field "path" Decode.string)
        (Decode.field "source" Decode.string)


dependencyDecoder : Decode.Decoder RawDependency
dependencyDecoder =
    Decode.map3 RawDependency
        (Decode.field "name" Decode.string)
        (Decode.field "dependsOn" (Decode.list Decode.string))
        (Decode.field "docsJson" Decode.value)


main : Program Decode.Value () msg
main =
    Platform.worker
        { init = init
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


init : Decode.Value -> ( (), Cmd msg )
init flagsValue =
    ( ()
    , result (run flagsValue)
    )


run : Decode.Value -> Encode.Value
run flagsValue =
    case Decode.decodeValue flagsDecoder flagsValue of
        Err err ->
            Encode.object
                [ ( "ok", Encode.bool False )
                , ( "error", Encode.string ("flags decode error: " ++ Decode.errorToString err) )
                ]

        Ok flags ->
            case buildDependencies flags.allDependencies of
                Err err ->
                    Encode.object
                        [ ( "ok", Encode.bool False )
                        , ( "error", Encode.string ("docs.json decode error: " ++ err) )
                        ]

                Ok allDependencies ->
                    case parseAllSources flags.sources of
                        Err err ->
                            Encode.object
                                [ ( "ok", Encode.bool False )
                                , ( "error", Encode.string ("parse error: " ++ err) )
                                ]

                        Ok files ->
                            case
                                Elm.TypeInference.dependencyEnv
                                    { directDependencies = flags.directDependencies
                                    , allDependencies = allDependencies
                                    }
                            of
                                Err depEnvError ->
                                    Encode.object
                                        [ ( "ok", Encode.bool False )
                                        , ( "moduleCount", Encode.int (Dict.size files) )
                                        , ( "error", Encode.string (Error.toString depEnvError) )
                                        ]

                                Ok depEnv ->
                                    let
                                        project =
                                            Elm.TypeInference.inferProject
                                                { canSkipChecks = False }
                                                depEnv
                                                files
                                    in
                                    case Dict.values project.errors of
                                        [] ->
                                            Encode.object
                                                [ ( "ok", Encode.bool True )
                                                , ( "moduleCount", Encode.int (Dict.size files) )
                                                , ( "tableCount", Encode.int (Dict.size project.tables) )
                                                ]

                                        err :: _ ->
                                            Encode.object
                                                [ ( "ok", Encode.bool False )
                                                , ( "moduleCount", Encode.int (Dict.size files) )
                                                , ( "tableCount", Encode.int (Dict.size project.tables) )
                                                , ( "error", Encode.string (Error.toString err) )
                                                ]


buildDependencies : List RawDependency -> Result String (List Dependency)
buildDependencies rawDeps =
    rawDeps
        |> List.foldr
            (\raw acc ->
                acc
                    |> Result.andThen
                        (\accDeps ->
                            case Decode.decodeValue (Decode.list Elm.Docs.decoder) raw.docsJson of
                                Ok modules ->
                                    Ok
                                        ({ name = raw.name
                                         , dependencies = raw.dependsOn
                                         , modules = modules
                                         }
                                            :: accDeps
                                        )

                                Err err ->
                                    Err (raw.name ++ ": " ++ Decode.errorToString err)
                        )
            )
            (Ok [])


parseAllSources : List SourceFile -> Result String (Dict (List String) File)
parseAllSources sources =
    sources
        |> List.Extra.stoppableFoldl
            (\{ path, source } acc ->
                case Elm.Parser.parseToFile source of
                    Ok file ->
                        let
                            moduleName : List String
                            moduleName =
                                Elm.Syntax.Module.moduleName (Node.value file.moduleDefinition)
                        in
                        Continue (Result.map (Dict.insert moduleName file) acc)

                    Err deadEnds ->
                        -- Abort if something is unparsable.
                        Stop (Err (path ++ ": " ++ deadEndsToString deadEnds))
            )
            (Ok Dict.empty)


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map (\{ row, col } -> "line " ++ String.fromInt row ++ ", column " ++ String.fromInt col)
        |> List.Extra.unique
        |> String.join "; "
