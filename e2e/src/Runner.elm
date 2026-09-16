port module Runner exposing (main)

{-| Used by e2e/run.mjs.

Reads Elm project's source files and dependency docs.json files, parses
everything, builds a `DependencyEnv`, runs `Elm.TypeInference.inferProject`
with the `canSkipChecks` flag value (`False` = full validation) and reports back via ports.

1.  Elm infers and sends summary (`ok` / `error`) via the `result` port.
2.  run.mjs stops its timer, prints how fast it was and what the result was,
    then (only on success) sends `requestInferredTypes`.
3.  Elm serializes the tables and sends the text via the `inferredTypes` port.

-}

import Bitwise
import Dict exposing (Dict)
import Elm.Docs
import Elm.Parser
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.TypeInference exposing (Dependency)
import Elm.TypeInference.Error as Error
import Elm.TypeInference.Type as PublicType
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (Step(..))
import Parser
import RangeLike exposing (RangeLike)
import TypeLookupTable.Internal exposing (TypeLookupTable(..))


port result : Encode.Value -> Cmd msg


port inferredTypes : String -> Cmd msg


port requestInferredTypes : (Decode.Value -> msg) -> Sub msg


type alias Model =
    Maybe PendingTables


type alias PendingTables =
    { sourcePaths : Dict ModuleName String
    , tables : Dict ModuleName TypeLookupTable
    }


type Msg
    = GotInferredTypesRequest Decode.Value


type alias Flags =
    { sources : List SourceFile
    , directDependencies : List String
    , allDependencies : List RawDependency
    , canSkipChecks : Bool
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
    Decode.map4 Flags
        (Decode.field "sources" (Decode.list sourceFileDecoder))
        (Decode.field "directDependencies" (Decode.list Decode.string))
        (Decode.field "allDependencies" (Decode.list dependencyDecoder))
        (Decode.field "canSkipChecks" Decode.bool)


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


main : Program Decode.Value Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Decode.Value -> ( Model, Cmd Msg )
init flagsValue =
    run flagsValue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInferredTypesRequest _ ->
            case model of
                Nothing ->
                    ( model, inferredTypes "" )

                Just pending ->
                    ( model, inferredTypes (tablesToString pending.sourcePaths pending.tables) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    requestInferredTypes GotInferredTypesRequest


run : Decode.Value -> ( Model, Cmd Msg )
run flagsValue =
    case Decode.decodeValue flagsDecoder flagsValue of
        Err err ->
            ( Nothing
            , result
                (Encode.object
                    [ ( "ok", Encode.bool False )
                    , ( "error", Encode.string ("flags decode error: " ++ Decode.errorToString err) )
                    ]
                )
            )

        Ok flags ->
            case buildDependencies flags.allDependencies of
                Err err ->
                    ( Nothing
                    , result
                        (Encode.object
                            [ ( "ok", Encode.bool False )
                            , ( "error", Encode.string ("docs.json decode error: " ++ err) )
                            ]
                        )
                    )

                Ok allDependencies ->
                    case parseAllSources flags.sources of
                        Err err ->
                            ( Nothing
                            , result
                                (Encode.object
                                    [ ( "ok", Encode.bool False )
                                    , ( "error", Encode.string ("parse error: " ++ err) )
                                    ]
                                )
                            )

                        Ok modules ->
                            let
                                files : Dict (List String) File
                                files =
                                    Dict.map (\_ { file } -> file) modules

                                sourcePaths : Dict (List String) String
                                sourcePaths =
                                    Dict.map (\_ { path } -> path) modules
                            in
                            case
                                Elm.TypeInference.dependencyEnv
                                    { directDependencies = flags.directDependencies
                                    , allDependencies = allDependencies
                                    }
                            of
                                Err depEnvError ->
                                    ( Nothing
                                    , result
                                        (Encode.object
                                            [ ( "ok", Encode.bool False )
                                            , ( "moduleCount", Encode.int (Dict.size files) )
                                            , ( "error", Encode.string (Error.toString depEnvError) )
                                            ]
                                        )
                                    )

                                Ok depEnv ->
                                    let
                                        project =
                                            Elm.TypeInference.inferProject
                                                { canSkipChecks = flags.canSkipChecks }
                                                depEnv
                                                files

                                        pending : Model
                                        pending =
                                            Just
                                                { sourcePaths = sourcePaths
                                                , tables = project.tables
                                                }
                                    in
                                    case Dict.values project.errors of
                                        [] ->
                                            ( pending
                                            , result
                                                (Encode.object
                                                    [ ( "ok", Encode.bool True )
                                                    , ( "moduleCount", Encode.int (Dict.size files) )
                                                    , ( "tableCount", Encode.int (Dict.size project.tables) )
                                                    ]
                                                )
                                            )

                                        err :: _ ->
                                            ( pending
                                            , result
                                                (Encode.object
                                                    [ ( "ok", Encode.bool False )
                                                    , ( "moduleCount", Encode.int (Dict.size files) )
                                                    , ( "tableCount", Encode.int (Dict.size project.tables) )
                                                    , ( "error", Encode.string (Error.toString err) )
                                                    ]
                                                )
                                            )


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


parseAllSources : List SourceFile -> Result String (Dict (List String) { path : String, file : File })
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
                        Continue (Result.map (Dict.insert moduleName { path = path, file = file }) acc)

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


{-| Serialize every inferred type in every module table, one per line:

    src/Main.elm:60:1-60:31: Platform.Program Main.Flags Main.Model Main.Msg

-}
tablesToString : Dict ModuleName String -> Dict ModuleName TypeLookupTable -> String
tablesToString paths tables =
    let
        pathFor : ModuleName -> String
        pathFor moduleName =
            Dict.get moduleName paths
                |> Maybe.withDefault (String.join "." moduleName)

        lines : List String
        lines =
            tables
                |> Dict.toList
                |> List.sortBy (\( moduleName, _ ) -> pathFor moduleName)
                |> List.concatMap (\( moduleName, table ) -> tableToLines (pathFor moduleName) table)
                |> List.Extra.unique
    in
    case lines of
        [] ->
            ""

        _ ->
            String.join "\n" lines ++ "\n"


tableToLines : String -> TypeLookupTable -> List String
tableToLines path (TLT entries) =
    entries
        |> Dict.toList
        |> List.sortBy (\( rangeLike, _ ) -> rangeSortKey rangeLike)
        |> List.map
            (\( rangeLike, type_ ) ->
                let
                    ( startRow, startCol ) =
                        unpackPos (Tuple.first rangeLike)

                    ( endRow, endCol ) =
                        unpackPos (Tuple.second rangeLike)
                in
                path
                    ++ ":"
                    ++ String.fromInt startRow
                    ++ ":"
                    ++ String.fromInt startCol
                    ++ "-"
                    ++ String.fromInt endRow
                    ++ ":"
                    ++ String.fromInt endCol
                    ++ ": "
                    ++ PublicType.toString type_
            )


rangeSortKey : RangeLike -> List Int
rangeSortKey ( start, end ) =
    let
        ( startRow, startCol ) =
            unpackPos start

        ( endRow, endCol ) =
            unpackPos end
    in
    [ startRow, startCol, endRow, endCol ]


unpackPos : Int -> ( Int, Int )
unpackPos pos =
    ( Bitwise.shiftRightBy 16 pos, Bitwise.and 0xFFFF pos )
