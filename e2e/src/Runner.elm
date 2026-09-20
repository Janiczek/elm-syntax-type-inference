port module Runner exposing (main)

{-| Used by e2e/run.mjs.

Reads Elm project's source files and dependency docs.json files, parses
everything, builds a `DependencyEnv`, runs `Elm.TypeInference.inferProject`
and reports back via ports.

-}

import Bitwise
import Char
import Dict exposing (Dict)
import Elm.Docs
import Elm.Parser
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.TypeInference exposing (Dependency, DependencyEnv)
import Elm.TypeInference.Error as Error
import Elm.TypeInference.ModuleIds as ModuleIds
import Elm.TypeInference.ModuleIndex as ModuleIndex
import Elm.TypeInference.Type as Type
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Parser
import RangeLike exposing (RangeLike)
import Set exposing (Set)
import TypeLookupTable.Internal exposing (TypeLookupTable(..))


port result : Encode.Value -> Cmd msg


port inferredTypes : String -> Cmd msg


port requestInferredTypes : (Decode.Value -> msg) -> Sub msg


port requestPackageSources : Encode.Value -> Cmd msg


port providePackageSources : (Decode.Value -> msg) -> Sub msg


port inferenceStarted : Encode.Value -> Cmd msg


port inferenceStopped : Encode.Value -> Cmd msg


port beginInference : (Decode.Value -> msg) -> Sub msg


type alias Model =
    { active : Maybe Active
    , pending : Maybe PendingTables
    , inference : Maybe PendingInference
    }


finished : Maybe PendingTables -> Model
finished pending =
    { active = Nothing
    , pending = pending
    , inference = Nothing
    }


type alias Active =
    { directDependencies : List String
    , allDependencies : List Dependency
    , files : Dict ModuleName File
    , sourcePaths : Dict ModuleName String
    , dependencySources : Dict String (List File)
    , currentPackage : Maybe String
    }


type alias PendingInference =
    { depEnv : DependencyEnv
    , files : Dict ModuleName File
    , sourcePaths : Dict ModuleName String
    , currentPackage : Maybe String
    }


type alias PendingTables =
    { sourcePaths : Dict ModuleName String
    , tables : Dict ModuleName TypeLookupTable
    }


type Msg
    = GotInferredTypesRequest Decode.Value
    | GotPackageSources Decode.Value
    | GotBeginInference Decode.Value


type alias Flags =
    { sources : List SourceFile
    , directDependencies : List String
    , allDependencies : List RawDependency
    , exposedModules : Maybe (List String)
    , currentPackage : Maybe String
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


type alias ProvidedPackage =
    { name : String
    , sources : List SourceFile
    }


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map5 Flags
        (Decode.field "sources" (Decode.list sourceFileDecoder))
        (Decode.field "directDependencies" (Decode.list Decode.string))
        (Decode.field "allDependencies" (Decode.list dependencyDecoder))
        (Decode.field "exposedModules" (Decode.nullable (Decode.list Decode.string)))
        (Decode.oneOf
            [ Decode.field "currentPackage" (Decode.nullable Decode.string)
            , Decode.succeed Nothing
            ]
        )


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


providedPackageDecoder : Decode.Decoder ProvidedPackage
providedPackageDecoder =
    Decode.map2 ProvidedPackage
        (Decode.field "name" Decode.string)
        (Decode.field "sources" (Decode.list sourceFileDecoder))


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
            case model.pending of
                Nothing ->
                    ( model, inferredTypes "" )

                Just pending ->
                    ( model, inferredTypes (tablesToString pending.sourcePaths pending.tables) )

        GotPackageSources value ->
            case model.active of
                Nothing ->
                    ( model, Cmd.none )

                Just active ->
                    case Decode.decodeValue (Decode.list providedPackageDecoder) value of
                        Err err ->
                            ( finished Nothing
                            , result
                                (Encode.object
                                    [ ( "ok", Encode.bool False )
                                    , ( "moduleCount", Encode.int (Dict.size active.files) )
                                    , ( "error", Encode.string ("package sources decode error: " ++ Decode.errorToString err) )
                                    ]
                                )
                            )

                        Ok provided ->
                            let
                                merged : Dict String (List File)
                                merged =
                                    List.foldl
                                        (\pkg acc ->
                                            let
                                                newFiles : List File
                                                newFiles =
                                                    List.filterMap
                                                        (\sf -> Elm.Parser.parseToFile sf.source |> Result.toMaybe)
                                                        pkg.sources

                                                existing : List File
                                                existing =
                                                    Dict.get pkg.name acc
                                                        |> Maybe.withDefault []
                                            in
                                            Dict.insert pkg.name (existing ++ newFiles) acc
                                        )
                                        active.dependencySources
                                        provided
                            in
                            step { active | dependencySources = merged }

        GotBeginInference _ ->
            case model.inference of
                Nothing ->
                    ( model, Cmd.none )

                Just pending ->
                    runInference pending


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ requestInferredTypes GotInferredTypesRequest
        , providePackageSources GotPackageSources
        , beginInference GotBeginInference
        ]


run : Decode.Value -> ( Model, Cmd Msg )
run flagsValue =
    case Decode.decodeValue flagsDecoder flagsValue of
        Err err ->
            ( finished Nothing
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
                    ( finished Nothing
                    , result
                        (Encode.object
                            [ ( "ok", Encode.bool False )
                            , ( "error", Encode.string ("docs.json decode error: " ++ err) )
                            ]
                        )
                    )

                Ok allDependencies ->
                    let
                        parsedSources : ParsedSources
                        parsedSources =
                            parseAllSources flags.sources
                    in
                    case keepReachable flags.exposedModules parsedSources.parsed of
                        Err err ->
                            case List.filterMap (failureIfExposed flags.exposedModules) parsedSources.failed of
                                first :: _ ->
                                    ( finished Nothing
                                    , result
                                        (Encode.object
                                            [ ( "ok", Encode.bool False )
                                            , ( "error", Encode.string ("parse error: " ++ first) )
                                            ]
                                        )
                                    )

                                [] ->
                                    ( finished Nothing
                                    , result
                                        (Encode.object
                                            [ ( "ok", Encode.bool False )
                                            , ( "error", Encode.string err )
                                            ]
                                        )
                                    )

                        Ok kept ->
                            case relevantParseFailures flags.exposedModules kept parsedSources.failed of
                                first :: _ ->
                                    ( finished Nothing
                                    , result
                                        (Encode.object
                                            [ ( "ok", Encode.bool False )
                                            , ( "error", Encode.string ("parse error: " ++ first) )
                                            ]
                                        )
                                    )

                                [] ->
                                    step
                                        { directDependencies = flags.directDependencies
                                        , allDependencies = allDependencies
                                        , files = Dict.map (\_ { file } -> file) kept
                                        , sourcePaths = Dict.map (\_ { path } -> path) kept
                                        , dependencySources = Dict.empty
                                        , currentPackage = flags.currentPackage
                                        }


step : Active -> ( Model, Cmd Msg )
step active =
    case
        Elm.TypeInference.dependencyEnv
            { directDependencies = active.directDependencies
            , allDependencies = active.allDependencies
            , sourcesToResolveAmbiguity = active.dependencySources
            }
    of
        Elm.TypeInference.Failed depEnvError ->
            reportDepEnvError active.files depEnvError

        Elm.TypeInference.NeedPackageSources needed ->
            ( { active = Just active, pending = Nothing, inference = Nothing }
            , requestPackageSources (Encode.dict identity (Encode.list Encode.string) needed)
            )

        Elm.TypeInference.Ready depEnv ->
            ( finished Nothing
                |> (\model ->
                        { model
                            | inference =
                                Just
                                    { depEnv = depEnv
                                    , files = active.files
                                    , sourcePaths = active.sourcePaths
                                    , currentPackage = active.currentPackage
                                    }
                        }
                   )
            , inferenceStarted Encode.null
            )


reportDepEnvError : Dict (List String) File -> Error.Error -> ( Model, Cmd Msg )
reportDepEnvError files depEnvError =
    ( finished Nothing
    , result
        (Encode.object
            [ ( "ok", Encode.bool False )
            , ( "moduleCount", Encode.int (Dict.size files) )
            , ( "error", Encode.string (Error.toString depEnvError) )
            ]
        )
    )


runInference : PendingInference -> ( Model, Cmd Msg )
runInference pending =
    let
        project =
            Elm.TypeInference.inferProject
                pending.currentPackage
                pending.depEnv
                pending.files

        summary : Encode.Value
        summary =
            case Dict.values project.errors of
                [] ->
                    Encode.object
                        [ ( "ok", Encode.bool True )
                        , ( "moduleCount", Encode.int (Dict.size pending.files) )
                        , ( "tableCount", Encode.int (Dict.size project.tables) )
                        ]

                _ ->
                    Encode.object
                        [ ( "ok", Encode.bool False )
                        , ( "moduleCount", Encode.int (Dict.size pending.files) )
                        , ( "tableCount", Encode.int (Dict.size project.tables) )
                        , ( "error", Encode.string (String.join "\n" (List.map Error.toString (Dict.values project.errors))) )
                        ]
    in
    ( finished
        (Just
            { sourcePaths = pending.sourcePaths
            , tables = project.tables
            }
        )
    , Cmd.batch
        [ inferenceStopped Encode.null
        , result summary
        ]
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


type alias ParsedSources =
    { parsed : Dict ModuleName { path : String, file : File }
    , failed : List { path : String, error : String }
    }


{-| Don't abort on first failure; if not reachable from exposed we don't care about them.
-}
parseAllSources : List SourceFile -> ParsedSources
parseAllSources sources =
    List.foldl
        (\{ path, source } acc ->
            case Elm.Parser.parseToFile source of
                Ok file ->
                    let
                        moduleName : List String
                        moduleName =
                            Elm.Syntax.Module.moduleName (Node.value file.moduleDefinition)
                    in
                    { acc | parsed = Dict.insert moduleName { path = path, file = file } acc.parsed }

                Err deadEnds ->
                    { acc
                        | failed =
                            { path = path
                            , error = path ++ ": " ++ deadEndsToString deadEnds
                            }
                                :: acc.failed
                    }
        )
        { parsed = Dict.empty, failed = [] }
        sources


relevantParseFailures :
    Maybe (List String)
    -> Dict ModuleName { path : String, file : File }
    -> List { path : String, error : String }
    -> List String
relevantParseFailures maybeExposed kept failed =
    case maybeExposed of
        Nothing ->
            List.map .error (List.reverse failed)

        Just _ ->
            let
                importsSet : Set ModuleName
                importsSet =
                    kept
                        |> Dict.values
                        |> List.concatMap
                            (\{ file } ->
                                (ModuleIndex.fromFile ModuleIds.empty file |> Tuple.first).imports
                                    |> List.map (\import_ -> FullModuleName.toModuleName import_.moduleName)
                            )
                        |> Set.fromList
            in
            List.filterMap (failureIfNeeded importsSet maybeExposed) (List.reverse failed)


failureIfExposed : Maybe (List String) -> { path : String, error : String } -> Maybe String
failureIfExposed maybeExposed { path, error } =
    case maybeExposed of
        Nothing ->
            Nothing

        Just exposedDotted ->
            let
                rootsSet : Set ModuleName
                rootsSet =
                    exposedDotted
                        |> List.map (String.split ".")
                        |> Set.fromList
            in
            if List.any (\candidate -> Set.member candidate rootsSet) (pathToCandidateModules path) then
                Just error

            else
                Nothing


failureIfNeeded : Set ModuleName -> Maybe (List String) -> { path : String, error : String } -> Maybe String
failureIfNeeded importsSet maybeExposed { path, error } =
    case failureIfExposed maybeExposed { path = path, error = error } of
        Just _ ->
            Just error

        Nothing ->
            if List.any (\candidate -> Set.member candidate importsSet) (pathToCandidateModules path) then
                Just error

            else
                Nothing


pathToCandidateModules : String -> List ModuleName
pathToCandidateModules path =
    let
        parts : List String
        parts =
            path
                |> String.replace "\\" "/"
                |> String.split "/"
                |> List.filter (\p -> p /= "" && p /= ".")

        withoutExt : List String
        withoutExt =
            case List.reverse parts of
                [] ->
                    []

                last :: rest ->
                    let
                        stem : String
                        stem =
                            if String.endsWith ".elm" last then
                                String.dropRight 4 last

                            else
                                last
                    in
                    List.reverse rest ++ [ stem ]

        suffixes : List (List String)
        suffixes =
            List.indexedMap (\i _ -> List.drop i withoutExt) withoutExt
    in
    List.filter (List.all isModuleSegment) suffixes


isModuleSegment : String -> Bool
isModuleSegment segment =
    case String.toList segment of
        first :: _ ->
            Char.isUpper first

        [] ->
            False


keepReachable :
    Maybe (List String)
    -> Dict ModuleName { path : String, file : File }
    -> Result String (Dict ModuleName { path : String, file : File })
keepReachable maybeExposed modules =
    case maybeExposed of
        Nothing ->
            Ok modules

        Just exposedDotted ->
            let
                roots : List ModuleName
                roots =
                    List.map (String.split ".") exposedDotted

                missing : List String
                missing =
                    roots
                        |> List.filter (\root -> not (Dict.member root modules))
                        |> List.map (String.join ".")
            in
            case missing of
                first :: rest ->
                    Err ("exposed module not found in sources: " ++ String.join ", " (first :: rest))

                [] ->
                    Ok (reachableFrom roots modules)


reachableFrom :
    List ModuleName
    -> Dict ModuleName { path : String, file : File }
    -> Dict ModuleName { path : String, file : File }
reachableFrom roots modules =
    let
        firstPartyImports : ModuleName -> List ModuleName
        firstPartyImports name =
            case Dict.get name modules of
                Nothing ->
                    []

                Just { file } ->
                    (ModuleIndex.fromFile ModuleIds.empty file |> Tuple.first).imports
                        |> List.map (\import_ -> FullModuleName.toModuleName import_.moduleName)
                        |> List.filter (\target -> Dict.member target modules)

        go : List ModuleName -> Set ModuleName -> Dict ModuleName { path : String, file : File } -> Dict ModuleName { path : String, file : File }
        go queue seen acc =
            case queue of
                [] ->
                    acc

                name :: rest ->
                    if Set.member name seen then
                        go rest seen acc

                    else
                        case Dict.get name modules of
                            Nothing ->
                                go rest (Set.insert name seen) acc

                            Just entry ->
                                go (rest ++ firstPartyImports name)
                                    (Set.insert name seen)
                                    (Dict.insert name entry acc)
    in
    go roots Set.empty Dict.empty


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
                    ++ Type.toString type_
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
