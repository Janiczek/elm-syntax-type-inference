port module Runner exposing (main)

{-| Used by e2e/run.mjs.

Kind of a worst-case scenario (as the library is tailored towards lazy sparse usages instead of "give me type of everything you can"):

1.  Reads Elm project's source files and dependency docs.json files
2.  Parses source code to `Elm.Syntax.File`s, `Elm.Docs.Module`s and `Elm.Project.Project`s
3.  Builds an `Elm.TypeInference.Project` for the project (fetching package
    sources on `NeedPackageSources`) -- timed as "project ms"
4.  Runs `Elm.TypeInference.getAllTypes` on every module -- timed as
    "all types ms"
5.  Reports back via ports.

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Parser
import Elm.Syntax.File exposing (File)
import Elm.Syntax.File.Extra as FileExtra
import Elm.Syntax.FullModuleName as FullModuleName
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.ModuleName.Extra as ModuleNameExtra
import Elm.Syntax.Node as Node
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference exposing (Dependency, Project)
import Elm.TypeInference.InferError as InferError exposing (InferError)
import Elm.TypeInference.ProjectError as ProjectError exposing (ProjectError(..))
import Elm.TypeInference.ModuleIds as ModuleIds
import Elm.TypeInference.ModuleIndex as ModuleIndex
import Elm.TypeInference.Type as Type
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Parser
import Set exposing (Set)


port result : Encode.Value -> Cmd msg


port inferredTypes : String -> Cmd msg


port requestInferredTypes : (Decode.Value -> msg) -> Sub msg


port requestPackageSources : Encode.Value -> Cmd msg


port providePackageSources : (Decode.Value -> msg) -> Sub msg


port projectStarted : Encode.Value -> Cmd msg


port projectStopped : Encode.Value -> Cmd msg


port beginProject : (Decode.Value -> msg) -> Sub msg


port allTypesStopped : Encode.Value -> Cmd msg


port beginAllTypes : (Decode.Value -> msg) -> Sub msg


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
    , files : List File
    , sourcePaths : Dict ModuleName String
    , dependencySources : Dict String (List File)
    , currentPackage : Maybe String
    }


type alias PendingInference =
    { project : Project
    , files : List File
    , sourcePaths : Dict ModuleName String
    }


type alias PendingTables =
    { sourcePaths : Dict ModuleName String
    , project : Project
    , files : List File
    , summary : Encode.Value
    }


type Msg
    = GotInferredTypesRequest Decode.Value
    | GotPackageSources Decode.Value
    | GotBeginProject Decode.Value
    | GotBeginAllTypes Decode.Value


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
                    let
                        ( output, newProject ) =
                            tablesToString pending.sourcePaths pending.files pending.project
                    in
                    ( { model | pending = Just { pending | project = newProject } }
                    , inferredTypes output
                    )

        GotPackageSources value ->
            case model.active of
                Nothing ->
                    ( model, Cmd.none )

                Just active ->
                    case Decode.decodeValue (Decode.list providedPackageDecoder) value of
                        Err err ->
                            ( finished Nothing
                            , Cmd.batch
                                [ projectStopped Encode.null
                                , result
                                    (Encode.object
                                        [ ( "ok", Encode.bool False )
                                        , ( "moduleCount", Encode.int (List.length active.files) )
                                        , ( "error", Encode.string ("package sources decode error: " ++ Decode.errorToString err) )
                                        ]
                                    )
                                ]
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

                                mergedActive : Active
                                mergedActive =
                                    { active | dependencySources = merged }
                            in
                            ( { model | active = Just mergedActive }
                            , projectStarted Encode.null
                            )

        GotBeginProject _ ->
            case model.active of
                Nothing ->
                    ( model, Cmd.none )

                Just active ->
                    buildProjectStep active

        GotBeginAllTypes _ ->
            case model.inference of
                Nothing ->
                    ( model, Cmd.none )

                Just pending ->
                    runAllTypes pending


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ requestInferredTypes GotInferredTypesRequest
        , providePackageSources GotPackageSources
        , beginProject GotBeginProject
        , beginAllTypes GotBeginAllTypes
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
                                    ( { active =
                                            Just
                                                { directDependencies = flags.directDependencies
                                                , allDependencies = allDependencies
                                                , files = kept |> Dict.values |> List.map .file
                                                , sourcePaths = Dict.map (\_ { path } -> path) kept
                                                , dependencySources = Dict.empty
                                                , currentPackage = flags.currentPackage
                                                }
                                      , pending = Nothing
                                      , inference = Nothing
                                      }
                                    , projectStarted Encode.null
                                    )


buildProjectStep : Active -> ( Model, Cmd Msg )
buildProjectStep active =
    case
        Elm.TypeInference.init
            { directDependencies = active.directDependencies
            , allDependencies = active.allDependencies
            , sourcesToResolveAmbiguity = active.dependencySources
            , projectPackageName = active.currentPackage
            , projectFiles = active.files
            }
    of
        Err err ->
            case err of
                NeedPackageSources needed ->
                    ( { active = Just active, pending = Nothing, inference = Nothing }
                    , requestPackageSources (Encode.dict identity (Encode.list Encode.string) needed)
                    )

                _ ->
                    ( finished Nothing
                    , Cmd.batch
                        [ projectStopped Encode.null
                        , result (projectErrorValue active.files err)
                        ]
                    )

        Ok proj ->
            ( finished Nothing
                |> (\model ->
                        { model
                            | inference =
                                Just
                                    { project = proj
                                    , files = active.files
                                    , sourcePaths = active.sourcePaths
                                    }
                        }
                   )
            , projectStopped (Encode.bool True)
            )


projectErrorValue : List File -> ProjectError -> Encode.Value
projectErrorValue files err =
    Encode.object
        [ ( "ok", Encode.bool False )
        , ( "moduleCount", Encode.int (List.length files) )
        , ( "error", Encode.string (ProjectError.toString err) )
        ]


{-| Phase two: pull a type out for every recorded range in every file.

Uses the bulk `getAllTypes` path (one module-name resolution + one outer
`tables` update per module).
-}
runAllTypes : PendingInference -> ( Model, Cmd Msg )
runAllTypes pending =
    let
        collectFile : File -> ( Dict ModuleName InferError, Int, Project ) -> ( Dict ModuleName InferError, Int, Project )
        collectFile file ( accErrors, accRangeCount, accProj ) =
            let
                fileModuleName : ModuleName
                fileModuleName =
                    FileExtra.moduleName file
            in
            case Elm.TypeInference.getAllTypes fileModuleName accProj of
                ( Err getErr, nextProj ) ->
                    ( Dict.insert fileModuleName getErr accErrors, accRangeCount, nextProj )

                ( Ok pairs, nextProj ) ->
                    ( accErrors, accRangeCount + List.length pairs, nextProj )

        ( collectedErrors, rangeCount, finalProject ) =
            List.foldl collectFile ( Dict.empty, 0, pending.project ) pending.files

        summary : Encode.Value
        summary =
            Encode.object
                ([ ( "ok", Encode.bool (Dict.isEmpty collectedErrors) )
                 , ( "moduleCount", Encode.int (List.length pending.files) )
                 , ( "tableCount", Encode.int (List.length pending.files - Dict.size collectedErrors) )
                 , ( "rangeCount", Encode.int rangeCount )
                 ]
                    ++ (case Dict.values collectedErrors of
                            [] ->
                                []

                            errs ->
                                [ ( "error", Encode.string (String.join "\n" (List.map InferError.toString errs))) ]
                       )
                )
    in
    ( finished
        (Just
            { sourcePaths = pending.sourcePaths
            , project = finalProject
            , files = pending.files
            , summary = summary
            }
        )
    , Cmd.batch
        [ allTypesStopped Encode.null
        , result summary
        ]
    )


{-| Serialize one inferred type per recorded range line:

    src/Main.elm:60:1-60:31: Platform.Program Main.Flags Main.Model Main.Msg

-}
tablesToString : Dict ModuleName String -> List File -> Project -> ( String, Project )
tablesToString paths files proj0 =
    let
        pathFor : ModuleName -> String
        pathFor moduleName =
            Dict.get moduleName paths
                |> Maybe.withDefault (ModuleNameExtra.toString moduleName)

        goFile : File -> ( List String, Project ) -> ( List String, Project )
        goFile file ( accLines, proj ) =
            let
                fileModuleName : ModuleName
                fileModuleName =
                    FileExtra.moduleName file

                prefix : String
                prefix =
                    pathFor fileModuleName
            in
            case Elm.TypeInference.getAllTypes fileModuleName proj of
                ( Err _, nextProj ) ->
                    ( accLines, nextProj )

                ( Ok pairs, nextProj ) ->
                    ( List.foldl
                        (\( range, type_ ) innerLines ->
                            lineFor prefix range type_ :: innerLines
                        )
                        accLines
                        pairs
                    , nextProj
                    )

        ( reversedLines, finalProj ) =
            List.foldl goFile ( [], proj0 ) files

        lines : List String
        lines =
            reversedLines
                |> List.reverse
                |> List.sort
                |> List.Extra.unique
    in
    case lines of
        [] ->
            ( "", finalProj )

        _ ->
            ( String.join "\n" lines ++ "\n", finalProj )


lineFor : String -> Range -> Type.Type -> String
lineFor path range type_ =
    path
        ++ ":"
        ++ String.fromInt range.start.row
        ++ ":"
        ++ String.fromInt range.start.column
        ++ "-"
        ++ String.fromInt range.end.row
        ++ ":"
        ++ String.fromInt range.end.column
        ++ ": "
        ++ Type.toString type_


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
                        |> List.map ModuleNameExtra.fromDotted
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
    List.filter (List.all ModuleNameExtra.isSegment) suffixes


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
                    List.map ModuleNameExtra.fromDotted exposedDotted

                missing : List String
                missing =
                    roots
                        |> List.filter (\root -> not (Dict.member root modules))
                        |> List.map ModuleNameExtra.toString
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
