module Elm.TypeInference exposing (infer)

{-| TODO write docs

TODO check declarations against their type annotations

@docs infer

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.Infer as Infer
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Elm.TypeInference.Type as Type exposing (Id, MonoType)
import Elm.TypeInference.TypeEquation as TypeEquation exposing (TypeEquation)
import Elm.TypeInference.Unify as Unify
import List.ExtraExtra as List
import Maybe.Extra as Maybe
import RangeLike exposing (RangeLike)
import Result.Extra as Result
import TypeLookupTable exposing (TypeLookupTable)


{- TODO Look at converting between our Type
   and https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAnnotation
   and https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Type
-}


{-| TODO docs
-}
infer : Dict ModuleName File -> Result Error (Dict ModuleName TypeLookupTable)
infer files =
    files
        |> parseModuleNameKeys
        |> State.fromMaybe MissingModuleName
        |> State.andThen
            (\files_ ->
                files_
                    |> gatherTypeAliases
                    |> State.andThen (infer_ files_)
            )
        |> State.run (State.init Dict.empty)
        |> Tuple.first


parseModuleNameKeys : Dict ModuleName a -> Maybe (Dict FullModuleName a)
parseModuleNameKeys dict =
    dict
        |> Dict.toList
        |> Maybe.combineMap
            (\( moduleName, value ) ->
                FullModuleName.fromModuleName moduleName
                    |> Maybe.map (\fullModuleName -> ( fullModuleName, value ))
            )
        |> Maybe.map Dict.fromList


infer_ :
    Dict FullModuleName File
    -> Dict ( FullModuleName, VarName ) MonoType
    -> TIState (Dict ModuleName TypeLookupTable)
infer_ files typeAliases =
    State.do (State.traverse (inferFile files) (Dict.toList files)) <| \fileEquations ->
    State.do Infer.varEquations <| \varEquations ->
    let
        allEquations : List TypeEquation
        allEquations =
            List.fastConcat fileEquations ++ varEquations
    in
    State.do (Unify.unifyMany typeAliases (List.map TypeEquation.dropLabel allEquations)) <| \substitutionMap ->
    State.do State.getNodeIds <| \nodeIds ->
    files
        |> Dict.keys
        |> List.map (toTypeLookupTable substitutionMap nodeIds)
        |> Dict.fromList
        |> State.pure


inferFile :
    Dict FullModuleName File
    -> ( FullModuleName, File )
    -> TIState (List TypeEquation)
inferFile files ( moduleName, thisFile ) =
    thisFile.declarations
        |> State.traverse
            (Infer.inferDeclaration
                { files = files
                , thisFile = thisFile
                , thisModuleName = moduleName
                }
            )
        |> State.map List.fastConcat


toTypeLookupTable :
    SubstitutionMap
    -> Dict FullModuleName (Dict RangeLike Id)
    -> FullModuleName
    -> ( ModuleName, TypeLookupTable )
toTypeLookupTable substitutionMap nodeIds fullModuleName =
    let
        moduleName : ModuleName
        moduleName =
            FullModuleName.toModuleName fullModuleName
    in
    ( moduleName
    , nodeIds
        |> Dict.get fullModuleName
        |> Maybe.withDefault Dict.empty
        |> Dict.map (\_ id -> SubstitutionMap.substitute substitutionMap (Type.id id))
        |> TypeLookupTable.fromDict moduleName
    )


gatherTypeAliases :
    Dict FullModuleName File
    -> TIState (Dict ( FullModuleName, VarName ) MonoType)
gatherTypeAliases files =
    files
        |> Dict.toList
        |> List.map
            (\( moduleName, file ) ->
                file.declarations
                    |> List.map
                        (\declarationNode ->
                            case Node.value declarationNode of
                                Declaration.AliasDeclaration typeAlias ->
                                    let
                                        type_ : TIState MonoType
                                        type_ =
                                            typeAlias.typeAnnotation
                                                |> Node.value
                                                |> Type.fromTypeAnnotation
                                                |> Result.mapError (State.error << ImpossibleType)
                                                |> Result.map State.pure
                                                |> Result.merge
                                    in
                                    type_
                                        |> State.map
                                            (\type__ ->
                                                Just
                                                    ( ( moduleName, Node.value typeAlias.name )
                                                    , type__
                                                    )
                                            )

                                _ ->
                                    State.pure Nothing
                        )
                    |> State.combine
                    |> State.map Maybe.values
            )
        |> State.combine
        |> State.map (List.fastConcat >> Dict.fromList)
