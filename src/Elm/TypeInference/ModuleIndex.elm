module Elm.TypeInference.ModuleIndex exposing
    ( ModuleIndex, ImportIndex
    , fromFile
    , importCouldExposeValue, importExposesType
    , modulesWithAlias, isImportedUnaliased
    )

{-| A precomputed summary of everything name resolution needs to know about a
`File`.

`ModuleLookup` used to re-derive all of this from the AST on _every_ name
occurrence: `containsValueDeclaration` is a `List.any` over all declarations,
`exposesValue` rescans declarations per opened union type, and so on. With D
declarations and N name occurrences in a module that's O(N\*D) -- measured at
roughly O(n^1.7) on generated modules, and it's paid twice per name (once to
build the SCC graph, once during inference).

Building this index costs O(D) once per file; every lookup then costs O(log D).

@docs ModuleIndex, ImportIndex
@docs fromFile
@docs importCouldExposeValue, importExposesType
@docs modulesWithAlias, isImportedUnaliased

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (Exposing(..))
import Elm.Syntax.Expression.Extra
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Import
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern.Extra
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.Syntax.VarName exposing (VarName)
import Set exposing (Set)


{-| `declared*` is what the module defines, `exposed*` what it lets out -- with
`Bar(..)` already resolved against the declarations, so no guessing is left for
lookup time.
-}
type alias ModuleIndex =
    { moduleName : FullModuleName
    , dottedModuleName : String
    , declaredValues : Set VarName
    , declaredTypes : Set VarName
    , exposedValues : Set VarName
    , exposedTypes : Set VarName
    , infixes : Dict VarName VarName
    , imports : List ImportIndex
    }


{-| `exposing_` is the import's _own_ `exposing` clause, which only tells us
what this import could bring into unqualified scope; whether the target module
actually has that name is a separate question (`exposedValues` over there).
-}
type alias ImportIndex =
    { moduleName : FullModuleName
    , dottedModuleName : String
    , alias_ : Maybe String
    , exposing_ : ExposingIndex
    }


type ExposingIndex
    = -- `import Foo` -- brings nothing into unqualified scope
      ExposesNothing
    | ExposesAll
    | ExposesExplicit
        { -- `FunctionExpose` + `InfixExpose`
          values : Set VarName
        , -- `TypeOrAliasExpose` + `TypeExpose`
          types : Set VarName
        , -- `TypeExpose` with `(..)`
          hasOpenedUnion : Bool
        , -- `TypeOrAliasExpose`: could be a record alias's constructor function
          opaqueTypes : Set VarName
        }


fromFile : File -> ModuleIndex
fromFile file =
    let
        moduleName : FullModuleName
        moduleName =
            file.moduleDefinition
                |> Node.value
                |> Module.moduleName
                |> FullModuleName.fromModuleName_

        exposing_ : Exposing
        exposing_ =
            file.moduleDefinition
                |> Node.value
                |> Module.exposingList

        decls : Declarations
        decls =
            List.foldl (Node.value >> addDeclaration) emptyDeclarations file.declarations
    in
    { moduleName = moduleName
    , dottedModuleName = FullModuleName.toString moduleName
    , declaredValues = decls.values
    , declaredTypes = decls.types
    , exposedValues = exposedValues exposing_ decls
    , exposedTypes = exposedTypes exposing_ decls
    , infixes = decls.infixes
    , imports = List.map (Node.value >> importIndex) file.imports
    }



-- DECLARATIONS


type alias Declarations =
    { values : Set VarName
    , types : Set VarName
    , unionConstructors : Dict VarName (List VarName)
    , recordAliases : Set VarName
    , infixes : Dict VarName VarName
    }


emptyDeclarations : Declarations
emptyDeclarations =
    { values = Set.empty
    , types = Set.empty
    , unionConstructors = Dict.empty
    , recordAliases = Set.empty
    , infixes = Dict.empty
    }


addDeclaration : Declaration -> Declarations -> Declarations
addDeclaration decl acc =
    case decl of
        FunctionDeclaration fn ->
            { acc | values = Set.insert (Elm.Syntax.Expression.Extra.functionName fn) acc.values }

        AliasDeclaration typeAlias ->
            let
                name : VarName
                name =
                    Node.value typeAlias.name

                isRecord : Bool
                isRecord =
                    case Node.value typeAlias.typeAnnotation of
                        TypeAnnotation.Record _ ->
                            True

                        _ ->
                            False
            in
            { acc
              -- Record aliases define an implicit constructor function
                | values =
                    if isRecord then
                        Set.insert name acc.values

                    else
                        acc.values
                , types = Set.insert name acc.types
                , recordAliases =
                    if isRecord then
                        Set.insert name acc.recordAliases

                    else
                        acc.recordAliases
            }

        CustomTypeDeclaration customType ->
            let
                typeName : VarName
                typeName =
                    Node.value customType.name

                ctorNames : List VarName
                ctorNames =
                    List.map (\ctor -> Node.value (Node.value ctor).name) customType.constructors
            in
            { acc
                | values = List.foldl Set.insert acc.values ctorNames
                , types = Set.insert typeName acc.types
                , unionConstructors =
                    -- `List.head`-like: the first declaration of a name wins,
                    -- matching the old `unionConstructorNames`.
                    if Dict.member typeName acc.unionConstructors then
                        acc.unionConstructors

                    else
                        Dict.insert typeName ctorNames acc.unionConstructors
            }

        PortDeclaration signature ->
            { acc | values = Set.insert (Node.value signature.name) acc.values }

        InfixDeclaration infix ->
            let
                operator : VarName
                operator =
                    Node.value infix.operator
            in
            { acc
                | values = Set.insert operator acc.values
                , infixes =
                    if Dict.member operator acc.infixes then
                        acc.infixes

                    else
                        Dict.insert operator (Node.value infix.function) acc.infixes
            }

        Destructuring pattern _ ->
            { acc
                | values =
                    List.foldl Set.insert
                        acc.values
                        (Elm.Syntax.Pattern.Extra.varNames (Node.value pattern))
            }



-- EXPOSING


exposedValues : Exposing -> Declarations -> Set VarName
exposedValues exposing_ decls =
    case exposing_ of
        All _ ->
            decls.values

        Explicit exposedNodes ->
            exposedNodes
                |> List.foldl
                    (\exposedNode acc ->
                        case Node.value exposedNode of
                            Exposing.FunctionExpose fn ->
                                Set.insert fn acc

                            Exposing.InfixExpose op ->
                                Set.insert op acc

                            Exposing.TypeOrAliasExpose name ->
                                -- Only a record alias brings a value (its
                                -- constructor function) along with the type.
                                if Set.member name decls.recordAliases then
                                    Set.insert name acc

                                else
                                    acc

                            Exposing.TypeExpose exposedType ->
                                if exposedType.open /= Nothing then
                                    Dict.get exposedType.name decls.unionConstructors
                                        |> Maybe.withDefault []
                                        |> List.foldl Set.insert acc

                                else
                                    acc
                    )
                    Set.empty


exposedTypes : Exposing -> Declarations -> Set VarName
exposedTypes exposing_ decls =
    case exposing_ of
        All _ ->
            decls.types

        Explicit exposedNodes ->
            exposedNodes
                |> List.foldl
                    (\exposedNode acc ->
                        case Node.value exposedNode of
                            Exposing.TypeOrAliasExpose name ->
                                Set.insert name acc

                            Exposing.TypeExpose exposedType ->
                                Set.insert exposedType.name acc

                            _ ->
                                acc
                    )
                    Set.empty



-- IMPORTS


importIndex : Elm.Syntax.Import.Import -> ImportIndex
importIndex import_ =
    let
        moduleName : FullModuleName
        moduleName =
            FullModuleName.fromModuleName_ (Node.value import_.moduleName)
    in
    { moduleName = moduleName
    , dottedModuleName = FullModuleName.toString moduleName
    , alias_ =
        case Maybe.map Node.value import_.moduleAlias of
            Just [ single ] ->
                Just single

            _ ->
                Nothing
    , exposing_ =
        case Maybe.map Node.value import_.exposingList of
            Nothing ->
                ExposesNothing

            Just (All _) ->
                ExposesAll

            Just (Explicit exposedNodes) ->
                ExposesExplicit <|
                    List.foldl
                        (\exposedNode acc ->
                            case Node.value exposedNode of
                                Exposing.FunctionExpose fn ->
                                    { acc | values = Set.insert fn acc.values }

                                Exposing.InfixExpose op ->
                                    { acc | values = Set.insert op acc.values }

                                Exposing.TypeOrAliasExpose name ->
                                    { acc
                                        | types = Set.insert name acc.types
                                        , opaqueTypes = Set.insert name acc.opaqueTypes
                                    }

                                Exposing.TypeExpose exposedType ->
                                    { acc
                                        | types = Set.insert exposedType.name acc.types
                                        , hasOpenedUnion = acc.hasOpenedUnion || exposedType.open /= Nothing
                                    }
                        )
                        { values = Set.empty
                        , types = Set.empty
                        , hasOpenedUnion = False
                        , opaqueTypes = Set.empty
                        }
                        exposedNodes
    }


{-| Could this import bring this value/operator into unqualified scope?

A pre-filter for speed (`True` doesn't mean "does expose" -- the target
module's `exposedValues` settles that).

-}
importCouldExposeValue : ImportIndex -> VarName -> Bool
importCouldExposeValue import_ varName =
    case import_.exposing_ of
        ExposesNothing ->
            False

        ExposesAll ->
            True

        ExposesExplicit e ->
            Set.member varName e.values
                || -- Could a type in the list have brought this value in
                   -- without naming it? Only the declarations can tell, so
                   -- say "maybe".
                   (couldBeConstructorName varName
                        && (e.hasOpenedUnion || Set.member varName e.opaqueTypes)
                   )


{-| Does this import's own `exposing` clause name this type?
-}
importExposesType : ImportIndex -> VarName -> Bool
importExposesType import_ typeName =
    case import_.exposing_ of
        ExposesNothing ->
            False

        ExposesAll ->
            True

        ExposesExplicit e ->
            Set.member typeName e.types


couldBeConstructorName : VarName -> Bool
couldBeConstructorName varName =
    case String.uncons varName of
        Just ( firstChar, _ ) ->
            Char.isUpper firstChar

        Nothing ->
            False


{-| Every module aliased to the given name, in import order.

The same alias can be given to more than one import (e.g. `import Svg as S` and
`import Internal.Svg as S`); Elm allows that as long as each individual
reference stays unambiguous, so callers have to try them all.

-}
modulesWithAlias : ModuleIndex -> String -> List FullModuleName
modulesWithAlias index wantedAlias =
    index.imports
        |> List.filterMap
            (\import_ ->
                if import_.alias_ == Just wantedAlias then
                    Just import_.moduleName

                else
                    Nothing
            )


isImportedUnaliased : ModuleIndex -> ModuleName -> Bool
isImportedUnaliased index moduleName =
    List.any
        (\import_ -> FullModuleName.toModuleName import_.moduleName == moduleName)
        index.imports
