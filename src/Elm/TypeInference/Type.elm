module Elm.TypeInference.Type exposing
    ( Type(..), toString, toTypeAnnotation
    , PackageName, VarName
    )

{-| A data structure representing the Elm types.

This module is not named `Elm.Type` because that already exists in elm/project-metadata-utils.

@docs Type, toString, toTypeAnnotation
@docs PackageName, VarName

-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.ModuleName.Extra
import Elm.Syntax.Node as Node
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)


{-| Eg. "elm/core".
Comes from elm.json dependency keys.
Empty string for the user's project.
-}
type alias PackageName =
    String


{-| An alias for var names (eg. "foobar" in `\foobar -> foobar + 1`).
-}
type alias VarName =
    String


{-| The inferred type.
-}
type Type
    = TypeVar String
    | Function
        { from : Type
        , to : Type
        }
    | Int
    | Float
    | Char
    | String
    | Bool
    | List Type
    | Unit
    | Tuple2 Type Type
    | Tuple3 Type Type Type
    | Record { fields : Dict String Type }
    | ExtensibleRecord
        { fields : Dict String Type
        , extensionTypevar : String
        }
    | Named
        { package : PackageName
        , moduleName : ModuleName
        , name : String
        , arguments : List Type
        }
    | WebGLShader
        { attributesFields : Dict String Type
        , attributesExtensionTypevar : Maybe String
        , uniformsFields : Dict String Type
        , uniformsExtensionTypevar : Maybe String
        , varyingsFields : Dict String Type
        , varyingsExtensionTypevar : Maybe String
        }


{-| Wraps a type in parentheses when it wouldn't parse back unambiguously
in argument position (of `->` or of a type constructor application).
-}
wrapped : Type -> String
wrapped t =
    case t of
        Function _ ->
            "(" ++ toString t ++ ")"

        Named r ->
            if List.isEmpty r.arguments then
                toString t

            else
                "(" ++ toString t ++ ")"

        _ ->
            toString t


{-| Display a type.

    Function { from = Int, to = TypeVar "a" }
    --> "Int -> a"

-}
toString : Type -> String
toString t =
    case t of
        TypeVar name ->
            name

        Function { from, to } ->
            wrapped from ++ " -> " ++ toString to

        Int ->
            "Int"

        Float ->
            "Float"

        Char ->
            "Char"

        String ->
            "String"

        Bool ->
            "Bool"

        List inner ->
            "List " ++ wrapped inner

        Unit ->
            "()"

        Tuple2 a b ->
            "( " ++ toString a ++ ", " ++ toString b ++ " )"

        Tuple3 a b c ->
            "( " ++ toString a ++ ", " ++ toString b ++ ", " ++ toString c ++ " )"

        Record { fields } ->
            let
                fieldStrings : List String
                fieldStrings =
                    fields
                        |> Dict.toList
                        |> List.map (\( name, fieldType ) -> name ++ " : " ++ toString fieldType)
            in
            "{" ++ String.join ", " fieldStrings ++ "}"

        ExtensibleRecord { fields, extensionTypevar } ->
            let
                fieldStrings : List String
                fieldStrings =
                    fields
                        |> Dict.toList
                        |> List.map (\( name, fieldType ) -> name ++ " : " ++ toString fieldType)
            in
            "{ " ++ extensionTypevar ++ " | " ++ String.join ", " fieldStrings ++ " }"

        Named { moduleName, name, arguments } ->
            let
                argStrings : List String
                argStrings =
                    arguments
                        |> List.map wrapped

                qualifiedName : String
                qualifiedName =
                    Elm.Syntax.ModuleName.Extra.toString moduleName
                        ++ "."
                        ++ name
            in
            (qualifiedName
                :: argStrings
            )
                |> String.join " "

        WebGLShader r ->
            [ "Shader"
            , shaderSlotToString r.attributesFields r.attributesExtensionTypevar
            , shaderSlotToString r.uniformsFields r.uniformsExtensionTypevar
            , shaderSlotToString r.varyingsFields r.varyingsExtensionTypevar
            ]
                |> String.join " "


{-| Convert a `Type` to an `elm-syntax` `TypeAnnotation`.

All `Node`s use dummy ranges.

-}
toTypeAnnotation : Type -> TypeAnnotation
toTypeAnnotation type_ =
    case type_ of
        TypeVar name ->
            TypeAnnotation.GenericType name

        Function { from, to } ->
            TypeAnnotation.FunctionTypeAnnotation
                (Node.empty (toTypeAnnotation from))
                (Node.empty (toTypeAnnotation to))

        Int ->
            TypeAnnotation.Typed (Node.empty ( [ "Basics" ], "Int" )) []

        Float ->
            TypeAnnotation.Typed (Node.empty ( [ "Basics" ], "Float" )) []

        Char ->
            TypeAnnotation.Typed (Node.empty ( [ "Char" ], "Char" )) []

        String ->
            TypeAnnotation.Typed (Node.empty ( [ "String" ], "String" )) []

        Bool ->
            TypeAnnotation.Typed (Node.empty ( [ "Basics" ], "Bool" )) []

        List itemType ->
            TypeAnnotation.Typed
                (Node.empty ( [ "List" ], "List" ))
                [ Node.empty (toTypeAnnotation itemType) ]

        Unit ->
            TypeAnnotation.Unit

        Tuple2 t1 t2 ->
            TypeAnnotation.Tupled
                [ Node.empty (toTypeAnnotation t1)
                , Node.empty (toTypeAnnotation t2)
                ]

        Tuple3 t1 t2 t3 ->
            TypeAnnotation.Tupled
                [ Node.empty (toTypeAnnotation t1)
                , Node.empty (toTypeAnnotation t2)
                , Node.empty (toTypeAnnotation t3)
                ]

        Record { fields } ->
            TypeAnnotation.Record
                (recordFieldsToRecordDefinition fields)

        ExtensibleRecord { fields, extensionTypevar } ->
            TypeAnnotation.GenericRecord
                (Node.empty extensionTypevar)
                (Node.empty (recordFieldsToRecordDefinition fields))

        Named { moduleName, name, arguments } ->
            TypeAnnotation.Typed
                (Node.empty ( moduleName, name ))
                (List.map (toTypeAnnotation >> Node.empty) arguments)

        WebGLShader r ->
            TypeAnnotation.Typed
                (Node.empty ( [ "WebGL" ], "Shader" ))
                ([ shaderSlotToTypeAnnotation r.attributesFields r.attributesExtensionTypevar
                 , shaderSlotToTypeAnnotation r.uniformsFields r.uniformsExtensionTypevar
                 , shaderSlotToTypeAnnotation r.varyingsFields r.varyingsExtensionTypevar
                 ]
                    |> List.map Node.empty
                )


recordFieldsToRecordDefinition : Dict String Type -> TypeAnnotation.RecordDefinition
recordFieldsToRecordDefinition fields =
    fields
        |> Dict.toList
        |> List.map
            (\( fieldName, fieldType ) ->
                Node.empty
                    ( Node.empty fieldName
                    , Node.empty (toTypeAnnotation fieldType)
                    )
            )


shaderSlotToType : Dict String Type -> Maybe String -> Type
shaderSlotToType fields extensionTypevar =
    case extensionTypevar of
        Nothing ->
            Record { fields = fields }

        Just var ->
            if Dict.isEmpty fields then
                TypeVar var

            else
                ExtensibleRecord { fields = fields, extensionTypevar = var }


shaderSlotToString : Dict String Type -> Maybe String -> String
shaderSlotToString fields extensionTypevar =
    toString (shaderSlotToType fields extensionTypevar)


shaderSlotToTypeAnnotation : Dict String Type -> Maybe String -> TypeAnnotation
shaderSlotToTypeAnnotation fields extensionTypevar =
    toTypeAnnotation (shaderSlotToType fields extensionTypevar)
