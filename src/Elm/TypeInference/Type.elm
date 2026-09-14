module Elm.TypeInference.Type exposing
    ( Type(..), toString
    , PackageName
    )

{-| A data structure representing the Elm types.

This module is not named `Elm.Type` because that already exists in elm/project-metadata-utils.

@docs Type, toString
@docs PackageName

-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.ModuleName.Extra


{-| Eg. "elm/core".
Comes from elm.json dependency keys.
Empty string for the user's project.
-}
type alias PackageName =
    -- We could re-expose Elm.TypeInference.Type.Internal.PackageName, but then the docs wouldn't be as nice.
    String


{-| TODO docs
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
        -- TODO do we need to allow extensible records in here?
        { attributes : Dict String Type
        , uniforms : Dict String Type
        , varyings : Dict String Type
        }



{- Wraps a type in parentheses when it wouldn't parse back unambiguously
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


{-| TODO docs
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

        WebGLShader { attributes, uniforms, varyings } ->
            [ "Shader"
            , toString (Record { fields = attributes })
            , toString (Record { fields = uniforms })
            , toString (Record { fields = varyings })
            ]
                |> String.join " "
