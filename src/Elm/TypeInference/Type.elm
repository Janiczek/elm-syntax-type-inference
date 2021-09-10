module Elm.TypeInference.Type exposing
    ( Id
    , Type(..)
    , TypeOrId(..)
    , external
    , getId
    , getType
    , isParametric
    , varName
    , varName_
    , varNames
    , varNames_
    )

{-| A data structure representing the Elm types.

Module is not `Elm.Type` because that already exists in elm/project-metadata-utils.

-}

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.TypeInference.VarName exposing (VarName)
import List.ExtraExtra as List
import Transform


type TypeOrId
    = Id Id
    | Type Type


type alias Id =
    Int


{-| TODO maybe remove some hardcoded types and refer to them with `external` == UserDefinedType?

Candidates:

  - Int
  - Float
  - Char
  - String
  - Bool
  - List

See here:
<https://github.com/elm/compiler/blob/39949053f9469862b17006e3965f3b440414d13c/compiler/src/Type/Type.hs#L196-L201>

(Evan for some reason never needed to define a list type, while we do...)
(Also, he defined Never. Why do we _not_ need that one?)

-}
type Type
    = TypeVar String
    | Function
        { from : TypeOrId
        , to : TypeOrId
        }
    | Int
    | Float
    | Number {- Int | Number -}
    | Char
    | String
    | Bool
    | List TypeOrId
    | Unit
    | Tuple TypeOrId TypeOrId
    | Tuple3 TypeOrId TypeOrId TypeOrId
    | Record (Dict VarName TypeOrId)
    | ExtensibleRecord
        { type_ : TypeOrId
        , fields : Dict VarName TypeOrId
        }
    | UserDefinedType
        { moduleName : FullModuleName
        , name : VarName
        , args : List TypeOrId
        }
    | WebGLShader
        { attributes : Dict VarName TypeOrId
        , uniforms : Dict VarName TypeOrId
        , varyings : Dict VarName TypeOrId
        }


{-| Unwrap the string inside the type variable
-}
varName : Type -> Maybe String
varName type_ =
    case type_ of
        TypeVar string ->
            Just string

        _ ->
            Nothing


{-| Unwrap the string inside the type variable
-}
varName_ : TypeOrId -> Maybe String
varName_ typeOrId =
    case typeOrId of
        Id _ ->
            Nothing

        Type type_ ->
            varName type_


getId : TypeOrId -> Maybe Int
getId typeOrId =
    case typeOrId of
        Id id ->
            Just id

        Type _ ->
            Nothing


getType : TypeOrId -> Maybe Type
getType typeOrId =
    case typeOrId of
        Id _ ->
            Nothing

        Type type_ ->
            Just type_


{-| Does it contain lower-case type parameters?
-}
isParametric : TypeOrId -> Bool
isParametric typeOrId =
    let
        f =
            isParametric

        recordBindings : Dict VarName TypeOrId -> Bool
        recordBindings bindings =
            List.any f (Dict.values bindings)
    in
    case typeOrId of
        Id _ ->
            True

        Type type_ ->
            case type_ of
                TypeVar _ ->
                    True

                Function { from, to } ->
                    f from || f to

                Int ->
                    False

                Float ->
                    False

                Number ->
                    True

                Char ->
                    False

                String ->
                    False

                Bool ->
                    False

                Unit ->
                    False

                List element ->
                    f element

                Tuple t1 t2 ->
                    f t1 || f t2

                Tuple3 t1 t2 t3 ->
                    f t1 || f t2 || f t3

                Record fields ->
                    recordBindings fields

                ExtensibleRecord r ->
                    -- in practice always `True`
                    f r.type_ || f (Type (Record r.fields))

                UserDefinedType { args } ->
                    List.any f args

                WebGLShader { attributes, uniforms, varyings } ->
                    recordBindings attributes
                        || recordBindings uniforms
                        || recordBindings varyings


varNames : Type -> List String
varNames type_ =
    type_
        |> Transform.children recursiveChildren
        |> List.filterMap varName


varNames_ : TypeOrId -> List String
varNames_ typeOrId =
    typeOrId
        |> Transform.children recursiveChildren_
        |> List.filterMap varName_


{-| Find all the children of this expression (and their children, etc...)
-}
recursiveChildren : (Type -> List Type) -> Type -> List Type
recursiveChildren fn type_ =
    let
        fn_ : TypeOrId -> List Type
        fn_ typeOrId =
            case typeOrId of
                Id _ ->
                    []

                Type t ->
                    fn t

        recordBindings bindings =
            List.fastConcatMap fn_ (Dict.values bindings)
    in
    case type_ of
        TypeVar _ ->
            []

        Function _ ->
            []

        Int ->
            []

        Float ->
            []

        Number ->
            []

        Char ->
            []

        String ->
            []

        Bool ->
            []

        List t ->
            fn_ t

        Unit ->
            []

        Tuple t1 t2 ->
            fn_ t1 ++ fn_ t2

        Tuple3 t1 t2 t3 ->
            fn_ t1 ++ fn_ t2 ++ fn_ t3

        Record fields ->
            recordBindings fields

        ExtensibleRecord { fields } ->
            -- we ignore the record type itself
            fn (Record fields)

        UserDefinedType { args } ->
            List.fastConcatMap fn_ args

        WebGLShader { attributes, uniforms, varyings } ->
            recordBindings attributes
                ++ recordBindings uniforms
                ++ recordBindings varyings


{-| Find all the children of this expression (and their children, etc...)
-}
recursiveChildren_ : (TypeOrId -> List TypeOrId) -> TypeOrId -> List TypeOrId
recursiveChildren_ fn typeOrId =
    let
        recordBindings bindings =
            List.fastConcatMap fn (Dict.values bindings)
    in
    case typeOrId of
        Id _ ->
            []

        Type (TypeVar _) ->
            []

        Type (Function _) ->
            []

        Type Int ->
            []

        Type Float ->
            []

        Type Number ->
            []

        Type Char ->
            []

        Type String ->
            []

        Type Bool ->
            []

        Type (List t) ->
            fn t

        Type Unit ->
            []

        Type (Tuple t1 t2) ->
            fn t1 ++ fn t2

        Type (Tuple3 t1 t2 t3) ->
            fn t1 ++ fn t2 ++ fn t3

        Type (Record fields) ->
            recordBindings fields

        Type (ExtensibleRecord { type_, fields }) ->
            fn type_ ++ fn (Type (Record fields))

        Type (UserDefinedType { args }) ->
            List.fastConcatMap fn args

        Type (WebGLShader { attributes, uniforms, varyings }) ->
            recordBindings attributes
                ++ recordBindings uniforms
                ++ recordBindings varyings


external : FullModuleName -> VarName -> Type
external moduleName typeName =
    UserDefinedType
        { moduleName = moduleName
        , name = typeName
        , args = []
        }
