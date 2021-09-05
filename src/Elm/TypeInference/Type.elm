module Elm.TypeInference.Type exposing
    ( Id
    , Type(..)
    , TypeOrId(..)
    , getId
    , getType
    , isParametric
    , mapType
    , mapTypeOrId
    , varName
    , varName_
    , varNames
    , varNames_
    )

{-| A data structure representing the Elm types.

Module is not `Elm.Type` because that already exists in elm/project-metadata-utils.

-}

import Dict exposing (Dict)
import Dict.ExtraExtra as Dict
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.TypeInference.VarName exposing (VarName)
import List.ExtraExtra as List
import Result.Extra as Result
import Transform


type TypeOrId
    = Id Id
    | Type Type


type alias Id =
    Int


type Type
    = TypeVar String
    | Function { from : TypeOrId, to : TypeOrId }
    | Int
    | Float
    | Char
    | String
    | Bool
    | List TypeOrId
    | Unit
    | Tuple TypeOrId TypeOrId
    | Tuple3 TypeOrId TypeOrId TypeOrId
    | Record (Dict VarName TypeOrId)
    | {- The actual definitions of type aliases and custom types are elsewhere
         (in the Declaration module), this is just a "pointer", "var".

         Also, this is the *usage* of a type! So while definition of Maybe
         might be `Maybe a`, here you'll most likely see specific stuff
         like `Maybe Int`.

         This constructor encompasses both type aliases and custom types:
      -}
      UserDefinedType
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

                Record bindings ->
                    recordBindings bindings

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

        Record bindings ->
            recordBindings bindings

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

        Type (Record bindings) ->
            recordBindings bindings

        Type (UserDefinedType { args }) ->
            List.fastConcatMap fn args

        Type (WebGLShader { attributes, uniforms, varyings }) ->
            recordBindings attributes
                ++ recordBindings uniforms
                ++ recordBindings varyings


mapTypeOrId : (a -> b) -> TypeOrId -> TypeOrId
mapTypeOrId fn typeOrId =
    case typeOrId of
        Id id ->
            Id id

        Type type_ ->
            Type <| mapType fn type_


mapType : (a -> b) -> Type -> Type
mapType fn type_ =
    let
        f =
            mapTypeOrId fn
    in
    case type_ of
        TypeVar str ->
            TypeVar str

        Function { from, to } ->
            Function
                { from = f from
                , to = f to
                }

        Int ->
            Int

        Float ->
            Float

        Char ->
            Char

        String ->
            String

        Bool ->
            Bool

        List typeOrId ->
            List <| f typeOrId

        Unit ->
            Unit

        Tuple a b ->
            Tuple
                (f a)
                (f b)

        Tuple3 a b c ->
            Tuple3
                (f a)
                (f b)
                (f c)

        Record dict ->
            Record <| Dict.map (always f) dict

        UserDefinedType r ->
            UserDefinedType
                { moduleName = r.moduleName
                , name = r.name
                , args = List.map f r.args
                }

        WebGLShader { attributes, uniforms, varyings } ->
            WebGLShader
                { attributes = Dict.map (always f) attributes
                , uniforms = Dict.map (always f) uniforms
                , varyings = Dict.map (always f) varyings
                }
