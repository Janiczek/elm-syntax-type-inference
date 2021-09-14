module Elm.TypeInference.Type exposing
    ( Id
    , MonoType(..)
    , SuperType(..)
    , Type(..)
    , TypeVar
    , TypeVarStyle(..)
    , closeOver
    , external
    , freeVars
    , freeVarsMono
    , freeVarsTypeEnv
    , generalize
    , getDebugId
    , id
    , id_
    , isParametric
    , mono
    , monoTypeToString
    , number
    , number_
    , recurse
    , toString
    , varToString
    )

{-| A data structure representing the Elm types.

Module is not `Elm.Type` because that already exists in elm/project-metadata-utils.

-}

import AssocList
import AssocSet as Set exposing (Set)
import Dict exposing (Dict)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.TypeInference.VarName exposing (VarName)
import List.ExtraExtra as List
import Transform


type alias Id =
    Int


id : Id -> Type
id theId =
    mono (id_ theId)


id_ : Id -> MonoType
id_ theId =
    TypeVar ( Generated theId, Normal )


number : Id -> Type
number theId =
    mono (number_ theId)


number_ : Id -> MonoType
number_ theId =
    TypeVar ( Generated theId, Number )


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
    = Forall (List TypeVar) MonoType


{-|

    x : a (in source code) == NormalVar "a"
    x : a (given by compiler) == NormalId 1
    x : number (in source code) == SuperVar Number ""
    x : number1 (in source code) == SuperVar Number "1"
    x : number (given by compiler) == SuperId Number 1

-}
type alias TypeVar =
    ( TypeVarStyle, SuperType )


type TypeVarStyle
    = Generated Id
    | Named String


type SuperType
    = Normal
    | {- Int | Float -} Number


type MonoType
    = TypeVar TypeVar
    | Function
        { from : MonoType
        , to : MonoType
        }
    | Int
    | Float
    | Char
    | String
    | Bool
    | List MonoType
    | Unit
    | Tuple MonoType MonoType
    | Tuple3 MonoType MonoType MonoType
    | Record (Dict VarName MonoType)
    | ExtensibleRecord
        { type_ : MonoType
        , fields : Dict VarName MonoType
        }
    | UserDefinedType
        { moduleName : FullModuleName
        , name : VarName
        , args : List MonoType
        }
    | WebGLShader
        { attributes : Dict VarName MonoType
        , uniforms : Dict VarName MonoType
        , varyings : Dict VarName MonoType
        }


isParametric : Type -> Bool
isParametric (Forall _ monoType) =
    let
        isParametric_ : MonoType -> Bool
        isParametric_ t =
            case t of
                TypeVar _ ->
                    True

                _ ->
                    False
    in
    monoType
        |> Transform.children recursiveChildren
        |> List.any isParametric_


external : FullModuleName -> VarName -> MonoType
external moduleName typeName =
    UserDefinedType
        { moduleName = moduleName
        , name = typeName
        , args = []
        }


mono : MonoType -> Type
mono =
    Forall []



-- TRANSFORM LIB HELPERS


recurse : (MonoType -> MonoType) -> MonoType -> MonoType
recurse f type_ =
    case type_ of
        TypeVar _ ->
            type_

        Function { from, to } ->
            Function
                { from = f from
                , to = f to
                }

        Int ->
            type_

        Float ->
            type_

        Char ->
            type_

        String ->
            type_

        Bool ->
            type_

        List listItemType ->
            List <| f listItemType

        Unit ->
            type_

        Tuple t1 t2 ->
            Tuple (f t1) (f t2)

        Tuple3 t1 t2 t3 ->
            Tuple3 (f t1) (f t2) (f t3)

        Record fields ->
            Record (Dict.map (always f) fields)

        ExtensibleRecord r ->
            ExtensibleRecord
                { type_ = f r.type_
                , fields = Dict.map (always f) r.fields
                }

        UserDefinedType r ->
            UserDefinedType
                { r | args = List.map f r.args }

        WebGLShader r ->
            WebGLShader
                { attributes = Dict.map (always f) r.attributes
                , uniforms = Dict.map (always f) r.uniforms
                , varyings = Dict.map (always f) r.varyings
                }


{-| Find all the children of this expression (and their children, etc...)
-}
recursiveChildren : (MonoType -> List MonoType) -> MonoType -> List MonoType
recursiveChildren fn type_ =
    let
        recordBindings bindings =
            List.fastConcatMap fn (Dict.values bindings)
    in
    case type_ of
        TypeVar _ ->
            []

        Function { from, to } ->
            fn from ++ fn to

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

        List listItemType ->
            fn listItemType

        Unit ->
            []

        Tuple t1 t2 ->
            fn t1 ++ fn t2

        Tuple3 t1 t2 t3 ->
            fn t1 ++ fn t2 ++ fn t3

        Record fields ->
            recordBindings fields

        ExtensibleRecord r ->
            fn r.type_ ++ recordBindings r.fields

        UserDefinedType { args } ->
            List.fastConcatMap fn args

        WebGLShader { attributes, uniforms, varyings } ->
            recordBindings attributes
                ++ recordBindings uniforms
                ++ recordBindings varyings


freeVars : Type -> Set TypeVar
freeVars (Forall boundIds monoType) =
    Set.diff
        (freeVarsMono monoType)
        (Set.fromList boundIds)


freeVarsMono : MonoType -> Set TypeVar
freeVarsMono type_ =
    let
        freeVarsMono_ : MonoType -> Maybe TypeVar
        freeVarsMono_ t =
            case t of
                TypeVar typeVar ->
                    Just typeVar

                _ ->
                    Nothing
    in
    type_
        |> Transform.children recursiveChildren
        |> List.filterMap freeVarsMono_
        |> Set.fromList


freeVarsTypeEnv : Dict VarName Type -> Set TypeVar
freeVarsTypeEnv env =
    env
        |> Dict.values
        |> List.foldl (\type_ acc -> Set.union (freeVars type_) acc) Set.empty


freeVarsTypeEquation : ( Type, Type ) -> Set TypeVar
freeVarsTypeEquation ( t1, t2 ) =
    Set.union
        (freeVars t1)
        (freeVars t2)


closeOver : MonoType -> Type
closeOver monoType =
    -- TODO Diehl normalizes the type var names to a,b,... but we want to keep them... maybe
    monoType
        |> generalize Dict.empty


generalize : Dict VarName Type -> MonoType -> Type
generalize typeEnv monoType =
    let
        boundIds : List TypeVar
        boundIds =
            Set.diff
                (freeVarsMono monoType)
                (freeVarsTypeEnv typeEnv)
                |> Set.toList
    in
    Forall boundIds monoType


toString : Type -> String
toString (Forall boundVars monoType) =
    let
        preamble =
            boundVars
                |> List.map (\var -> "∀" ++ varToString var)
                |> String.join " "
                |> (\str ->
                        if String.isEmpty str then
                            str

                        else
                            str ++ ". "
                   )
    in
    preamble ++ monoTypeToString monoType


monoTypeToString : MonoType -> String
monoTypeToString type_ =
    let
        f =
            monoTypeToString

        recordBindings bindings =
            bindings
                |> Dict.toList
                |> List.map (\( fieldName, fieldType ) -> fieldName ++ " : " ++ f fieldType)
                |> String.join ", "
    in
    case type_ of
        TypeVar var ->
            varToString var

        Function { from, to } ->
            [ from, to ]
                |> List.map f
                |> String.join " -> "

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
            "List " ++ f inner

        Unit ->
            "()"

        Tuple t1 t2 ->
            "( " ++ f t1 ++ ", " ++ f t2 ++ " )"

        Tuple3 t1 t2 t3 ->
            "( " ++ f t1 ++ ", " ++ f t2 ++ ", " ++ f t3 ++ " )"

        Record bindings ->
            "{ " ++ recordBindings bindings ++ " }"

        ExtensibleRecord r ->
            "{ " ++ f r.type_ ++ " | " ++ recordBindings r.fields ++ " }"

        UserDefinedType r ->
            FullModuleName.toString r.moduleName
                ++ "."
                ++ r.name
                ++ " "
                ++ (r.args |> List.map f |> String.join " ")

        WebGLShader r ->
            "Shader "
                ++ String.join " "
                    [ f (Record r.attributes)
                    , f (Record r.uniforms)
                    , f (Record r.varyings)
                    ]


varToString : TypeVar -> String
varToString ( style, super ) =
    -- TODO this has issues: collisions between named and generated
    case ( super, style ) of
        ( Normal, Generated theId ) ->
            "#" ++ String.fromInt theId

        ( Normal, Named name ) ->
            name

        ( Number, Generated theId ) ->
            "number" ++ String.fromInt theId

        ( Number, Named name ) ->
            "number" ++ name


getDebugId : Type -> Int
getDebugId (Forall _ monoType) =
    case monoType of
        TypeVar ( Generated theId, _ ) ->
            theId

        _ ->
            -1
