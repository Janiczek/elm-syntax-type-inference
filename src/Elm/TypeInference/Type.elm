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
    , fromTypeAnnotation
    , generalize
    , getDebugId
    , id
    , id_
    , isParametric
    , mono
    , monoTypeToString
    , normalize
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
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.TypeInference.VarName exposing (VarName)
import List.ExtraExtra as List
import Result.Extra as Result
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
            let
                bindingsStr =
                    (" " ++ recordBindings bindings ++ " ")
                        |> String.trim
            in
            "{" ++ bindingsStr ++ "}"

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


normalize : Type -> Type
normalize ((Forall boundVars monoType) as type_) =
    let
        allVars : List TypeVar
        allVars =
            Set.union
                (freeVarsMono monoType)
                (Set.fromList boundVars)
                |> Set.toList

        newVars : List TypeVar
        newVars =
            allVars
                |> List.foldl
                    (\( style, super ) ( nextId, nextVarOrd, acc ) ->
                        case style of
                            Generated _ ->
                                ( nextId + 1, nextVarOrd, ( Generated nextId, super ) :: acc )

                            Named _ ->
                                ( nextId, nextVarOrd + 1, ( Named (ordToName nextVarOrd), super ) :: acc )
                    )
                    ( 0, 0, [] )
                |> (\( _, _, vars ) -> List.reverse vars)

        -- We can't use SubstitutionMap.substitute because it works recursively
        -- We need to replace the vars just once and not follow the links.
        subst : AssocList.Dict TypeVar TypeVar
        subst =
            List.map2 Tuple.pair allVars newVars
                |> AssocList.fromList
    in
    type_
        |> mapVars
            (\var ->
                case AssocList.get var subst of
                    Nothing ->
                        var

                    Just newVar ->
                        newVar
            )


mapVars : (TypeVar -> TypeVar) -> Type -> Type
mapVars fn (Forall boundVars monoType) =
    Forall (List.map fn boundVars) (mapVarsMono fn monoType)


mapVarsMono : (TypeVar -> TypeVar) -> MonoType -> MonoType
mapVarsMono fn type_ =
    Transform.transformOnce
        recurse
        (\t ->
            case t of
                TypeVar var ->
                    TypeVar (fn var)

                _ ->
                    t
        )
        type_


ordToName : Int -> String
ordToName n =
    let
        radix =
            26

        {- The functions below are stolen from fredcy/elm-parseint and tweaked
           to work similar to:

           https://en.wikipedia.org/wiki/Bijective_numeration#The_bijective_base-26_system
        -}
        charFromInt : Int -> Char
        charFromInt i =
            Char.fromCode <| i + Char.toCode 'a'

        go : Int -> String
        go i =
            if i < radix then
                String.fromChar <| charFromInt i

            else
                go (i // radix) ++ (String.fromChar <| charFromInt (modBy radix i))
    in
    go n


fromTypeAnnotation : TypeAnnotation -> Result TypeAnnotation MonoType
fromTypeAnnotation typeAnnotation =
    let
        f : TypeAnnotation -> Result TypeAnnotation MonoType
        f annotation =
            fromTypeAnnotation annotation

        recordBindings :
            List (Node ( Node String, Node TypeAnnotation ))
            -> Result TypeAnnotation (Dict VarName MonoType)
        recordBindings fields =
            fields
                |> List.map
                    (\fieldNode ->
                        let
                            ( fieldNameNode, annotationNode ) =
                                Node.value fieldNode

                            type_ : Result TypeAnnotation MonoType
                            type_ =
                                f (Node.value annotationNode)
                        in
                        type_
                            |> Result.map (\type__ -> ( Node.value fieldNameNode, type__ ))
                    )
                |> Result.combine
                |> Result.map Dict.fromList
    in
    case typeAnnotation of
        TypeAnnotation.GenericType name ->
            Ok <| TypeVar ( Named name, Normal )

        TypeAnnotation.Typed name annotations ->
            let
                ( moduleName, typeName ) =
                    Node.value name

                fullModuleName : FullModuleName
                fullModuleName =
                    FullModuleName.fromModuleName_ moduleName

                args : Result TypeAnnotation (List MonoType)
                args =
                    annotations
                        |> List.map (Node.value >> f)
                        |> Result.combine
            in
            args
                |> Result.map
                    (\args_ ->
                        UserDefinedType
                            { moduleName = fullModuleName
                            , name = typeName
                            , args = args_
                            }
                    )

        TypeAnnotation.Unit ->
            Ok Unit

        TypeAnnotation.Tupled [ a, b ] ->
            Result.map2 Tuple
                (f (Node.value a))
                (f (Node.value b))

        TypeAnnotation.Tupled [ a, b, c ] ->
            Result.map3 Tuple3
                (f (Node.value a))
                (f (Node.value b))
                (f (Node.value c))

        TypeAnnotation.Tupled _ ->
            Err typeAnnotation

        TypeAnnotation.Record fields ->
            recordBindings fields
                |> Result.map Record

        TypeAnnotation.GenericRecord name fields ->
            recordBindings (Node.value fields)
                |> Result.map
                    (\fields_ ->
                        ExtensibleRecord
                            { type_ = TypeVar ( Named (Node.value name), Normal )
                            , fields = fields_
                            }
                    )

        TypeAnnotation.FunctionTypeAnnotation from to ->
            Result.map2
                (\from_ to_ ->
                    Function
                        { from = from_
                        , to = to_
                        }
                )
                (f (Node.value from))
                (f (Node.value to))
