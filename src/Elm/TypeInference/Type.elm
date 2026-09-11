module Elm.TypeInference.Type exposing
    ( FromTypeAnnotationError(..)
    , Id
    , MonoType(..)
    , PackageName
    , ResolverAmbiguity
    , SuperType(..)
    , Type(..)
    , TypeResolver
    , TypeVar
    , TypeVarStyle(..)
    , closeOver
    , collapseExtensible
    , collapsePrimitive
    , external
    , freeVars
    , freeVarsMono
    , freeVarsTypeEnv
    , freshVar
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
    , parseVarName
    , recurse
    , superTypeToString
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
import Elm.TypeInference.ImplicitImports as ImplicitImports
import Elm.TypeInference.VarName exposing (VarName)
import List.ExtraExtra
import Result.Extra
import Transform


type alias Id =
    Int


{-| "" for the first-party project being inferred,
"foo/bar" for dependencies from docs.json
-}
type alias PackageName =
    String


{-| Type/module collision
-}
type alias ResolverAmbiguity =
    { moduleName : String
    , possiblePackages : List PackageName
    }


type alias TypeResolver =
    List String -> String -> Result ResolverAmbiguity ( PackageName, FullModuleName )


type FromTypeAnnotationError
    = ImpossibleAnnotation TypeAnnotation
    | AmbiguousModuleName ResolverAmbiguity


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


{-| When instantiating, we want to keep the supertype (eg. `number`) when making
the fresh variable.
-}
freshVar : SuperType -> Id -> MonoType
freshVar super theId =
    TypeVar ( Generated theId, super )


{-| `Int`, `Float`, `Char`, `String`, `Bool` and `List` are deliberately kept as
`MonoType` primitives instead of `UserDefinedType`, since that would make `elm/core`
`docs.json` a hard prerequisite for all inference and turn `Unify`'s primitive arms
into string comparisons. We actually do the inverse - turn qualified `Basics.Int`
into the primitive in `collapsePrimitive`.
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
    | {- Int | Float | Char | String | List comparable | tuples of comparables -} Comparable
    | {- String | List a -} Appendable
    | {- String | List comparable -} CompAppend


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
        { package : PackageName
        , moduleName : FullModuleName
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


external : PackageName -> FullModuleName -> VarName -> MonoType
external package moduleName typeName =
    UserDefinedType
        { package = package
        , moduleName = moduleName
        , name = typeName
        , args = []
        }


mono : MonoType -> Type
mono =
    Forall []


{-| Extensible record on top of a closed record is a closed record:
`{ r | a : Float }` with `r = { b : Char }` is `{ a : Float, b : Char }`

Bias towards the outer fields.

-}
collapseExtensible : MonoType -> MonoType
collapseExtensible type_ =
    case type_ of
        ExtensibleRecord r ->
            case r.type_ of
                Record baseFields ->
                    Record (Dict.union r.fields baseFields)

                ExtensibleRecord _ ->
                    type_

                TypeVar _ ->
                    type_

                Function _ ->
                    type_

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

                List _ ->
                    type_

                Unit ->
                    type_

                Tuple _ _ ->
                    type_

                Tuple3 _ _ _ ->
                    type_

                UserDefinedType _ ->
                    type_

                WebGLShader _ ->
                    type_

        TypeVar _ ->
            type_

        Function _ ->
            type_

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

        List _ ->
            type_

        Unit ->
            type_

        Tuple _ _ ->
            type_

        Tuple3 _ _ _ ->
            type_

        Record _ ->
            type_

        UserDefinedType _ ->
            type_

        WebGLShader _ ->
            type_


{-| Converts `elm/core` `UserDefinedType` into a `MonoType` primitive
(Int, Float, Bool, Char, String, List).
-}
collapsePrimitive : PackageName -> FullModuleName -> VarName -> List MonoType -> Maybe MonoType
collapsePrimitive package moduleName name args =
    if package /= ImplicitImports.package then
        Nothing

    else
        case ( FullModuleName.toString moduleName, name, args ) of
            ( "Basics", "Int", [] ) ->
                Just Int

            ( "Basics", "Float", [] ) ->
                Just Float

            ( "Basics", "Bool", [] ) ->
                Just Bool

            ( "Char", "Char", [] ) ->
                Just Char

            ( "String", "String", [] ) ->
                Just String

            ( "List", "List", [ inner ] ) ->
                Just (List inner)

            _ ->
                Nothing



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
            List.ExtraExtra.fastConcatMap fn (Dict.values bindings)
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
            List.ExtraExtra.fastConcatMap fn args

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
    monoType
        |> generalize Set.empty


generalize : Set TypeVar -> MonoType -> Type
generalize envFreeVars monoType =
    let
        boundIds : List TypeVar
        boundIds =
            Set.diff
                (freeVarsMono monoType)
                envFreeVars
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

        {- Wraps a type in parentheses when it wouldn't parse back unambiguously
           in argument position (of `->` or of a type constructor application).
        -}
        wrapped t =
            case t of
                Function _ ->
                    "(" ++ f t ++ ")"

                UserDefinedType r ->
                    if List.isEmpty r.args then
                        f t

                    else
                        "(" ++ f t ++ ")"

                _ ->
                    f t
    in
    case type_ of
        TypeVar var ->
            varToString var

        Function { from, to } ->
            -- `->` is right-associative, so only the left side is ambiguous
            wrapped from ++ " -> " ++ f to

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
            ((FullModuleName.toString r.moduleName ++ "." ++ r.name)
                :: List.map wrapped r.args
            )
                |> String.join " "

        WebGLShader r ->
            "Shader "
                ++ String.join " "
                    [ f (Record r.attributes)
                    , f (Record r.uniforms)
                    , f (Record r.varyings)
                    ]


{-| The name of the Elm typeclass a type variable is constrained by.

`Normal` (an unconstrained variable) has no such name; we say "any type" since
that's what it accepts.

-}
superTypeToString : SuperType -> String
superTypeToString super =
    case super of
        Normal ->
            "any type"

        Number ->
            "number"

        Comparable ->
            "comparable"

        Appendable ->
            "appendable"

        CompAppend ->
            "compappend"


varToString : TypeVar -> String
varToString ( style, super ) =
    let
        prefix =
            if super == Normal then
                ""

            else
                superTypeToString super
    in
    case ( super, style ) of
        ( Normal, Generated theId ) ->
            "#" ++ String.fromInt theId

        ( Normal, Named name ) ->
            name

        ( _, Generated theId ) ->
            prefix ++ "#" ++ String.fromInt theId

        ( _, Named name ) ->
            prefix ++ name


{-| Parse typevar; honor Elm's typeclasses (`number`, `comparable`, `appendable`, `compappend`).
Possibly followed by a disambiguating suffix (`number1`).
-}
parseVarName : String -> TypeVar
parseVarName name =
    let
        prefixes : List ( String, SuperType )
        prefixes =
            [ ( "compappend", CompAppend )
            , ( "comparable", Comparable )
            , ( "appendable", Appendable )
            , ( "number", Number )
            ]
    in
    prefixes
        |> List.filterMap
            (\( prefix, super ) ->
                if String.startsWith prefix name then
                    Just ( Named (String.dropLeft (String.length prefix) name), super )

                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault ( Named name, Normal )


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


fromTypeAnnotation : TypeResolver -> TypeAnnotation -> Result FromTypeAnnotationError MonoType
fromTypeAnnotation resolver typeAnnotation =
    let
        f : TypeAnnotation -> Result FromTypeAnnotationError MonoType
        f annotation =
            fromTypeAnnotation resolver annotation

        recordBindings :
            List (Node ( Node String, Node TypeAnnotation ))
            -> Result FromTypeAnnotationError (Dict VarName MonoType)
        recordBindings fields =
            fields
                |> List.map
                    (\fieldNode ->
                        let
                            ( fieldNameNode, annotationNode ) =
                                Node.value fieldNode

                            type_ : Result FromTypeAnnotationError MonoType
                            type_ =
                                f (Node.value annotationNode)
                        in
                        type_
                            |> Result.map (\type__ -> ( Node.value fieldNameNode, type__ ))
                    )
                |> Result.Extra.combine
                |> Result.map Dict.fromList
    in
    case typeAnnotation of
        TypeAnnotation.GenericType name ->
            Ok <| TypeVar (parseVarName name)

        TypeAnnotation.Typed name annotations ->
            let
                ( moduleName, typeName ) =
                    Node.value name

                args : Result FromTypeAnnotationError (List MonoType)
                args =
                    annotations
                        |> List.map (Node.value >> f)
                        |> Result.Extra.combine
            in
            case ( moduleName, typeName ) of
                ( [], "Int" ) ->
                    Ok Int

                ( [], "Float" ) ->
                    Ok Float

                ( [], "Bool" ) ->
                    Ok Bool

                ( [], "Char" ) ->
                    Ok Char

                ( [], "String" ) ->
                    Ok String

                ( [], "List" ) ->
                    case annotations of
                        [ single ] ->
                            f (Node.value single) |> Result.map List

                        _ ->
                            Err (ImpossibleAnnotation typeAnnotation)

                _ ->
                    Result.andThen
                        (\args_ ->
                            resolver moduleName typeName
                                |> Result.mapError AmbiguousModuleName
                                |> Result.map
                                    (\( package, fullModuleName ) ->
                                        collapsePrimitive package fullModuleName typeName args_
                                            |> Maybe.withDefault
                                                (UserDefinedType
                                                    { package = package
                                                    , moduleName = fullModuleName
                                                    , name = typeName
                                                    , args = args_
                                                    }
                                                )
                                    )
                        )
                        args

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
            Err (ImpossibleAnnotation typeAnnotation)

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
