module Elm.TypeInference.Type exposing
    ( Type(..), MonoType(..)
    , PackageName
    , FromTypeAnnotationError(..), Id, ResolverAmbiguity, TypeResolver, closeOver, collapseExtensible, collapsePrimitive, external, freeVarsMono, fromTypeAnnotation, id_, mapVarsMono, mono, monoTypeToString, normalize, number_, toString, toTypeAnnotation
    )

{-| A data structure representing the Elm types.

This module is not named `Elm.Type` because that already exists in elm/project-metadata-utils.

TODO put most of these into Internal, and keep this only the outwards-facing API

@docs Type, MonoType
@docs PackageName


# TODO move to Internal

@docs FromTypeAnnotationError, Id, ResolverAmbiguity, TypeResolver, closeOver, collapseExtensible, collapsePrimitive, external, freeVarsMono, fromTypeAnnotation, id_, mapVarsMono, mono, monoTypeToString, normalize, number_, toString, toTypeAnnotation

-}

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.ImplicitImports as ImplicitImports
import Elm.TypeInference.TypeVar as TypeVar
    exposing
        ( SuperType(..)
        , TypeVar
        , TypeVarStyle(..)
        )
import Elm.TypeInference.VarSet as VarSet
    exposing
        ( VarKey
        , VarSet
        , varKey
        )
import Result.Extra


{-| TODO docs
-}
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


{-| TODO docs
-}
type alias TypeResolver =
    List String -> String -> Result ResolverAmbiguity ( PackageName, FullModuleName )


{-| TODO docs
-}
type FromTypeAnnotationError
    = ImpossibleAnnotation TypeAnnotation
    | AmbiguousModuleName ResolverAmbiguity


{-| TODO docs
-}
id_ : Id -> MonoType
id_ theId =
    TypeVar ( Generated theId, Normal )


{-| TODO docs
-}
number_ : Id -> MonoType
number_ theId =
    TypeVar ( Generated theId, Number )


{-| TODO docs
-}
type Type
    = Forall (List TypeVar) MonoType


{-| TODO docs
-}
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
        -- TODO do we need to also be able to support ExtensibleRecord here?
        -- See eg. https://github.com/elm-explorations/webgl/blob/main/README.md#writing-shaders
        { attributes : Dict VarName MonoType
        , uniforms : Dict VarName MonoType
        , varyings : Dict VarName MonoType
        }


{-| TODO docs
-}
external : PackageName -> FullModuleName -> VarName -> MonoType
external package moduleName typeName =
    UserDefinedType
        { package = package
        , moduleName = moduleName
        , name = typeName
        , args = []
        }


{-| TODO docs
-}
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



-- RECURSION HELPERS


{-| Apply `f` to the direct children of a type, keeping the type's shape.
-}
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


{-| TODO docs
-}
freeVarsMono : MonoType -> VarSet
freeVarsMono type_ =
    freeVarsMonoHelp type_ VarSet.empty


{-| Note: this walks the type backwards to insert into VarSet in a specific
order (the order of first appearance, to play nice with `normalize` - #0, #1,
a, b, ...)
-}
freeVarsMonoHelp : MonoType -> VarSet -> VarSet
freeVarsMonoHelp type_ acc =
    let
        inFields : Dict VarName MonoType -> VarSet -> VarSet
        inFields fields acc_ =
            Dict.foldr (\_ fieldType -> freeVarsMonoHelp fieldType) acc_ fields
    in
    case type_ of
        TypeVar typeVar ->
            VarSet.insert typeVar acc

        Function { from, to } ->
            acc
                |> freeVarsMonoHelp to
                |> freeVarsMonoHelp from

        Int ->
            acc

        Float ->
            acc

        Char ->
            acc

        String ->
            acc

        Bool ->
            acc

        List listItemType ->
            freeVarsMonoHelp listItemType acc

        Unit ->
            acc

        Tuple t1 t2 ->
            acc
                |> freeVarsMonoHelp t2
                |> freeVarsMonoHelp t1

        Tuple3 t1 t2 t3 ->
            acc
                |> freeVarsMonoHelp t3
                |> freeVarsMonoHelp t2
                |> freeVarsMonoHelp t1

        Record fields ->
            inFields fields acc

        ExtensibleRecord r ->
            acc
                |> inFields r.fields
                |> freeVarsMonoHelp r.type_

        UserDefinedType r ->
            List.foldr freeVarsMonoHelp acc r.args

        WebGLShader r ->
            acc
                |> inFields r.varyings
                |> inFields r.uniforms
                |> inFields r.attributes


{-| TODO docs
-}
closeOver : MonoType -> Type
closeOver monoType =
    monoType
        |> generalize VarSet.empty


{-| TODO docs
-}
generalize : VarSet -> MonoType -> Type
generalize envFreeVars monoType =
    let
        boundIds : List TypeVar
        boundIds =
            VarSet.diff
                (freeVarsMono monoType)
                envFreeVars
                |> VarSet.toList
    in
    Forall boundIds monoType


{-| TODO docs
-}
toString : Type -> String
toString (Forall boundVars monoType) =
    let
        preamble : String
        preamble =
            boundVars
                |> List.map (\var -> "∀" ++ TypeVar.toString var)
                |> String.join " "
                |> (\str ->
                        if String.isEmpty str then
                            str

                        else
                            str ++ ". "
                   )
    in
    preamble ++ monoTypeToString monoType


{-| TODO docs
-}
monoTypeToString : MonoType -> String
monoTypeToString type_ =
    let
        f : MonoType -> String
        f =
            monoTypeToString

        recordBindings : Dict VarName MonoType -> String
        recordBindings bindings =
            bindings
                |> Dict.toList
                |> List.map (\( fieldName, fieldType ) -> fieldName ++ " : " ++ f fieldType)
                |> String.join ", "

        {- Wraps a type in parentheses when it wouldn't parse back unambiguously
           in argument position (of `->` or of a type constructor application).
        -}
        wrapped : MonoType -> String
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
            TypeVar.toString var

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
                bindingsStr : String
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


{-| TODO docs
-}
normalize : Type -> Type
normalize ((Forall boundVars monoType) as type_) =
    let
        allVars : List TypeVar
        allVars =
            VarSet.union
                (freeVarsMono monoType)
                (VarSet.fromList boundVars)
                |> VarSet.toList

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

        -- We can't use SubstitutionMap.substituteMono because it works recursively
        -- We need to replace the vars just once and not follow the links.
        subst : Dict VarKey TypeVar
        subst =
            List.map2 (\var newVar -> ( varKey var, newVar )) allVars newVars
                |> Dict.fromList
    in
    type_
        |> mapVars
            (\var ->
                case Dict.get (varKey var) subst of
                    Nothing ->
                        var

                    Just newVar ->
                        newVar
            )


mapVars : (TypeVar -> TypeVar) -> Type -> Type
mapVars fn (Forall boundVars monoType) =
    Forall (List.map fn boundVars) (mapVarsMono fn monoType)


{-| Replace every var **once**, simultaneously -- no chain following.

That matters for `State.instantiate`: the fresh vars it maps a scheme's bound
vars to are drawn from the id counter of the module being inferred, while the
scheme's own bound ids come from whichever module defined it. The two id spaces
overlap, so a fresh id can collide with another bound id of the same scheme. A
chain-following substitution would then rename twice and collapse two distinct
quantified variables into one.

-}
mapVarsMono : (TypeVar -> TypeVar) -> MonoType -> MonoType
mapVarsMono fn type_ =
    case type_ of
        TypeVar var ->
            TypeVar (fn var)

        _ ->
            recurse (mapVarsMono fn) type_


ordToName : Int -> String
ordToName n =
    let
        radix : Int
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


{-| TODO docs
-}
toTypeAnnotation : Type -> TypeAnnotation
toTypeAnnotation (Forall _ mono_) =
    toTypeAnnotationMono mono_


toTypeAnnotationMono : MonoType -> TypeAnnotation
toTypeAnnotationMono mono_ =
    -- TODO non-dummy ranges
    case mono_ of
        TypeVar var ->
            TypeAnnotation.GenericType (TypeVar.toString var)

        Function { from, to } ->
            TypeAnnotation.FunctionTypeAnnotation
                (Node.empty (toTypeAnnotationMono from))
                (Node.empty (toTypeAnnotationMono to))

        Int ->
            TypeAnnotation.Typed (Node.empty ( [ "Basics" ], "Int" )) []

        Float ->
            TypeAnnotation.Typed (Node.empty ( [ "Basics" ], "Float" )) []

        Char ->
            TypeAnnotation.Typed (Node.empty ( [ "Basics" ], "Char" )) []

        String ->
            TypeAnnotation.Typed (Node.empty ( [ "Basics" ], "String" )) []

        Bool ->
            TypeAnnotation.Typed (Node.empty ( [ "Basics" ], "Bool" )) []

        List ts ->
            TypeAnnotation.Typed
                (Node.empty ( [ "List" ], "List" ))
                [ Node.empty (toTypeAnnotationMono ts) ]

        Unit ->
            TypeAnnotation.Unit

        Tuple t1 t2 ->
            TypeAnnotation.Tupled
                [ Node.empty (toTypeAnnotationMono t1)
                , Node.empty (toTypeAnnotationMono t2)
                ]

        Tuple3 t1 t2 t3 ->
            TypeAnnotation.Tupled
                [ Node.empty (toTypeAnnotationMono t1)
                , Node.empty (toTypeAnnotationMono t2)
                , Node.empty (toTypeAnnotationMono t3)
                ]

        Record fields ->
            TypeAnnotation.Record
                (recordFieldsToRecordDefinition fields)

        ExtensibleRecord r ->
            TypeAnnotation.GenericRecord
                -- the var needs to be stringified
                (Node.empty
                    (case
                        r.type_
                     of
                        TypeVar var ->
                            TypeVar.toString var

                        _ ->
                            -- Should be impossible for compiling code;
                            -- could happen for manually created MonoType values
                            -- TODO should we be more explicit in the type definition? ie. TypeVar instead of MonoType in the extensible record thingy
                            "<elm-syntax-type-inference bug: non-var as extensible record base>"
                    )
                )
                (Node.empty (recordFieldsToRecordDefinition r.fields))

        UserDefinedType r ->
            TypeAnnotation.Typed
                (Node.empty
                    ( FullModuleName.toModuleName r.moduleName
                    , r.name
                    )
                )
                (List.map (toTypeAnnotationMono >> Node.empty) r.args)

        WebGLShader r ->
            TypeAnnotation.Typed
                (Node.empty ( [ "WebGL" ], "Shader" ))
                ([ r.attributes
                 , r.uniforms
                 , r.varyings
                 ]
                    |> List.map
                        (recordFieldsToRecordDefinition
                            >> TypeAnnotation.Record
                            >> Node.empty
                        )
                )


recordFieldsToRecordDefinition : Dict VarName MonoType -> TypeAnnotation.RecordDefinition
recordFieldsToRecordDefinition fields =
    fields
        |> Dict.toList
        |> List.map
            (\( fieldName, fieldType ) ->
                Node.empty
                    ( Node.empty fieldName
                    , Node.empty (toTypeAnnotationMono fieldType)
                    )
            )


{-| TODO docs
-}
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
            Ok <| TypeVar (TypeVar.parse name)

        TypeAnnotation.Typed name annotations ->
            let
                ( moduleName, typeName ) =
                    Node.value name
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
                    let
                        args : Result FromTypeAnnotationError (List MonoType)
                        args =
                            annotations
                                |> List.map (Node.value >> f)
                                |> Result.Extra.combine
                    in
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
