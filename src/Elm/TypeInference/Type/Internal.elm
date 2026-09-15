module Elm.TypeInference.Type.Internal exposing
    ( Id
    , MonoType(..)
    , PackageName
    , Type(..)
    , TypeResolver
    , closeOver
    , collapseExtensible
    , collapsePrimitive
    , external
    , freeVarsMono
    , fromTypeAnnotation
    , fromTypeAnnotationError
    , id_
    , mapVarsMono
    , mono
    , number_
    , toPublicPair
    , toPublicType
    )

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Error exposing (ErrorDetails(..))
import Elm.TypeInference.Error.Internal exposing (FromTypeAnnotationError(..), ResolverAmbiguity)
import Elm.TypeInference.ImplicitImports as ImplicitImports
import Elm.TypeInference.Type as Public
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
import Set exposing (Set)


type alias Id =
    Int


{-| "" for the first-party project being inferred,
"foo/bar" for dependencies from docs.json
-}
type alias PackageName =
    String


type alias TypeResolver =
    List String -> String -> Result ResolverAmbiguity ( PackageName, FullModuleName )


id_ : Id -> MonoType
id_ theId =
    TypeVar ( Generated theId, Normal )


number_ : Id -> MonoType
number_ theId =
    TypeVar ( Generated theId, Number )


type Type
    = Forall (List TypeVar) MonoType


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
    | Tuple2 MonoType MonoType
    | Tuple3 MonoType MonoType MonoType
    | Record { fields : Dict VarName MonoType }
    | ExtensibleRecord
        { extensionTypevar : MonoType
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


{-| Canonicalize an extensible-record chain:

  - `{ r | }` (no fields) is just `r`
  - `{ { b : Char } | a : Float }` is `{ a : Float, b : Char }`
  - `{ { s | b : Char } | a : Float }` is `{ s | a : Float, b : Char }`

Bias towards the outer fields.

-}
collapseExtensible : MonoType -> MonoType
collapseExtensible type_ =
    case type_ of
        ExtensibleRecord r1 ->
            if Dict.isEmpty r1.fields then
                collapseExtensible r1.extensionTypevar

            else
                case r1.extensionTypevar of
                    Record r2 ->
                        Record { fields = Dict.union r1.fields r2.fields }

                    ExtensibleRecord r2 ->
                        collapseExtensible <|
                            ExtensibleRecord
                                { extensionTypevar = r2.extensionTypevar
                                , fields = Dict.union r1.fields r2.fields
                                }

                    _ ->
                        type_

        _ ->
            type_


{-| Converts `elm/core` `UserDefinedType` into a `MonoType` primitive
(Int, Float, Bool, Char, String, List).
-}
collapsePrimitive : PackageName -> FullModuleName -> VarName -> List MonoType -> Maybe MonoType
collapsePrimitive package moduleName name args =
    if package /= ImplicitImports.elmCorePackage then
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

        Tuple2 t1 t2 ->
            Tuple2 (f t1) (f t2)

        Tuple3 t1 t2 t3 ->
            Tuple3 (f t1) (f t2) (f t3)

        Record { fields } ->
            Record { fields = Dict.map (always f) fields }

        ExtensibleRecord r ->
            ExtensibleRecord
                { extensionTypevar = f r.extensionTypevar
                , fields = Dict.map (always f) r.fields
                }

        UserDefinedType r ->
            UserDefinedType
                { package = r.package
                , moduleName = r.moduleName
                , name = r.name
                , args = List.map f r.args
                }

        WebGLShader r ->
            WebGLShader
                { attributes = Dict.map (always f) r.attributes
                , uniforms = Dict.map (always f) r.uniforms
                , varyings = Dict.map (always f) r.varyings
                }


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

        Tuple2 t1 t2 ->
            acc
                |> freeVarsMonoHelp t2
                |> freeVarsMonoHelp t1

        Tuple3 t1 t2 t3 ->
            acc
                |> freeVarsMonoHelp t3
                |> freeVarsMonoHelp t2
                |> freeVarsMonoHelp t1

        Record { fields } ->
            inFields fields acc

        ExtensibleRecord r ->
            acc
                |> inFields r.fields
                |> freeVarsMonoHelp r.extensionTypevar

        UserDefinedType r ->
            List.foldr freeVarsMonoHelp acc r.args

        WebGLShader r ->
            acc
                |> inFields r.varyings
                |> inFields r.uniforms
                |> inFields r.attributes


closeOver : MonoType -> Type
closeOver monoType =
    monoType
        |> generalize VarSet.empty


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


normalize : Type -> Type
normalize ((Forall boundVars monoType) as type_) =
    let
        allVars : List TypeVar
        allVars =
            VarSet.union
                (freeVarsMono monoType)
                (VarSet.fromList boundVars)
                |> VarSet.toList

        -- eg. `number` and `comparable` get their own slot sequence independent of the `Normal` one
        usedNamesBySuper : Dict String (Set String)
        usedNamesBySuper =
            allVars
                |> List.foldl
                    (\( style, super ) acc ->
                        case style of
                            Named name ->
                                Dict.update
                                    (TypeVar.superTypeToString super)
                                    (\existing -> Just (Set.insert name (Maybe.withDefault Set.empty existing)))
                                    acc

                            Generated _ ->
                                acc
                    )
                    Dict.empty

        -- slot 0 is the bare word ("a", or "" for supertypes -> just "number");
        -- slot >= 1 is "b", "c", ... or "1", "2", ... for supertypes.
        nameForSlot : SuperType -> Int -> String
        nameForSlot super slot =
            case super of
                Normal ->
                    ordToName slot

                _ ->
                    if slot == 0 then
                        ""

                    else
                        String.fromInt slot

        nextFreeSlot : SuperType -> Int -> Int
        nextFreeSlot super slot =
            let
                used : Set String
                used =
                    Dict.get (TypeVar.superTypeToString super) usedNamesBySuper
                        |> Maybe.withDefault Set.empty
            in
            if Set.member (nameForSlot super slot) used then
                nextFreeSlot super (slot + 1)

            else
                slot

        newVars : List TypeVar
        newVars =
            allVars
                |> List.foldl
                    (\(( style, super ) as var) ( nextSlotBySuper, acc ) ->
                        case style of
                            Named _ ->
                                -- Leave it exactly as it is.
                                ( nextSlotBySuper, var :: acc )

                            Generated _ ->
                                let
                                    key : String
                                    key =
                                        TypeVar.superTypeToString super

                                    startSlot : Int
                                    startSlot =
                                        Dict.get key nextSlotBySuper |> Maybe.withDefault 0

                                    slot : Int
                                    slot =
                                        nextFreeSlot super startSlot
                                in
                                ( Dict.insert key (slot + 1) nextSlotBySuper
                                , ( Named (nameForSlot super slot), super ) :: acc
                                )
                    )
                    ( Dict.empty, [] )
                |> (\( _, vars ) -> List.reverse vars)

        subst : Dict VarKey TypeVar
        subst =
            List.map2 (\var newVar -> ( varKey var, newVar ))
                allVars
                newVars
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
            Result.map2 Tuple2
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
                |> Result.map (\fields_ -> Record { fields = fields_ })

        TypeAnnotation.GenericRecord name fields ->
            recordBindings (Node.value fields)
                |> Result.map
                    (\fields_ ->
                        ExtensibleRecord
                            { extensionTypevar = TypeVar ( Named (Node.value name), Normal )
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


{-| Convert a type-annotation conversion failure into an inference error.
-}
fromTypeAnnotationError : FromTypeAnnotationError -> ErrorDetails
fromTypeAnnotationError err =
    case err of
        ImpossibleAnnotation typeAnnotation ->
            ImpossibleType typeAnnotation

        AmbiguousModuleName ambiguity ->
            AmbiguousModuleOwner ambiguity


toPublicType : { alreadyNormalized : Bool } -> MonoType -> Public.Type
toPublicType { alreadyNormalized } origMono =
    let
        mono_ : MonoType
        mono_ =
            if alreadyNormalized then
                origMono

            else
                let
                    (Forall _ normalizedMono) =
                        normalize (Forall [] origMono)
                in
                normalizedMono
    in
    toPublicTypeAux mono_


{-| Convert two `MonoType`s to public `Type`s with a shared normalization.

Normalizing each side independently would name distinct variables identically
(`a` on both sides) and suggest sharing where there is none -- or rename a
shared variable differently on each side. Normalizing `Tuple2 t1 t2` once and
splitting keeps one naming scope for both, so equal vars stay equal and
distinct vars stay distinct across the pair.

Used for type-error payloads, which always come in pairs.

-}
toPublicPair : MonoType -> MonoType -> ( Public.Type, Public.Type )
toPublicPair t1 t2 =
    let
        (Forall _ normalizedCombined) =
            normalize (Forall [] (Tuple2 t1 t2))
    in
    case normalizedCombined of
        Tuple2 nt1 nt2 ->
            ( toPublicType { alreadyNormalized = True } nt1
            , toPublicType { alreadyNormalized = True } nt2
            )

        _ ->
            ( toPublicType { alreadyNormalized = False } t1
            , toPublicType { alreadyNormalized = False } t2
            )


toPublicTypeAux : MonoType -> Public.Type
toPublicTypeAux mono_ =
    let
        f : MonoType -> Public.Type
        f =
            toPublicType { alreadyNormalized = True }
    in
    case mono_ of
        TypeVar typeVar ->
            Public.TypeVar (TypeVar.toString typeVar)

        Function { from, to } ->
            Public.Function
                { from = f from
                , to = f to
                }

        Int ->
            Public.Int

        Float ->
            Public.Float

        Char ->
            Public.Char

        String ->
            Public.String

        Bool ->
            Public.Bool

        List ts ->
            Public.List (f ts)

        Unit ->
            Public.Unit

        Tuple2 t1 t2 ->
            Public.Tuple2
                (f t1)
                (f t2)

        Tuple3 t1 t2 t3 ->
            Public.Tuple3
                (f t1)
                (f t2)
                (f t3)

        Record { fields } ->
            Public.Record { fields = Dict.map (\_ v -> f v) fields }

        ExtensibleRecord { extensionTypevar, fields } ->
            Public.ExtensibleRecord
                { extensionTypevar =
                    case
                        extensionTypevar
                    of
                        TypeVar var ->
                            TypeVar.toString var

                        _ ->
                            -- Should be impossible for compiling code;
                            -- could happen for manually created MonoType values
                            -- TODO should we be more explicit in the type definition? ie. TypeVar instead of MonoType in the extensible record thingy
                            "<elm-syntax-type-inference bug: non-var as extensible record base [2]>"
                , fields = fields |> Dict.map (\_ v -> f v)
                }

        UserDefinedType r ->
            Public.Named
                { package = r.package
                , moduleName = FullModuleName.toModuleName r.moduleName
                , name = r.name
                , arguments = List.map f r.args
                }

        WebGLShader r ->
            Public.WebGLShader
                { attributes = r.attributes |> Dict.map (\_ v -> f v)
                , uniforms = r.uniforms |> Dict.map (\_ v -> f v)
                , varyings = r.varyings |> Dict.map (\_ v -> f v)
                }
