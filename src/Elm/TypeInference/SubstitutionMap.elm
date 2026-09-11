module Elm.TypeInference.SubstitutionMap exposing
    ( SubstitutionMap
    , compose
    , empty
    , fromList
    , singleton
    , substitute
    , substituteMono
    , substituteTypeEnv
    )

import Dict exposing (Dict)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Type as Type
    exposing
        ( MonoType(..)
        , SuperType(..)
        , Type(..)
        , TypeVar
        , TypeVarStyle(..)
        )


type alias SubstitutionMap =
    -- Values are `MonoType` (not `Type`) because Elm doesn't have higher-rank polymorphism.
    Dict Key MonoType


type alias Key =
    -- a comparable encoding of TypeVar
    ( Int, Int, String )


{-|

     (Generated 5, Number) --> (1 {- Number -}, 5, "")
     (Named "hello", Comparable) --> (2 {- Comparable -}, 0, "hello")

-}
key : TypeVar -> Key
key ( style, superType ) =
    case style of
        Generated id ->
            ( superTypeTag superType, id, "" )

        Named name ->
            ( superTypeTag superType + 5, 0, name )


superTypeTag : SuperType -> Int
superTypeTag superType =
    case superType of
        Normal ->
            0

        Number ->
            1

        Comparable ->
            2

        Appendable ->
            3

        CompAppend ->
            4


{-| Beware: left-biased. Bindings of `s1` win over the ones of `s2`.

We don't substitute s1 into s2's values. `substituteMono` resolves the chain later.

-}
compose : SubstitutionMap -> SubstitutionMap -> SubstitutionMap
compose s1 s2 =
    Dict.union s1 s2


fromList : List ( TypeVar, MonoType ) -> SubstitutionMap
fromList list =
    List.foldl
        (\( var, type_ ) acc -> Dict.insert (key var) type_ acc)
        empty
        list


empty : SubstitutionMap
empty =
    Dict.empty


singleton : TypeVar -> MonoType -> SubstitutionMap
singleton var type_ =
    Dict.singleton (key var) type_


substitute : SubstitutionMap -> Type -> Type
substitute subst (Forall boundIds monoType) =
    let
        subst_ : SubstitutionMap
        subst_ =
            List.foldl (\var acc -> Dict.remove (key var) acc) subst boundIds
    in
    Forall boundIds <| substituteMono subst_ monoType


substituteMono : SubstitutionMap -> MonoType -> MonoType
substituteMono substitutions monoType =
    case monoType of
        -- The main interesting part
        TypeVar var ->
            case resolveVar substitutions var of
                Nothing ->
                    monoType

                Just replacement ->
                    -- The replacement can itself mention substituted vars.
                    substituteMono substitutions replacement

        -- The rest are just recursion
        Function { from, to } ->
            collapse <|
                Function
                    { from = substituteMono substitutions from
                    , to = substituteMono substitutions to
                    }

        Int ->
            monoType

        Float ->
            monoType

        Char ->
            monoType

        String ->
            monoType

        Bool ->
            monoType

        List listItemType ->
            collapse <| List (substituteMono substitutions listItemType)

        Unit ->
            monoType

        Tuple t1 t2 ->
            collapse <|
                Tuple
                    (substituteMono substitutions t1)
                    (substituteMono substitutions t2)

        Tuple3 t1 t2 t3 ->
            collapse <|
                Tuple3
                    (substituteMono substitutions t1)
                    (substituteMono substitutions t2)
                    (substituteMono substitutions t3)

        Record fields ->
            collapse <| Record (substituteFields substitutions fields)

        ExtensibleRecord r ->
            collapse <|
                ExtensibleRecord
                    { type_ = substituteMono substitutions r.type_
                    , fields = substituteFields substitutions r.fields
                    }

        UserDefinedType r ->
            collapse <|
                UserDefinedType
                    { r | args = List.map (substituteMono substitutions) r.args }

        WebGLShader r ->
            collapse <|
                WebGLShader
                    { attributes = substituteFields substitutions r.attributes
                    , uniforms = substituteFields substitutions r.uniforms
                    , varyings = substituteFields substitutions r.varyings
                    }


substituteFields : SubstitutionMap -> Dict VarName MonoType -> Dict VarName MonoType
substituteFields substitutions fields =
    Dict.map (always (substituteMono substitutions)) fields


collapse : MonoType -> MonoType
collapse =
    Type.collapseExtensible


{-| Follow a chain of vars.
Tail-recursive. The occurs check in `Unify.bind` keeps the chains from looping.
-}
resolveVar : SubstitutionMap -> TypeVar -> Maybe MonoType
resolveVar substitutions var =
    resolveVarHelp substitutions Nothing var


resolveVarHelp : SubstitutionMap -> Maybe MonoType -> TypeVar -> Maybe MonoType
resolveVarHelp substitutions lastFound var =
    case Dict.get (key var) substitutions of
        Nothing ->
            lastFound

        Just ((TypeVar nextVar) as found) ->
            resolveVarHelp substitutions (Just found) nextVar

        Just found ->
            Just found


substituteTypeEnv : SubstitutionMap -> Dict VarName Type -> Dict VarName Type
substituteTypeEnv substitutions env =
    Dict.map (always (substitute substitutions)) env
