module Elm.TypeInference.SubstitutionMap exposing
    ( SubstitutionMap
    , compose
    , empty
    , fromList
    , singleton
    , substitute
    , substituteMono
    , substituteTypeEnv
    , substituteTypeEquation
    )

import AssocList
import Dict exposing (Dict)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.Type as Type
    exposing
        ( MonoType(..)
        , Type(..)
        , TypeVar
        )
import Transform


type alias SubstitutionMap =
    -- TODO shouldn't this be to Type instead?
    AssocList.Dict TypeVar MonoType


{-| Beware: left-biased
-}
compose : SubstitutionMap -> SubstitutionMap -> SubstitutionMap
compose s1 s2 =
    AssocList.map (always (substituteMono s1)) s2
        |> AssocList.union s1


fromList : List ( TypeVar, MonoType ) -> SubstitutionMap
fromList list =
    List.foldl
        (\( var, type_ ) acc -> compose (singleton var type_) acc)
        empty
        list


empty : SubstitutionMap
empty =
    AssocList.empty


singleton : TypeVar -> MonoType -> SubstitutionMap
singleton var type_ =
    AssocList.singleton var type_


substitute : SubstitutionMap -> Type -> Type
substitute subst (Forall boundIds monoType) =
    let
        subst_ =
            List.foldl AssocList.remove subst boundIds
    in
    Forall boundIds <| substituteMono subst_ monoType


substituteMono : SubstitutionMap -> MonoType -> MonoType
substituteMono substitutions monoType =
    let
        substituteMono_ : MonoType -> MonoType
        substituteMono_ type_ =
            case type_ of
                TypeVar var ->
                    AssocList.get var substitutions
                        |> Maybe.withDefault type_

                _ ->
                    type_
    in
    Transform.transformOnce
        Type.recurse
        substituteMono_
        monoType


substituteTypeEnv : SubstitutionMap -> Dict VarName Type -> Dict VarName Type
substituteTypeEnv substitutions env =
    Dict.map (always (substitute substitutions)) env


substituteTypeEquation : AssocList.Dict TypeVar MonoType -> ( Type, Type ) -> ( Type, Type )
substituteTypeEquation substitutions ( t1, t2 ) =
    ( substitute substitutions t1
    , substitute substitutions t2
    )
