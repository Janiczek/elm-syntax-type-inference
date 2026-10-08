module Elm.TypeInference.Naming exposing
    ( Naming
    , addInstantiated
    , addQuantifiedByLet
    , empty
    )

{-| Information about quantified (polymorphic, forall) vars, used to name them nicely later.
-}

import Dict exposing (Dict)
import Elm.TypeInference.Type.Internal exposing (Id)
import Elm.TypeInference.TypeVar exposing (TypeVar, TypeVarStyle(..))
import RangeLike exposing (RangeLike)


type alias Naming =
    { quantifiedByLet : Dict Id RangeLike
    , freshInstances : Dict Id (List Id)
    }


empty : Naming
empty =
    { quantifiedByLet = Dict.empty
    , freshInstances = Dict.empty
    }


addQuantifiedByLet : RangeLike -> List TypeVar -> Naming -> Naming
addQuantifiedByLet range vars naming =
    { quantifiedByLet =
        List.foldl
            (\( style, _ ) acc ->
                case style of
                    Generated theId ->
                        Dict.insert theId range acc

                    Named _ ->
                        acc
            )
            naming.quantifiedByLet
            vars
    , freshInstances = naming.freshInstances
    }


addInstantiated : List ( TypeVar, TypeVar ) -> Naming -> Naming
addInstantiated renaming naming =
    { quantifiedByLet = naming.quantifiedByLet
    , freshInstances =
        List.foldl
            (\( ( style, _ ), ( freshStyle, _ ) ) acc ->
                case ( style, freshStyle ) of
                    ( Generated boundId, Generated freshId ) ->
                        Dict.update boundId (Maybe.withDefault [] >> (::) freshId >> Just) acc

                    _ ->
                        acc
            )
            naming.freshInstances
            renaming
    }
