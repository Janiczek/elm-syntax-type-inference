module TypeLookupTable exposing (TypeLookupTable, get, empty)

{-|

@docs TypeLookupTable, get, empty

-}

import Dict
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type exposing (Type)
import Elm.TypeInference.Type.Internal as TypeI
import RangeLike
import TypeLookupTable.Internal as Internal


{-| A lazy lookup table: after the initial collection of type equations is done
in `Elm.TypeInference.inferModule`, we compute the type on demand when the user
calls `TypeLookupTable.get`.
-}
type alias TypeLookupTable =
    Internal.TypeLookupTable


{-| An empty lookup table. Not for direct use.

Useful for when a value of this type is needed to satisfy the compiler but will never be used.

-}
empty : TypeLookupTable
empty =
    Internal.empty


{-| Look up the inferred `Type` for a source `Range`.

Only exact ranges given by `Node.range` of AST nodes inside the input
`Elm.Syntax.File`s are recorded. There is no overlap matching.

-}
get : Range -> TypeLookupTable -> ( Maybe Type, TypeLookupTable )
get range (Internal.TLT tlt) =
    let
        rangeLike : RangeLike.RangeLike
        rangeLike =
            RangeLike.fromRange range
    in
    case Dict.get rangeLike tlt.nodeIds of
        Nothing ->
            ( Nothing, Internal.TLT tlt )

        Just id ->
            case Dict.get id tlt.cache of
                Just cached ->
                    ( Just cached, Internal.TLT tlt )

                Nothing ->
                    let
                        ( monoType, _, subst1 ) =
                            SubstitutionMap.substituteMono tlt.subst (TypeI.id_ id)

                        key : String
                        key =
                            TypeI.monoPublicKey { alreadyNormalized = False } monoType

                        ( pubType, pool1 ) =
                            case Dict.get key tlt.pool of
                                Just canonical ->
                                    ( canonical, tlt.pool )

                                Nothing ->
                                    let
                                        fresh : Type
                                        fresh =
                                            TypeI.toPublicType tlt.moduleMapping { alreadyNormalized = False } monoType
                                    in
                                    ( fresh, Dict.insert key fresh tlt.pool )
                    in
                    ( Just pubType
                    , Internal.TLT
                        { nodeIds = tlt.nodeIds
                        , subst = subst1
                        , moduleMapping = tlt.moduleMapping
                        , cache = Dict.insert id pubType tlt.cache
                        , pool = pool1
                        }
                    )
