module TypeLookupTable exposing
    ( TypeLookupTable
    , fromDict
    , get
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.Type exposing (Type)
import RangeLike exposing (RangeLike)


type TypeLookupTable
    = TypeLookupTable ModuleName (Dict RangeLike Type)


get : Range -> TypeLookupTable -> Maybe Type
get range (TypeLookupTable _ dict) =
    Dict.get (RangeLike.fromRange range) dict


fromDict : ModuleName -> Dict RangeLike Type -> TypeLookupTable
fromDict moduleName dict =
    TypeLookupTable moduleName dict
