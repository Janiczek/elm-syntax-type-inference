module TypeLookupTable.Internal exposing
    ( TypeLookupTable(..)
    , empty
    , toRangeLike
    , unionSingle
    )

import Bitwise
import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.Type exposing (Type)


type TypeLookupTable
    = TypeLookupTable ModuleName (Dict RangeLike Type)


type alias RangeLike =
    ( Int, Int )


empty : ModuleName -> TypeLookupTable
empty moduleName =
    TypeLookupTable moduleName Dict.empty


{-| Throws away module name information from the first table.
-}
unionSingle : TypeLookupTable -> TypeLookupTable -> TypeLookupTable
unionSingle (TypeLookupTable _ dictA) (TypeLookupTable moduleName dictB) =
    TypeLookupTable moduleName (Dict.union dictA dictB)


toRangeLike : Range -> RangeLike
toRangeLike { start, end } =
    {- shamelessly stolen from Jeroen:
       https://github.com/jfmengels/elm-review/blob/2.9.1/src/Review/ModuleNameLookupTable/Internal.elm
    -}
    ( Bitwise.shiftLeftBy 16 start.row + start.column
    , Bitwise.shiftLeftBy 16 end.row + end.column
    )
