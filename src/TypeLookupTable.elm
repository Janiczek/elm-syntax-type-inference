module TypeLookupTable exposing
    ( TypeLookupTable
    , empty
    , fromList
    , get
    , insert
    , union
    )

import Bitwise
import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.Type exposing (Type)
import NonemptyList exposing (NonemptyList)


type TypeLookupTable
    = TypeLookupTable ModuleName (Dict RangeLike ( Type, String ))


{-| This represents (from, to) where each number contains both the row and column.
-}
type alias RangeLike =
    ( Int, Int )


get : Range -> TypeLookupTable -> Maybe Type
get range (TypeLookupTable _ dict) =
    Dict.get (toRangeLike range) dict
        |> Maybe.map Tuple.first


union : NonemptyList TypeLookupTable -> TypeLookupTable
union ( fst, rest ) =
    List.foldl unionSingle fst rest


{-| Throws away module name information from the first table.
-}
unionSingle : TypeLookupTable -> TypeLookupTable -> TypeLookupTable
unionSingle (TypeLookupTable _ dictA) (TypeLookupTable moduleName dictB) =
    TypeLookupTable moduleName (Dict.union dictA dictB)


empty : ModuleName -> TypeLookupTable
empty moduleName =
    TypeLookupTable moduleName Dict.empty


toRangeLike : Range -> RangeLike
toRangeLike { start, end } =
    {- shamelessly stolen from Jeroen:
       https://github.com/jfmengels/elm-review/blob/2.9.1/src/Review/ModuleNameLookupTable/Internal.elm
    -}
    ( Bitwise.shiftLeftBy 16 start.row + start.column
    , Bitwise.shiftLeftBy 16 end.row + end.column
    )


insert : Range -> Type -> String -> TypeLookupTable -> TypeLookupTable
insert range type_ debugInfo (TypeLookupTable moduleName dict) =
    TypeLookupTable moduleName
        (Dict.insert (toRangeLike range) ( type_, debugInfo ) dict)


fromList : ModuleName -> List ( Range, Type, String ) -> TypeLookupTable
fromList moduleName list =
    List.foldl
        (\( range, type_, debugInfo ) acc -> insert range type_ debugInfo acc)
        (empty moduleName)
        list
