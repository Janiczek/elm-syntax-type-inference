module Elm.TypeInference.VarSet exposing
    ( GenKey
    , NamedKey
    , genKeyFrom
    , namedKeyFrom
    , superTypeTag
    , toList
    )

{-| An ordered set of `TypeVar`s, and the `TypeVar` identity it's keyed on.
-}

import Elm.TypeInference.TypeVar
    exposing
        ( SuperType(..)
        , TypeVar
        , TypeVarStyle(..)
        )
import Set exposing (Set)


{-| `comparable` encoding of a generated `TypeVar`: `id * 5 + superTypeTag`.
-}
type alias GenKey =
    Int


{-| `comparable` encoding of a named `TypeVar`: `(superTypeTag, name)`.
-}
type alias NamedKey =
    ( Int, String )


{-|

    genKeyFrom 5 Number --> 5 * 5 + 1 == 26

-}
genKeyFrom : Int -> SuperType -> GenKey
genKeyFrom theId superType =
    theId * 5 + superTypeTag superType


{-|

    namedKeyFrom "hello" Comparable --> (2, "hello")

-}
namedKeyFrom : String -> SuperType -> NamedKey
namedKeyFrom name superType =
    ( superTypeTag superType, name )


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


toList : List TypeVar -> List TypeVar
toList order =
    toListHelp Set.empty Set.empty order []


toListHelp : Set GenKey -> Set NamedKey -> List TypeVar -> List TypeVar -> List TypeVar
toListHelp seenGen seenNamed remaining acc =
    case remaining of
        [] ->
            List.reverse acc

        (( style, super ) as var) :: rest ->
            case style of
                Generated theId ->
                    let
                        k : GenKey
                        k =
                            genKeyFrom theId super
                    in
                    if Set.member k seenGen then
                        toListHelp seenGen seenNamed rest acc

                    else
                        toListHelp (Set.insert k seenGen) seenNamed rest (var :: acc)

                Named name ->
                    let
                        k : NamedKey
                        k =
                            namedKeyFrom name super
                    in
                    if Set.member k seenNamed then
                        toListHelp seenGen seenNamed rest acc

                    else
                        toListHelp seenGen (Set.insert k seenNamed) rest (var :: acc)
