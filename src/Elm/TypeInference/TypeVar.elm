module Elm.TypeInference.TypeVar exposing
    ( TypeVar, TypeVarStyle(..), SuperType(..)
    , parse
    , toString, superTypeToString
    )

{-|

@docs TypeVar, TypeVarStyle, SuperType
@docs parse
@docs toString, superTypeToString

-}


{-|

    x : a (in source code) == NormalVar "a"
    x : a (given by compiler) == NormalId 1
    x : number (in source code) == SuperVar Number ""
    x : number1 (in source code) == SuperVar Number "1"
    x : number (given by compiler) == SuperId Number 1

-}
type alias TypeVar =
    ( TypeVarStyle, SuperType )


{-| TODO docs
-}
type TypeVarStyle
    = Generated Int
    | Named String


{-| TODO docs
-}
type SuperType
    = Normal
    | {- Int | Float -} Number
    | {- Int | Float | Char | String | List comparable | tuples of comparables -} Comparable
    | {- String | List a -} Appendable
    | {- String | List comparable -} CompAppend


{-| TODO docs
-}
toString : TypeVar -> String
toString ( style, super ) =
    let
        prefix : String
        prefix =
            if super == Normal then
                ""

            else
                superTypeToString super
    in
    case ( super, style ) of
        ( Normal, Generated theId ) ->
            "#" ++ String.fromInt theId

        ( Normal, Named name ) ->
            name

        ( _, Generated theId ) ->
            prefix ++ "#" ++ String.fromInt theId

        ( _, Named name ) ->
            prefix ++ name


{-| TODO docs
-}
parse : String -> TypeVar
parse name =
    let
        prefixes : List ( String, SuperType )
        prefixes =
            [ ( "compappend", CompAppend )
            , ( "comparable", Comparable )
            , ( "appendable", Appendable )
            , ( "number", Number )
            ]
    in
    prefixes
        |> List.filterMap
            (\( prefix, super ) ->
                if String.startsWith prefix name then
                    Just ( Named (String.dropLeft (String.length prefix) name), super )

                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault ( Named name, Normal )


{-| TODO docs
-}
superTypeToString : SuperType -> String
superTypeToString super =
    case super of
        Normal ->
            "any type"

        Number ->
            "number"

        Comparable ->
            "comparable"

        Appendable ->
            "appendable"

        CompAppend ->
            "compappend"
