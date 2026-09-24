module Elm.TypeInference.TypeVar exposing
    ( SuperType(..)
    , TypeVar
    , TypeVarStyle(..)
    , equal
    , parse
    , superTypeEqual
    , superTypeNotEqual
    , toString
    )

{-| -}

import List.Extra


{-|

    x : a (in source code) == NormalVar "a"
    x : a (given by compiler) == NormalId 1
    x : number (in source code) == SuperVar Number ""
    x : number1 (in source code) == SuperVar Number "1"
    x : number (given by compiler) == SuperId Number 1

Prefer `TypeVar.equal` over `==`

-}
type alias TypeVar =
    ( TypeVarStyle, SuperType )


type TypeVarStyle
    = Generated Int
    | Named String


{-| Prefer `TypeVar.superTypeEqual`/`superTypeNotEqual` over `==`/`/=`
-}
type SuperType
    = Normal
    | {- Int | Float -} Number
    | {- Int | Float | Char | String | List comparable | tuples of comparables -} Comparable
    | {- String | List a -} Appendable
    | {- String | List comparable -} CompAppend


toString : TypeVar -> String
toString ( style, super ) =
    case super of
        Normal ->
            case style of
                Generated theId ->
                    "#" ++ String.fromInt theId

                Named name ->
                    name

        _ ->
            let
                prefix : String
                prefix =
                    superTypeToString super
            in
            case style of
                Generated theId ->
                    prefix ++ "#" ++ String.fromInt theId

                Named name ->
                    prefix ++ name


parse : String -> TypeVar
parse name =
    let
        maybeConstrained : Maybe ( TypeVarStyle, SuperType )
        maybeConstrained =
            typeVariableConstraintPrefixes
                |> List.Extra.findMap
                    (\( prefix, super ) ->
                        if String.startsWith prefix name then
                            Just
                                ( Named (String.dropLeft (String.length prefix) name)
                                , super
                                )

                        else
                            Nothing
                    )
    in
    case maybeConstrained of
        Just constrained ->
            constrained

        Nothing ->
            ( Named name, Normal )


typeVariableConstraintPrefixes : List ( String, SuperType )
typeVariableConstraintPrefixes =
    [ ( "compappend", CompAppend )
    , ( "comparable", Comparable )
    , ( "appendable", Appendable )
    , ( "number", Number )
    ]


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


{-| Faster than `==`
-}
equal : TypeVar -> TypeVar -> Bool
equal ( style1, super1 ) ( style2, super2 ) =
    case style1 of
        Generated id1 ->
            case style2 of
                Generated id2 ->
                    -- id1 == id2
                    (id1 - id2 == 0)
                        && superTypeEqual super1 super2

                Named _ ->
                    False

        Named name1 ->
            case style2 of
                Named name2 ->
                    name1 == name2 && superTypeEqual super1 super2

                Generated _ ->
                    False


{-| Faster than `==`
-}
superTypeEqual : SuperType -> SuperType -> Bool
superTypeEqual a b =
    superTypeToTag a - superTypeToTag b == 0


superTypeToTag : SuperType -> Int
superTypeToTag super =
    case super of
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


{-| Faster than `/=`
-}
superTypeNotEqual : SuperType -> SuperType -> Bool
superTypeNotEqual a b =
    superTypeToTag a - superTypeToTag b /= 0
