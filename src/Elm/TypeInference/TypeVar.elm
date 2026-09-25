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
    superTypeToString super
        ++ (case style of
                Generated theId ->
                    "#" ++ String.fromInt theId

                Named name ->
                    name
           )
        ++ ""


parse : String -> TypeVar
parse name =
    -- using String.slice instead of dropLeft to avoid a bounds check.
    -- using String.slice 0 2 case of instead of one if else if chain
    --     because String.startsWith uses find() == 0 internally
    --     and because most type variables are short and do not have a constraint
    case String.slice 0 1 name of
        "c" ->
            if String.startsWith "compappend" name then
                ( Named (String.slice 10 (String.length name) name), CompAppend )

            else if String.startsWith "comparable" name then
                ( Named (String.slice 10 (String.length name) name), Comparable )

            else
                ( Named name, Normal )

        "n" ->
            if String.startsWith "number" name then
                ( Named (String.slice 6 (String.length name) name), Number )

            else
                ( Named name, Normal )

        "a" ->
            if String.startsWith "appendable" name then
                ( Named (String.slice 10 (String.length name) name), Appendable )

            else
                ( Named name, Normal )

        _ ->
            ( Named name, Normal )


superTypeToString : SuperType -> String
superTypeToString super =
    case super of
        Normal ->
            ""

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
