module Elm.Syntax.File.Extra exposing
    ( containsTypeDeclaration
    , containsValueDeclaration
    , exposesInExposing
    , exposesType
    , exposesValue
    , moduleName
    , resolveOperatorFunction
    , unalias
    )

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (Exposing(..))
import Elm.Syntax.Expression.Extra
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern.Extra
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.Syntax.VarName exposing (VarName)
import List.Extra


{-|

     `module Foo.Bar exposing (..)`
     --> ( "Foo", [ "Bar" ] )

-}
moduleName : File -> FullModuleName
moduleName file =
    file.moduleDefinition
        |> Node.value
        |> Module.moduleName
        |> FullModuleName.fromModuleName_


{-| Does the module declare this type? (Doesn't count constructors, only type names.)

Given:

    module Foo exposing (..)

    type Bar
        = Bar1
        | Bar2 Int

    type alias Baz =
        { x : Int }

    quux : Int
    quux =
        1

then:

    containsTypeDeclaration "Bar" file
    --> True

    containsTypeDeclaration "Baz" file
    --> True

    containsTypeDeclaration "Bar1" file
    --> False

    containsTypeDeclaration "quux" file
    --> False

-}
containsTypeDeclaration : VarName -> File -> Bool
containsTypeDeclaration typeName file =
    file.declarations
        |> List.any
            (\declNode ->
                case Node.value declNode of
                    AliasDeclaration typeAlias ->
                        Node.value typeAlias.name == typeName

                    CustomTypeDeclaration customType ->
                        Node.value customType.name == typeName

                    FunctionDeclaration _ ->
                        False

                    PortDeclaration _ ->
                        False

                    InfixDeclaration _ ->
                        False

                    Destructuring _ _ ->
                        False
            )


{-| Does the module declare this value?

Given:

    module Foo exposing (..)

    type Bar
        = Bar1
        | Bar2 Int

    type alias Point =
        { x : Int }

    type alias Id =
        String

    quux : Int
    quux =
        1

then:

    containsValueDeclaration "quux" file
    --> True

    containsValueDeclaration "Bar1" file
    --> True

    containsValueDeclaration "Point" file
    --> True

    containsValueDeclaration "Bar" file
    --> False

    containsValueDeclaration "Id" file
    --> False

-}
containsValueDeclaration : VarName -> File -> Bool
containsValueDeclaration varName file =
    file.declarations
        |> List.any
            (\declNode ->
                case Node.value declNode of
                    FunctionDeclaration fn ->
                        Elm.Syntax.Expression.Extra.functionName fn == varName

                    AliasDeclaration typeAlias ->
                        -- Record aliases define an implicit constructor function
                        (Node.value typeAlias.name == varName)
                            && isRecordAnnotation typeAlias.typeAnnotation

                    CustomTypeDeclaration customType ->
                        List.any (\ctor -> Node.value (Node.value ctor).name == varName) customType.constructors

                    PortDeclaration signature ->
                        Node.value signature.name == varName

                    InfixDeclaration infix ->
                        Node.value infix.operator == varName

                    Destructuring pattern _ ->
                        List.member varName (Elm.Syntax.Pattern.Extra.varNames (Node.value pattern))
            )


{-| Does the module expose this type or type alias?

Given:

    module Foo exposing (Bar)

    type Bar
        = Bar1

    type Baz
        = Baz1

then:

    exposesType "Bar" file
    --> True

    exposesType "Baz" file
    --> False

    exposesType "Bar1" file
    --> False -- Constructors don't count

-}
exposesType : VarName -> File -> Bool
exposesType typeName file =
    let
        exposing_ : Exposing
        exposing_ =
            file.moduleDefinition
                |> Node.value
                |> Module.exposingList
    in
    case exposing_ of
        All _ ->
            containsTypeDeclaration typeName file

        Explicit exposedNodes ->
            exposedNodes
                |> List.any
                    (\exposedNode ->
                        case Node.value exposedNode of
                            Exposing.TypeOrAliasExpose name ->
                                name == typeName

                            Exposing.TypeExpose exposedType ->
                                exposedType.name == typeName

                            _ ->
                                False
                    )


{-| Which function does an operator in this file alias?

Given `infix right 5 (++) = append`:

    resolveOperatorFunction "++" file
    --> Just "append"

    resolveOperatorFunction "--" file
    --> Nothing

-}
resolveOperatorFunction : VarName -> File -> Maybe VarName
resolveOperatorFunction operator file =
    file.declarations
        |> List.filterMap
            (\declNode ->
                case Node.value declNode of
                    InfixDeclaration infix ->
                        if Node.value infix.operator == operator then
                            Just (Node.value infix.function)

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head


{-| Does the module expose this _value_?

Given:

    module Foo exposing (Bar, baz)

    type Bar
        = Bar1

    baz : Int
    baz =
        1

then:

    exposesValue "baz" file
    --> True

    exposesValue "Bar" file
    --> False

    exposesValue "nope" file
    --> False

Given:

    module Foo exposing (Bar(..), Point)

    type Bar
        = Bar1
        | Bar2 Int

    type alias Point =
        { x : Int }

then:

    exposesValue "Bar1" file
    --> True

    exposesValue "Point" file
    --> True

    exposesValue "Bar" file
    --> False

-}
exposesValue : VarName -> File -> Bool
exposesValue varName file =
    let
        exposing_ : Exposing
        exposing_ =
            file.moduleDefinition
                |> Node.value
                |> Module.exposingList
    in
    case exposing_ of
        Explicit _ ->
            let
                -- Check declarations because of Bar(..) not mentioning the names of constructors
                exposedAlongsideType : () -> Bool
                exposedAlongsideType () =
                    List.any
                        (\typeName -> List.member varName (unionConstructorNames typeName file))
                        (openedUnionTypeNames exposing_)
                        || List.any
                            (\typeName -> typeName == varName && definesRecordAlias typeName file)
                            (opaqueTypeOrAliasNames exposing_)
            in
            case exposesInExposing varName exposing_ of
                Just answer ->
                    answer

                Nothing ->
                    exposedAlongsideType ()

        All _ ->
            containsValueDeclaration varName file


{-| Given:

    type Bar
        = Bar1
        | Bar2 Int

    type alias Baz =
        { x : Int }

then:

    unionConstructorNames "Bar" file
    --> [ "Bar1", "Bar2" ]

    unionConstructorNames "Baz" file
    --> []

    unionConstructorNames "nope" file
    --> []

-}
unionConstructorNames : String -> File -> List VarName
unionConstructorNames typeName file =
    file.declarations
        |> List.filterMap
            (\declNode ->
                case Node.value declNode of
                    CustomTypeDeclaration customType ->
                        if Node.value customType.name == typeName then
                            Just (List.map (\ctor -> Node.value (Node.value ctor).name) customType.constructors)

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.withDefault []


{-| Is this record alias defined here? (Relevant for tracking their constructor functions.)

Given:

    type alias Point =
        { x : Int, y : Int }

    type alias Id =
        String

    type Bar
        = Bar1

then:

    definesRecordAlias "Point" file
    --> True

    definesRecordAlias "Id" file
    --> False

    definesRecordAlias "Bar" file
    --> False

-}
definesRecordAlias : String -> File -> Bool
definesRecordAlias typeName file =
    file.declarations
        |> List.any
            (\declNode ->
                case Node.value declNode of
                    AliasDeclaration typeAlias ->
                        (Node.value typeAlias.name == typeName)
                            && isRecordAnnotation typeAlias.typeAnnotation

                    _ ->
                        False
            )


isRecordAnnotation : Node.Node TypeAnnotation.TypeAnnotation -> Bool
isRecordAnnotation annotation =
    case Node.value annotation of
        TypeAnnotation.Record _ ->
            True

        _ ->
            False


{-| Like `Exposing.exposesFunction`, but also recognizes operators.

`Nothing` means the exposing list doesn't know and only the module's
declarations can settle it (see `exposesValue`).

Given `exposing (foo, Bar(..), Point, (|=))`:

    exposesInExposing "foo" exposing_
    --> Just True

    exposesInExposing "|=" exposing_
    --> Just True

    exposesInExposing "baz" exposing_
    --> Just False

A `SomeUnionType(..)` causes every uppercase name return a Nothing: they could
all be constructors hidden in there.

    exposesInExposing "Bar1" exposing_
    --> Nothing

    exposesInExposing "Bar" exposing_
    --> Nothing

    exposesInExposing "Point" exposing_
    --> Nothing

    exposesInExposing "Quux" exposing_
    --> Just False

Given `exposing (foo, Bar)` with no `(..)` in the list, `Bar1` is unambiguously
not present but `Bar` could still be a record type constructor function:

    exposesInExposing "Bar1" exposing_
    --> Just False

    exposesInExposing "Bar" exposing_
    --> Nothing

Given `exposing (..)` everything answers Nothing:

    exposesInExposing "whatever" exposing_
    --> Nothing

-}
exposesInExposing : VarName -> Exposing -> Maybe Bool
exposesInExposing varName exposing_ =
    case exposing_ of
        All _ ->
            Nothing

        Explicit exposedNodes ->
            let
                namedVerbatim : Bool
                namedVerbatim =
                    exposedNodes
                        |> List.any
                            (\exposedNode ->
                                case Node.value exposedNode of
                                    Exposing.FunctionExpose fun ->
                                        fun == varName

                                    Exposing.InfixExpose op ->
                                        op == varName

                                    _ ->
                                        False
                            )

                {- Could a type in the list have brought this value in without
                   naming it? Only the declarations can tell.
                -}
                couldRideAlongWithType : Bool
                couldRideAlongWithType =
                    couldBeConstructorName varName
                        && (not (List.isEmpty (openedUnionTypeNames exposing_))
                                || List.member varName (opaqueTypeOrAliasNames exposing_)
                           )
            in
            if namedVerbatim then
                Just True

            else if couldRideAlongWithType then
                Nothing

            else
                Just False


couldBeConstructorName : VarName -> Bool
couldBeConstructorName varName =
    case String.uncons varName of
        Just ( firstChar, _ ) ->
            Char.isUpper firstChar

        Nothing ->
            False


{-| Given `exposing (Foo(..), Bar, baz)`:

    openedUnionTypeNames exposing_
    --> [ "Foo" ]

Given `exposing (..)`:

    openedUnionTypeNames exposing_
    --> []

-}
openedUnionTypeNames : Exposing -> List String
openedUnionTypeNames exposing_ =
    case exposing_ of
        All _ ->
            []

        Explicit exposedNodes ->
            exposedNodes
                |> List.filterMap
                    (\exposedNode ->
                        case Node.value exposedNode of
                            Exposing.TypeExpose exposedType ->
                                if exposedType.open /= Nothing then
                                    Just exposedType.name

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )


{-| Given `exposing (Foo(..), Bar, baz)`:

    opaqueTypeOrAliasNames exposing_
    --> [ "Bar" ]

Given `exposing (..)` we have no names to report:

    opaqueTypeOrAliasNames exposing_
    --> []

-}
opaqueTypeOrAliasNames : Exposing -> List String
opaqueTypeOrAliasNames exposing_ =
    case exposing_ of
        All _ ->
            []

        Explicit exposedNodes ->
            exposedNodes
                |> List.filterMap
                    (\exposedNode ->
                        case Node.value exposedNode of
                            Exposing.TypeOrAliasExpose name ->
                                Just name

                            _ ->
                                Nothing
                    )


{-| Reverses the aliasing in import statements for a single module name.

Given `import Foo as F`:

    unalias file "F"
    --> Just "Foo"

    unalias file "Foo"
    --> Nothing

    unalias file "Foox"
    --> Nothing

-}
unalias : File -> String -> Maybe FullModuleName
unalias file wantedAlias =
    file.imports
        |> List.Extra.find
            (\importNode ->
                let
                    maybeAlias : Maybe ModuleName
                    maybeAlias =
                        importNode
                            |> Node.value
                            |> .moduleAlias
                            |> Maybe.map Node.value
                in
                maybeAlias == Just [ wantedAlias ]
            )
        |> Maybe.andThen
            (\importNode ->
                importNode
                    |> Node.value
                    |> .moduleName
                    |> Node.value
                    |> FullModuleName.fromModuleName
            )
