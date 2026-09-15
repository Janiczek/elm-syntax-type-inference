module Elm.TypeInference.ImplicitImports exposing
    ( elmCorePackage
    , unaliasModule
    , implicitValueHomes
    , moduleExposingType
    , isImplicitQualifiedModule
    )

{-| Elm compiles every module with these implicit imports:

    import Basics exposing (..)
    import List exposing (List, (::))
    import Maybe exposing (Maybe(..))
    import Result exposing (Result(..))
    import String exposing (String)
    import Char exposing (Char)
    import Tuple

    import Debug

    import Platform exposing ( Program )
    import Platform.Cmd as Cmd exposing ( Cmd )
    import Platform.Sub as Sub exposing ( Sub )

The data below is hardcoded because `elm/core`'s prelude never changes.
We don't list all of `Basics`' functions; `docs.json` supplies that, so any
otherwise-unknown unqualified value can only come from `Basics`.

@docs elmCorePackage
@docs unaliasModule
@docs implicitValueHomes
@docs moduleExposingType
@docs isImplicitQualifiedModule

-}

import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.VarName exposing (VarName)


elmCorePackage : String
elmCorePackage =
    "elm/core"


{-| `Cmd` -> `Platform.Cmd`, `Sub` -> `Platform.Sub`

Only these two implicit imports use an alias.

-}
unaliasModule : String -> Maybe FullModuleName
unaliasModule singleSegmentAlias =
    case singleSegmentAlias of
        "Cmd" ->
            Just (FullModuleName.fromDotted "Platform.Cmd")

        "Sub" ->
            Just (FullModuleName.fromDotted "Platform.Sub")

        _ ->
            Nothing


{-| Which implicit modules could expose this unqualified value?

Only `Basics` (`exposing (..)`), `List` (`(::)`), `Maybe`
(`Just`, `Nothing`) and `Result` (`Ok`, `Err`) expose values; everything else
exposes `Only []` for values. `Basics` is always a candidate because we don't
enumerate its contents here -- `docs.json` settles whether it defines the name.

    identity --> [Basics]
    foobar --> [Basics]
    "::" --> [Basics, List]
    "Just" --> [Basics, Maybe]

-}
implicitValueHomes : VarName -> List FullModuleName
implicitValueHomes varName =
    case varName of
        "::" ->
            [ FullModuleName.fromDotted "Basics"
            , FullModuleName.fromDotted "List"
            ]

        "Just" ->
            [ FullModuleName.fromDotted "Basics"
            , FullModuleName.fromDotted "Maybe"
            ]

        "Nothing" ->
            [ FullModuleName.fromDotted "Basics"
            , FullModuleName.fromDotted "Maybe"
            ]

        "Ok" ->
            [ FullModuleName.fromDotted "Basics"
            , FullModuleName.fromDotted "Result"
            ]

        "Err" ->
            [ FullModuleName.fromDotted "Basics"
            , FullModuleName.fromDotted "Result"
            ]

        _ ->
            [ FullModuleName.fromDotted "Basics" ]


{-| Which implicit module exposes this type unqualified? (`Tuple` and `Debug`
expose none.)
-}
moduleExposingType : String -> Maybe FullModuleName
moduleExposingType typeName =
    case typeName of
        "Int" ->
            Just (FullModuleName.fromDotted "Basics")

        "Float" ->
            Just (FullModuleName.fromDotted "Basics")

        "Bool" ->
            Just (FullModuleName.fromDotted "Basics")

        "Never" ->
            Just (FullModuleName.fromDotted "Basics")

        "Order" ->
            Just (FullModuleName.fromDotted "Basics")

        "List" ->
            Just (FullModuleName.fromDotted "List")

        "Maybe" ->
            Just (FullModuleName.fromDotted "Maybe")

        "Result" ->
            Just (FullModuleName.fromDotted "Result")

        "String" ->
            Just (FullModuleName.fromDotted "String")

        "Char" ->
            Just (FullModuleName.fromDotted "Char")

        "Program" ->
            Just (FullModuleName.fromDotted "Platform")

        "Cmd" ->
            Just (FullModuleName.fromDotted "Platform.Cmd")

        "Sub" ->
            Just (FullModuleName.fromDotted "Platform.Sub")

        _ ->
            Nothing


{-| Is this qualifier implicitly available for qualified references?

`import Basics/List/Maybe/Result/String/Char/Tuple/Debug/Platform` are all
implicit by full name, so `List.map` or `Tuple.first` work without an explicit
import. `Platform.Cmd`/`Platform.Sub` are only implicit via their `Cmd`/`Sub`
aliases (see `unaliasModule`), so a full `Platform.Cmd` qualifier needs an
explicit import.
-}
isImplicitQualifiedModule : ModuleName -> Bool
isImplicitQualifiedModule qualifier =
    case qualifier of
        [ "Basics" ] ->
            True

        [ "List" ] ->
            True

        [ "Maybe" ] ->
            True

        [ "Result" ] ->
            True

        [ "String" ] ->
            True

        [ "Char" ] ->
            True

        [ "Tuple" ] ->
            True

        [ "Debug" ] ->
            True

        [ "Platform" ] ->
            True

        _ ->
            False
