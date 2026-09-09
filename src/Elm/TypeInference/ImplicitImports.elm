module Elm.TypeInference.ImplicitImports exposing
    ( package
    , unaliasModule
    , modulesPossiblyExposingValue
    , moduleExposingType
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

@docs package
@docs unaliasModule
@docs modulesPossiblyExposingValue
@docs moduleExposingType

-}

import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.VarName exposing (VarName)


type Exposed
    = All
    | Only (List String)


type alias ImplicitImport =
    { moduleName : String
    , alias_ : Maybe String
    , values : Exposed
    , types : Exposed
    }


package : String
package =
    "elm/core"


list : List ImplicitImport
list =
    [ { moduleName = "Basics"
      , alias_ = Nothing
      , values = All
      , types = Only [ "Int", "Float", "Bool", "Never", "Order" ]
      }
    , { moduleName = "List"
      , alias_ = Nothing
      , values = Only [ "::" ]
      , types = Only [ "List" ]
      }
    , { moduleName = "Maybe"
      , alias_ = Nothing
      , values = Only [ "Just", "Nothing" ]
      , types = Only [ "Maybe" ]
      }
    , { moduleName = "Result"
      , alias_ = Nothing
      , values = Only [ "Ok", "Err" ]
      , types = Only [ "Result" ]
      }
    , { moduleName = "String"
      , alias_ = Nothing
      , values = Only []
      , types = Only [ "String" ]
      }
    , { moduleName = "Char"
      , alias_ = Nothing
      , values = Only []
      , types = Only [ "Char" ]
      }
    , { moduleName = "Tuple"
      , alias_ = Nothing
      , values = Only []
      , types = Only []
      }
    , { moduleName = "Debug"
      , alias_ = Nothing
      , values = Only []
      , types = Only []
      }
    , { moduleName = "Platform"
      , alias_ = Nothing
      , values = Only []
      , types = Only [ "Program" ]
      }
    , { moduleName = "Platform.Cmd"
      , alias_ = Just "Cmd"
      , values = Only []
      , types = Only [ "Cmd" ]
      }
    , { moduleName = "Platform.Sub"
      , alias_ = Just "Sub"
      , values = Only []
      , types = Only [ "Sub" ]
      }
    ]


{-| `Cmd` -> `Platform.Cmd`, `Sub` -> `Platform.Sub`

This only comes into play if we know there's no `import My.Cmd as Cmd`

-}
unaliasModule : String -> Maybe FullModuleName
unaliasModule singleSegmentAlias =
    list
        |> List.filter (\import_ -> import_.alias_ == Just singleSegmentAlias)
        |> List.head
        |> Maybe.map (.moduleName >> FullModuleName.fromDotted)


modulesPossiblyExposingValue : VarName -> List String
modulesPossiblyExposingValue varName =
    list
        |> List.filter (\import_ -> exposes varName import_.values)
        |> List.map .moduleName


moduleExposingType : String -> Maybe FullModuleName
moduleExposingType typeName =
    list
        |> List.filter (\import_ -> exposes typeName import_.types)
        |> List.head
        |> Maybe.map (.moduleName >> FullModuleName.fromDotted)


exposes : String -> Exposed -> Bool
exposes name exposed =
    case exposed of
        All ->
            True

        Only names ->
            List.member name names
