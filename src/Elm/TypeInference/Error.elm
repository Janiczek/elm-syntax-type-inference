module Elm.TypeInference.Error exposing (Error(..), fromTypeAnnotationError, toString)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.Type
import Elm.TypeInference.Type as Type exposing (FromTypeAnnotationError(..), MonoType, SuperType, TypeVar)
import Elm.Writer


type Error
    = -- Syntax errors
      ImpossibleExpr (Node Expression)
    | ImpossiblePattern (Node Pattern)
    | ImpossibleType TypeAnnotation
    | ImpossibleDocsType Elm.Type.Type
    | MissingModuleName
      -- Var qualification errors
    | VarNotFound { usedIn : FullModuleName, varName : VarName }
    | AmbiguousName { usedIn : FullModuleName, varName : VarName, possibleModules : List FullModuleName }
    | AmbiguousModuleOwner { moduleName : String, possiblePackages : List String }
      -- Type errors
    | TypeMismatchMono MonoType MonoType
    | InfiniteType TypeVar MonoType
    | SuperTypeMismatch SuperType MonoType


fromTypeAnnotationError : FromTypeAnnotationError -> Error
fromTypeAnnotationError err =
    case err of
        ImpossibleAnnotation typeAnnotation ->
            ImpossibleType typeAnnotation

        AmbiguousModuleName ambiguity ->
            AmbiguousModuleOwner ambiguity


toString : Error -> String
toString error =
    case error of
        ImpossibleExpr exprNode ->
            String.join " "
                [ "ImpossibleExpr"
                , rangeToString (Node.range exprNode)
                , Elm.Writer.write (Elm.Writer.writeExpression exprNode)
                ]

        ImpossiblePattern patternNode ->
            String.join " "
                [ "ImpossiblePattern"
                , rangeToString (Node.range patternNode)
                , Elm.Writer.write (Elm.Writer.writePattern patternNode)
                ]

        ImpossibleType typeAnnotation ->
            "ImpossibleType "
                ++ Elm.Writer.write
                    (Elm.Writer.writeTypeAnnotation
                        (Node.Node Range.emptyRange typeAnnotation)
                    )

        ImpossibleDocsType type_ ->
            "ImpossibleDocsType " ++ docsTypeToString type_

        MissingModuleName ->
            "MissingModuleName"

        VarNotFound r ->
            "VarNotFound "
                ++ record
                    [ ( "usedIn", FullModuleName.toString r.usedIn )
                    , ( "varName", r.varName )
                    ]

        AmbiguousName r ->
            "AmbiguousName "
                ++ record
                    [ ( "usedIn", FullModuleName.toString r.usedIn )
                    , ( "varName", r.varName )
                    , ( "possibleModules", list (List.map FullModuleName.toString r.possibleModules) )
                    ]

        AmbiguousModuleOwner r ->
            "AmbiguousModuleOwner "
                ++ record
                    [ ( "moduleName", r.moduleName )
                    , ( "possiblePackages", list r.possiblePackages )
                    ]

        TypeMismatchMono t1 t2 ->
            String.join " "
                [ "TypeMismatchMono"
                , parenIfHasSpace (Type.monoTypeToString t1)
                , parenIfHasSpace (Type.monoTypeToString t2)
                ]

        InfiniteType typeVar type_ ->
            String.join " "
                [ "InfiniteType"
                , parenIfHasSpace (Type.varToString typeVar)
                , parenIfHasSpace (Type.monoTypeToString type_)
                ]

        SuperTypeMismatch super type_ ->
            String.join " "
                [ "SuperTypeMismatch"
                , parenIfHasSpace (Type.superTypeToString super)
                , parenIfHasSpace (Type.monoTypeToString type_)
                ]



-- HELPERS


{-| Adds (...) if the string has spaces.
Handy for types: eg. `TypeMismatchMono Int (List String)`
-}
parenIfHasSpace : String -> String
parenIfHasSpace str =
    if String.contains " " str then
        "(" ++ str ++ ")"

    else
        str


record : List ( String, String ) -> String
record fields =
    fields
        |> List.map (\( key, value ) -> key ++ " = " ++ value)
        |> String.join ", "
        |> (\str -> "{ " ++ str ++ " }")


list : List String -> String
list items =
    "[" ++ String.join ", " items ++ "]"


rangeToString : Range -> String
rangeToString { start } =
    String.fromInt start.row ++ ":" ++ String.fromInt start.column


docsTypeToString : Elm.Type.Type -> String
docsTypeToString type_ =
    let
        -- Wraps in parens if it wouldn't parse back unambiguously in arg position
        wrapped : Elm.Type.Type -> String
        wrapped t =
            case t of
                Elm.Type.Lambda _ _ ->
                    "(" ++ docsTypeToString t ++ ")"

                Elm.Type.Type _ (_ :: _) ->
                    "(" ++ docsTypeToString t ++ ")"

                _ ->
                    docsTypeToString t
    in
    case type_ of
        Elm.Type.Var name ->
            name

        Elm.Type.Lambda from to ->
            -- `->` is right-associative, so only the left side is ambiguous
            wrapped from ++ " -> " ++ docsTypeToString to

        Elm.Type.Tuple [] ->
            "()"

        Elm.Type.Tuple types ->
            "( " ++ String.join ", " (List.map docsTypeToString types) ++ " )"

        Elm.Type.Type name args ->
            (name :: List.map wrapped args)
                |> String.join " "

        Elm.Type.Record fields extensibleVar ->
            let
                prefix : String
                prefix =
                    case extensibleVar of
                        Nothing ->
                            ""

                        Just var ->
                            var ++ " | "

                fieldsStr : String
                fieldsStr =
                    fields
                        |> List.map
                            (\( fieldName, fieldType ) ->
                                fieldName ++ " : " ++ docsTypeToString fieldType
                            )
                        |> String.join ", "
            in
            if String.isEmpty prefix && List.isEmpty fields then
                "{}"

            else
                "{ " ++ prefix ++ fieldsStr ++ " }"
