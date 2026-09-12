module Elm.TypeInference.Error exposing (Error(..), fromTypeAnnotationError, toString, withDeclarations)

{-| Errors reported while resolving or inferring a module.

@docs Error, fromTypeAnnotationError, toString, withDeclarations

-}

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.VarName exposing (VarName)
import Elm.Type
import Elm.TypeInference.Type as Type exposing (FromTypeAnnotationError(..), MonoType)
import Elm.TypeInference.TypeVar as TypeVar exposing (SuperType, TypeVar)
import Elm.Writer


{-| Failures encountered while resolving or inferring a module.
-}
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
    | InternalInconsistency MonoType MonoType
      {- Location for the three type errors above, which carry none of their
         own -- without it a failure is essentially undebuggable.
      -}
    | InDeclarations { moduleName : FullModuleName, declarationNames : List VarName } Error


{-| Attach the binding group an error came from -- but only to the errors that
don't already say where they happened.

TODO: put the location on the error variants themselves, remove InDeclarations

-}
withDeclarations : { moduleName : FullModuleName, declarationNames : List VarName } -> Error -> Error
withDeclarations where_ error =
    case error of
        TypeMismatchMono _ _ ->
            InDeclarations where_ error

        InfiniteType _ _ ->
            InDeclarations where_ error

        SuperTypeMismatch _ _ ->
            InDeclarations where_ error

        _ ->
            error


{-| Convert a type-annotation conversion failure into an inference error.
-}
fromTypeAnnotationError : FromTypeAnnotationError -> Error
fromTypeAnnotationError err =
    case err of
        ImpossibleAnnotation typeAnnotation ->
            ImpossibleType typeAnnotation

        AmbiguousModuleName ambiguity ->
            AmbiguousModuleOwner ambiguity


{-| Render an error for diagnostic output.
-}
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
                , parenIfHasSpace (TypeVar.toString typeVar)
                , parenIfHasSpace (Type.monoTypeToString type_)
                ]

        InDeclarations r inner ->
            toString inner
                ++ " (in "
                ++ FullModuleName.toString r.moduleName
                ++ "."
                ++ String.join "/" r.declarationNames
                ++ ")"

        SuperTypeMismatch super type_ ->
            String.join " "
                [ "SuperTypeMismatch"
                , parenIfHasSpace (TypeVar.superTypeToString super)
                , parenIfHasSpace (Type.monoTypeToString type_)
                ]

        InternalInconsistency t1 t2 ->
            String.join " "
                [ "InternalInconsistency"
                , parenIfHasSpace (Type.monoTypeToString t1)
                , parenIfHasSpace (Type.monoTypeToString t2)
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
            in
            if String.isEmpty prefix && List.isEmpty fields then
                "{}"

            else
                let
                    fieldsStr : String
                    fieldsStr =
                        fields
                            |> List.map
                                (\( fieldName, fieldType ) ->
                                    fieldName ++ " : " ++ docsTypeToString fieldType
                                )
                            |> String.join ", "
                in
                "{ " ++ prefix ++ fieldsStr ++ " }"
