module Elm.Syntax.DeclarationV2 exposing (DeclarationV2(..), map)

import Elm.Syntax.ExpressionV2 as ExpressionV2 exposing (ExprWith, FunctionV2)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.PatternV2 as PatternV2 exposing (PatternWith)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


type DeclarationV2 meta
    = FunctionDeclaration (FunctionV2 meta)
    | AliasDeclaration TypeAlias
    | CustomTypeDeclaration Type
    | PortDeclaration Signature
    | InfixDeclaration Infix
    | Destructuring (PatternWith meta) (ExprWith meta)


map : (meta1 -> meta2) -> DeclarationV2 meta1 -> DeclarationV2 meta2
map fn declaration =
    case declaration of
        AliasDeclaration typeAlias ->
            AliasDeclaration typeAlias

        CustomTypeDeclaration type_ ->
            CustomTypeDeclaration type_

        PortDeclaration signature ->
            PortDeclaration signature

        InfixDeclaration infix ->
            InfixDeclaration infix

        FunctionDeclaration function ->
            FunctionDeclaration (ExpressionV2.mapFunction fn function)

        Destructuring pattern expr ->
            Destructuring
                (PatternV2.map fn pattern)
                (ExpressionV2.map fn expr)
