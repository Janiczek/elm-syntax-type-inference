module Elm.Syntax.DeclarationV2 exposing (DeclarationV2(..))

import Elm.Syntax.ExpressionV2 exposing (ExprWith, ExpressionV2, FunctionV2)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.PatternV2 exposing (PatternWith)
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
