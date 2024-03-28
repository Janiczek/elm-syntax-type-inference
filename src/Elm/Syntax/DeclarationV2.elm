module Elm.Syntax.DeclarationV2 exposing
    ( DeclarationV2(..)
    , map
    , toTypeLookupTable
    )

import Elm.Syntax.ExpressionV2 as ExpressionV2 exposing (ExprWith, FunctionV2)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.NodeV2 as NodeV2 exposing (LocatedNode, NodeV2(..), TypedMeta)
import Elm.Syntax.PatternV2 as PatternV2 exposing (PatternWith)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import TypeLookupTable.Internal exposing (TypeLookupTable)


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


toTypeLookupTable : LocatedNode (DeclarationV2 TypedMeta) -> TypeLookupTable
toTypeLookupTable (NodeV2 _ _) =
    Debug.todo "DeclarationV2.toTypeLookupTable"
