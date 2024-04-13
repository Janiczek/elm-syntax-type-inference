module Elm.Syntax.DeclarationV2 exposing
    ( DeclarationV2(..)
    , map
    , toTypeLookupTable
    )

import Elm.Syntax.ExpressionV2 as ExpressionV2 exposing (ExprWith, FunctionV2)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.NodeV2 as NodeV2 exposing (LocatedNode, NodeV2(..), TypedMeta)
import Elm.Syntax.PatternV2 as PatternV2 exposing (PatternWith)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import TypeLookupTable exposing (TypeLookupTable)


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


toTypeLookupTable : ModuleName -> LocatedNode (DeclarationV2 TypedMeta) -> TypeLookupTable
toTypeLookupTable moduleName (NodeV2 _ decl) =
    case decl of
        FunctionDeclaration { declaration } ->
            let
                (NodeV2 declMeta impl) =
                    declaration

                (NodeV2 exprMeta expr) =
                    impl.expression

                (NodeV2 nameMeta name) =
                    impl.name

                exprType =
                    exprMeta.type_

                exprRange =
                    exprMeta.range

                nameRange =
                    nameMeta.range

                declRange =
                    declMeta.range

                -- TODO arguments
            in
            TypeLookupTable.fromList moduleName
                [ ( nameRange, exprType, "fn decl name: " ++ name )
                , ( exprRange, exprType, "fn decl expr" )
                , ( declRange, exprType, "fn decl" )
                ]

        AliasDeclaration _ ->
            Debug.todo "DeclarationV2.toTypeLookupTable: AliasDeclaration"

        CustomTypeDeclaration _ ->
            Debug.todo "DeclarationV2.toTypeLookupTable: CustomTypeDeclaration"

        PortDeclaration _ ->
            Debug.todo "DeclarationV2.toTypeLookupTable: PortDeclaration"

        InfixDeclaration _ ->
            Debug.todo "DeclarationV2.toTypeLookupTable: InfixDeclaration"

        Destructuring _ _ ->
            Debug.todo "DeclarationV2.toTypeLookupTable: Destructuring"
