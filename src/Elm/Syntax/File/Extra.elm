module Elm.Syntax.File.Extra exposing
    ( containsDeclaration
    , containsTypeDeclaration
    , exposes
    , exposesInExposing
    , exposesType
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
import Elm.Syntax.VarName exposing (VarName)
import List.Extra


moduleName : File -> FullModuleName
moduleName file =
    file.moduleDefinition
        |> Node.value
        |> Module.moduleName
        |> FullModuleName.fromModuleName_


containsDeclaration : VarName -> File -> Bool
containsDeclaration varName file =
    file.declarations
        |> List.any
            (\declNode ->
                case Node.value declNode of
                    FunctionDeclaration fn ->
                        Elm.Syntax.Expression.Extra.functionName fn == varName

                    AliasDeclaration typeAlias ->
                        Node.value typeAlias.name == varName

                    CustomTypeDeclaration customType ->
                        Node.value customType.name == varName

                    PortDeclaration signature ->
                        Node.value signature.name == varName

                    InfixDeclaration infix ->
                        Node.value infix.operator == varName

                    Destructuring pattern _ ->
                        List.member varName (Elm.Syntax.Pattern.Extra.varNames (Node.value pattern))
            )


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

                    _ ->
                        False
            )


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


exposes : VarName -> File -> Bool
exposes varName file =
    let
        exposing_ : Exposing
        exposing_ =
            file.moduleDefinition
                |> Node.value
                |> Module.exposingList
    in
    case exposing_ of
        Explicit _ ->
            exposesInExposing varName exposing_

        All _ ->
            {- exposesInExposing would always give us True
               which would be a lie. We need to check against the
               declarations inside the File in this case.
            -}
            containsDeclaration varName file


{-| Like `Exposing.exposesFunction`, but also recognizes operators.
-}
exposesInExposing : VarName -> Exposing -> Bool
exposesInExposing varName exposing_ =
    case exposing_ of
        All _ ->
            True

        Explicit exposedNodes ->
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
