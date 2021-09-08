module Elm.Syntax.File.Extra exposing
    ( containsDeclaration
    , exposes
    , moduleName
    , unalias
    )

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (Exposing(..))
import Elm.Syntax.Expression.Extra as Expression
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern.Extra as Pattern
import Elm.Syntax.VarName exposing (VarName)
import List.Extra as List


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
                        Expression.functionName fn == varName

                    AliasDeclaration typeAlias ->
                        Node.value typeAlias.name == varName

                    CustomTypeDeclaration customType ->
                        Node.value customType.name == varName

                    PortDeclaration signature ->
                        Node.value signature.name == varName

                    InfixDeclaration infix ->
                        Node.value infix.operator == varName

                    Destructuring pattern _ ->
                        List.member varName (Pattern.varNames (Node.value pattern))
            )


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
            Exposing.exposesFunction varName exposing_

        All _ ->
            {- Exposing.exposesFunction would always give us True
               which would be a lie. We need to check against the
               declarations inside the File in this case.
            -}
            containsDeclaration varName file


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
        |> List.find
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
