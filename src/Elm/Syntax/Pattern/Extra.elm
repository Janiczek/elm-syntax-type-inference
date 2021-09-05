module Elm.Syntax.Pattern.Extra exposing (varNames)

import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.VarName exposing (VarName)
import List.ExtraExtra as List


varNames : Pattern -> List VarName
varNames pattern =
    case pattern of
        AllPattern ->
            []

        UnitPattern ->
            []

        CharPattern _ ->
            []

        StringPattern _ ->
            []

        IntPattern _ ->
            []

        HexPattern _ ->
            []

        FloatPattern _ ->
            []

        TuplePattern patterns ->
            List.fastConcatMap (Node.value >> varNames) patterns

        RecordPattern fields ->
            List.map Node.value fields

        UnConsPattern p1 p2 ->
            varNames (Node.value p1) ++ varNames (Node.value p2)

        ListPattern patterns ->
            List.fastConcatMap (Node.value >> varNames) patterns

        VarPattern var ->
            [ var ]

        NamedPattern _ patterns ->
            List.fastConcatMap (Node.value >> varNames) patterns

        AsPattern p1 name ->
            Node.value name :: varNames (Node.value p1)

        ParenthesizedPattern p1 ->
            varNames (Node.value p1)
