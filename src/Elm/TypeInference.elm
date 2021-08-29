module Elm.TypeInference exposing (stub)

{-| TODO write docs


# Stub

@docs stub

-}

import Dict exposing (Dict)
import Elm.Syntax.ExpressionV2 as ExpressionV2
    exposing
        ( ExpressionV2
        , LocatedExpr
        , TypedExpr
        )
import Elm.Syntax.File as File exposing (File)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.AssignIds as AssignIds
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.GenerateEquations as GenerateEquations
import Elm.TypeInference.IdTypes as IdTypes exposing (IdTypes)
import Elm.TypeInference.Qualifiedness exposing (Qualified)
import Elm.TypeInference.State exposing (TIState)
import Elm.TypeInference.Type exposing (Id, Type, TypeOrId, TypeOrId_(..), Type_(..))
import Elm.TypeInference.Unify as Unify


{-| TODO stub
-}
stub : ()
stub =
    ()


inferExpr : LocatedExpr -> TIState TypedExpr
inferExpr aliases { idTypes, nextId, declarationTypes } expr =
    let
        ( exprWithIds, nextId1 ) =
            AssignIds.assignIds nextId expr

        ( nextId2, declarationTypes1, localTypeEquations ) =
            GenerateEquations.generateLocalEquations nextId1 declarationTypes exprWithIds

        typeEquationsLinkingDeclarationsToUsages =
            GenerateEquations.generateEquationsLinkingDeclarationsToUsages declarationTypes1

        allTypeEquations =
            localTypeEquations ++ typeEquationsLinkingDeclarationsToUsages

        newIdTypes : Result ( Error, IdTypes ) IdTypes
        newIdTypes =
            Unify.unifyAllEquations allTypeEquations aliases idTypes
    in
    newIdTypes
        |> Result.map
            (\map ->
                let
                    substituted =
                        substituteAllInExpr exprWithIds map
                in
                ( substituted, ( map, nextId2, declarationTypes1 ) )
            )
        |> Result.mapError substituteAllInError


substituteAllInExpr : IdTypes -> TypedExpr -> TypedExpr
substituteAllInExpr idTypes expr =
    ExpressionV2.transformOnce
        (ExpressionV2.mapType (getBetterType idTypes))
        expr


substituteAllInError : IdTypes -> Error -> Error
substituteAllInError idTypes error =
    case error of
        TypeMismatch t1 t2 ->
            TypeMismatch
                (getBetterType idTypes t1)
                (getBetterType idTypes t2)

        OccursCheckFailed id type_ ->
            OccursCheckFailed id (getBetterType idTypes type_)


getBetterType : IdTypes -> TypeOrId -> TypeOrId
getBetterType idTypes typeOrId =
    if IdTypes.isEmpty idTypes then
        typeOrId

    else
        case typeOrId of
            Id id ->
                IdTypes.get id idTypes
                    |> Maybe.map (getBetterType idTypes)
                    |> Maybe.withDefault typeOrId

            Type type_ ->
                case type_ of
                    Int ->
                        typeOrId

                    Float ->
                        typeOrId

                    Char ->
                        typeOrId

                    String ->
                        typeOrId

                    Bool ->
                        typeOrId

                    TypeVar _ ->
                        typeOrId

                    Function { from, to } ->
                        Type <|
                            Function
                                { from = getBetterType idTypes from
                                , to = getBetterType idTypes to
                                }

                    List param ->
                        Type <|
                            List <|
                                getBetterType idTypes param

                    Unit ->
                        typeOrId

                    Tuple e1 e2 ->
                        Type <|
                            Tuple
                                (getBetterType idTypes e1)
                                (getBetterType idTypes e2)

                    Tuple3 e1 e2 e3 ->
                        Type <|
                            Tuple3
                                (getBetterType idTypes e1)
                                (getBetterType idTypes e2)
                                (getBetterType idTypes e3)

                    UserDefinedType ut ->
                        Type <|
                            UserDefinedType
                                { ut | args = List.map (getBetterType idTypes) ut.args }

                    Record bindings ->
                        Type <|
                            Record <|
                                Dict.map (\_ binding -> getBetterType idTypes binding) bindings
