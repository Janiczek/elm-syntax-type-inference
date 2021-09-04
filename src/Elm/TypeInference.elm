module Elm.TypeInference exposing (infer)

{-| TODO write docs

@docs infer

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.ExpressionV2 as ExpressionV2
    exposing
        ( LocatedExpr
        , TypedExpr
        )
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FileV2 exposing (TypedFile)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.AssignIds as AssignIds
import Elm.TypeInference.Error exposing (Error(..))
import Elm.TypeInference.GenerateEquations as GenerateEquations
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.Type
    exposing
        ( Id
        , Type
        , TypeOrId
        , TypeOrId_(..)
        , Type_(..)
        )
import Elm.TypeInference.Unify as Unify
import List.ExtraExtra as List


infer : Dict ModuleName File -> Result (List Error) (Dict ModuleName TypedFile)
infer files =
    let
        typeAliases : Dict ( ModuleName, VarName ) Type
        typeAliases =
            gatherTypeAliases files
    in
    Debug.todo "infer"


gatherTypeAliases : Dict ModuleName File -> Dict ( ModuleName, VarName ) Type
gatherTypeAliases files =
    files
        |> Dict.toList
        |> List.fastConcatMap
            (\( moduleName, file ) ->
                file.declarations
                    |> List.filterMap
                        (\declarationNode ->
                            case Node.value declarationNode of
                                Declaration.AliasDeclaration typeAlias ->
                                    let
                                        type_ : Type
                                        type_ =
                                            typeAlias.typeAnnotation
                                                |> Node.value
                                                |> Type.fromTypeAnnotation
                                    in
                                    Just
                                        ( ( moduleName, Node.value typeAlias.name )
                                        , type_
                                        )

                                _ ->
                                    Nothing
                        )
            )
        |> Dict.fromList


inferExpr : Dict ( ModuleName, VarName ) Type -> LocatedExpr -> TIState TypedExpr
inferExpr typeAliases expr =
    -- TODO probably not do State.init here?
    let
        ( result, state ) =
            State.run (State.init typeAliases) (inferExpr_ expr)
    in
    State.fromTuple ( result, state )
        |> State.mapError (substituteTypesInError state.idTypes)


inferExpr_ : LocatedExpr -> TIState TypedExpr
inferExpr_ expr =
    State.do (AssignIds.assignIds expr) <| \exprWithIds ->
    State.do (GenerateEquations.generateLocalEquations exprWithIds) <| \exprEquations ->
    State.do GenerateEquations.generateVarEquations <| \varEquations ->
    State.do (Unify.unifyMany (exprEquations ++ varEquations)) <| \() ->
    State.do (substituteTypesInExpr exprWithIds) <| \betterExpr ->
    State.pure betterExpr


substituteTypesInExpr : TypedExpr -> TIState TypedExpr
substituteTypesInExpr expr =
    State.do State.getIdTypes <| \idTypes ->
    expr
        |> ExpressionV2.transformOnce (ExpressionV2.mapType (getBetterType idTypes))
        |> State.pure


substituteTypesInError : Dict Id TypeOrId -> Error -> Error
substituteTypesInError idTypes error =
    case error of
        TypeMismatch t1 t2 ->
            TypeMismatch
                (getBetterType idTypes t1)
                (getBetterType idTypes t2)

        OccursCheckFailed id type_ ->
            OccursCheckFailed id (getBetterType idTypes type_)

        ImpossibleAstPattern _ ->
            error


getBetterType : Dict Id TypeOrId -> TypeOrId -> TypeOrId
getBetterType idTypes typeOrId =
    if Dict.isEmpty idTypes then
        typeOrId

    else
        let
            f =
                getBetterType idTypes
        in
        case typeOrId of
            Id id ->
                Dict.get id idTypes
                    |> Maybe.map f
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
                                { from = f from
                                , to = f to
                                }

                    List param ->
                        Type <| List <| f param

                    Unit ->
                        typeOrId

                    Tuple e1 e2 ->
                        Type <| Tuple (f e1) (f e2)

                    Tuple3 e1 e2 e3 ->
                        Type <| Tuple3 (f e1) (f e2) (f e3)

                    Record bindings ->
                        Type <|
                            Record <|
                                Dict.map (always f) bindings

                    UserDefinedType ut ->
                        Type <|
                            UserDefinedType
                                { ut | args = List.map f ut.args }

                    WebGLShader { attributes, uniforms, varyings } ->
                        Type <|
                            WebGLShader
                                { attributes = Dict.map (always f) attributes
                                , uniforms = Dict.map (always f) uniforms
                                , varyings = Dict.map (always f) varyings
                                }
