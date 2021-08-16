module Elm.TypeInference.DeclarationTypes exposing
    ( DeclarationTypes
    , add
    , empty
    , singleton
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Qualifiedness exposing (Qualified)
import Elm.Syntax.VarName exposing (VarName)
import Elm.Type exposing (TypeOrId)


{-| A thin opaque wrapper around a dict from type variable IDs to inferred types.
Note ID can point to another ID (eg. dict entry `(1,Id 2)`) so you might need to
walk this dict multiple times.
-}
type DeclarationTypes
    = DeclarationTypes (Dict ( ModuleName, VarName ) (List (TypeOrId Qualified)))


empty : DeclarationTypes
empty =
    DeclarationTypes Dict.empty


add : ( ModuleName, VarName ) -> TypeOrId Qualified -> DeclarationTypes -> DeclarationTypes
add qualifiedName type_ (DeclarationTypes env) =
    env
        |> Dict.update qualifiedName
            (\maybeTypes ->
                case maybeTypes of
                    Nothing ->
                        Just [ type_ ]

                    Just types ->
                        Just (type_ :: types)
            )
        |> DeclarationTypes


singleton : ( ModuleName, VarName ) -> TypeOrId Qualified -> DeclarationTypes
singleton qualifiedName type_ =
    empty |> add qualifiedName type_
