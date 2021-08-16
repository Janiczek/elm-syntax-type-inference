module Elm.TypeInference exposing (stub)

{-| TODO write docs


# Stub

@docs stub

-}

import Dict exposing (Dict)
import Elm.Qualifiedness exposing (Qualified)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File as File exposing (File)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Type exposing (Id, Type)
import Elm.TypeInference.DeclarationTypes as DeclarationTypes exposing (DeclarationTypes)
import Elm.TypeInference.Error exposing (TypeInferenceError)
import Elm.TypeInference.TypesForIds as TypesForIds exposing (TypesForIds)


{-| TODO stub
-}
stub : ()
stub =
    ()


inferExpr :
    Dict ( ModuleName, String ) (Type Qualified)
    -> ( TypesForIds, Id, DeclarationTypes )
    -> Expression
    ->
        Result
            ( TypeInferenceError, TypesForIds )
            ( Type Qualified, ( TypesForIds, Id, DeclarationTypes ) )
inferExpr aliases ( typesForIds, unusedId, env ) expr =
    Debug.todo "inferExpr"
