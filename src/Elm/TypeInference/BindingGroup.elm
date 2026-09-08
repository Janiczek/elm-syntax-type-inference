module Elm.TypeInference.BindingGroup exposing (Member, solveGroup)

{-| Solve a binding group (SCC of mutually-referencing bindings).
-}

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName exposing (FullModuleName)
import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type as Type exposing (Id, MonoType, Type)
import Elm.TypeInference.TypeEquation as TypeEquation exposing (TypeEquation)
import Elm.TypeInference.Unify as Unify exposing (TypeAlias)
import Elm.TypeInference.VarName exposing (VarName)


{-| One binding in the binding group.
-}
type alias Member =
    { -- fresh type ID, registered against the declaration range
      id : Id
    , maybeAnnotation : Maybe Type
    , -- top-level decls get installed into `globalEnv`
      -- let..in bindings get installed into `typeEnv`
      install : Type -> TIState ()
    , -- monadic action to generate equations. Must be run after members'
      -- placeholders/annotations have been installed as they can reference each
      -- other.
      equations : TIState (List TypeEquation)
    }


solveGroup : Dict ( FullModuleName, VarName ) TypeAlias -> List Member -> TIState ()
solveGroup typeAliases members =
    State.do State.getTypeEnv <| \outerEnv ->
    State.do
        (State.traverse
            (\member ->
                case member.maybeAnnotation of
                    Just scheme ->
                        member.install scheme

                    Nothing ->
                        member.install (Type.mono (Type.id_ member.id))
            )
            members
        )
    <| \_ ->
    State.do (State.traverse .equations members) <| \eqLists ->
    let
        eqs : List TypeEquation
        eqs =
            List.concat eqLists
    in
    State.do State.getSubst <| \accumulatedSubst ->
    let
        preSubstitutedEqs : List ( MonoType, MonoType )
        preSubstitutedEqs =
            eqs
                |> List.map TypeEquation.dropLabel
                |> List.map
                    (Tuple.mapBoth
                        (SubstitutionMap.substituteMono accumulatedSubst)
                        (SubstitutionMap.substituteMono accumulatedSubst)
                    )
    in
    State.do (Unify.unifyMany typeAliases preSubstitutedEqs) <| \groupSubst ->
    State.do (State.composeSubst groupSubst) <| \() ->
    State.do
        (State.traverse
            (\member ->
                case member.maybeAnnotation of
                    Just _ ->
                        State.pure ()

                    Nothing ->
                        State.do (State.generalize outerEnv (Type.id_ member.id)) <| \scheme ->
                        member.install scheme
            )
            members
        )
    <| \_ ->
    State.pure ()
