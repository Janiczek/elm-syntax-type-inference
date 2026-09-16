module Elm.TypeInference.BindingGroup exposing (Member, solveGroup)

{-| Solve a binding group (SCC of mutually-referencing bindings).
-}

import Elm.Syntax.FullModuleName as FullModuleName
import Elm.TypeInference.Error exposing (ErrorDetails(..))
import Elm.TypeInference.State as State exposing (StateM)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type.Internal as Type exposing (Id, Type(..))
import Elm.TypeInference.TypeEquation as TypeEquation exposing (TypeEquation)
import Elm.TypeInference.Unify as Unify exposing (UnifyConfig)
import Elm.TypeInference.VarSet as VarSet


{-| One binding in the binding group.
-}
type alias Member =
    { -- fresh type ID, registered against the declaration Range
      id : Id
    , annotation : Maybe Type
    , -- top-level decls get installed into `globalEnv`
      -- let..in bindings get installed into `lexicalEnv`
      install : Type -> StateM ()
    , -- monadic action to generate equations. Must be run after members'
      -- placeholders/annotations have been installed as they can reference each
      -- other.
      equations : StateM (List TypeEquation)
    }


solveGroup : UnifyConfig -> List Member -> StateM ()
solveGroup cfg members =
    State.do
        (State.withDeeperLetRank
            (State.do
                (State.traverse
                    (\member ->
                        State.do (State.setIdToCurrentLetRank member.id) <| \() ->
                        case member.annotation of
                            Just scheme ->
                                -- Trust the annotation
                                member.install scheme

                            Nothing ->
                                member.install (Type.mono (Type.id_ member.id))
                    )
                    members
                )
             <| \_ ->
             State.do (State.traverse .equations members) <| \eqLists ->
             State.do
                 (eqLists
                     |> List.concat
                     |> List.map TypeEquation.dropLabel
                     |> Unify.unifyMany cfg
                 )
             <| \() ->
             checkAnnotations cfg members
            )
        )
    <| \() ->
    State.do
        (State.traverse
            (\member ->
                case member.annotation of
                    Just _ ->
                        State.pure ()

                    Nothing ->
                        State.do (State.generalize (Type.id_ member.id)) <| \scheme ->
                        member.install scheme
            )
            members
        )
    <| \_ ->
    State.pure ()


{-| A declaration body must be at least as general as its annotation.

Motivating example:

    x : number
    x =
        1.0

This shouldn't typecheck: we know 1.0 must be a Float, so `number` is too
general.

-}
checkAnnotations : UnifyConfig -> List Member -> StateM ()
checkAnnotations cfg members =
    if cfg.canSkipChecks then
        State.pure ()

    else
        State.traverse (checkOne cfg) members
            |> State.map (always ())


checkOne : UnifyConfig -> Member -> StateM ()
checkOne cfg member =
    case member.annotation of
        Nothing ->
            State.pure ()

        Just (Forall boundVars annoMono) ->
            if List.isEmpty boundVars then
                State.pure ()

            else
                State.do State.getSubst <| \subst ->
                let
                    ( finalMono, _, _ ) =
                        SubstitutionMap.substituteMono subst (Type.id_ member.id)
                in
                if List.isEmpty (VarSet.toList (Type.monoTypeVars finalMono)) then
                    let
                        ( pubAnno, pubFinal ) =
                            Type.toPublicPair annoMono finalMono
                    in
                    State.error
                        { moduleName = FullModuleName.toModuleName cfg.moduleName
                        , declarationNames = cfg.declarationNames
                        , details = TypeMismatch pubAnno pubFinal
                        }

                else
                    State.pure ()
