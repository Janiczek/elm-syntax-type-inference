module Elm.TypeInference.BindingGroup exposing (Member, solveGroup)

{-| Solve a binding group (SCC of mutually-referencing bindings).
-}

import Elm.TypeInference.State as State exposing (StateM)
import Elm.TypeInference.Type.Internal as Type exposing (Id, MonoType, Type)
import Elm.TypeInference.TypeEquation as TypeEquation exposing (TypeEquation)
import Elm.TypeInference.Unify as Unify exposing (UnifyConfig)


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
             eqLists
                 |> List.concat
                 |> List.map TypeEquation.dropLabel
                 |> Unify.unifyMany cfg
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
