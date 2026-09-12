module Elm.TypeInference.BindingGroup exposing (Member, solveGroup)

{-| Solve a binding group (SCC of mutually-referencing bindings).
-}

import Elm.TypeInference.State as State exposing (TIState)
import Elm.TypeInference.Type as Type exposing (Id, MonoType, Type)
import Elm.TypeInference.TypeEquation as TypeEquation exposing (TypeEquation)
import Elm.TypeInference.Unify as Unify exposing (UnifyCfg)


{-| One binding in the binding group.
-}
type alias Member =
    { -- fresh type ID, registered against the declaration range
      id : Id
    , maybeAnnotation : Maybe Type
    , -- top-level decls get installed into `globalEnv`
      -- let..in bindings get installed into `lexicalEnv`
      install : Type -> TIState ()
    , -- monadic action to generate equations. Must be run after members'
      -- placeholders/annotations have been installed as they can reference each
      -- other.
      equations : TIState (List TypeEquation)
    }


solveGroup : UnifyCfg -> List Member -> TIState ()
solveGroup cfg members =
    State.do State.enterLevel <|
        \() ->
            State.do
                (State.traverse
                    (\member ->
                        State.do (State.setIdLevel member.id) <|
                            \() ->
                                case member.maybeAnnotation of
                                    Just scheme ->
                                        member.install scheme

                                    Nothing ->
                                        member.install (Type.mono (Type.id_ member.id))
                    )
                    members
                )
            <|
                \_ ->
                    State.do (State.traverse .equations members) <|
                        \eqLists ->
                            let
                                droppedEqs : List ( MonoType, MonoType )
                                droppedEqs =
                                    eqLists
                                        |> List.concat
                                        |> List.map TypeEquation.dropLabel
                            in
                            State.do (Unify.unifyMany cfg droppedEqs) <|
                                \() ->
                                    State.do State.leaveLevel <|
                                        \() ->
                                            State.do
                                                (State.traverse
                                                    (\member ->
                                                        case member.maybeAnnotation of
                                                            Just _ ->
                                                                State.pure ()

                                                            Nothing ->
                                                                State.do (State.generalizeWith (Type.id_ member.id)) <|
                                                                    \scheme ->
                                                                        member.install scheme
                                                    )
                                                    members
                                                )
                                            <|
                                                \_ ->
                                                    State.pure ()
