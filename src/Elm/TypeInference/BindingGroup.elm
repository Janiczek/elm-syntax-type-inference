module Elm.TypeInference.BindingGroup exposing (Member, solveGroup)

{-| Solve a binding group (SCC of mutually-referencing bindings).
-}

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName as FullModuleName
import Elm.TypeInference.InferError exposing (InferErrorDetails(..))
import Elm.TypeInference.State as State exposing (StateM)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type.Internal as TypeI exposing (Id, MonoType(..), Type(..))
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
                (State.traverseUnit
                    (\member ->
                        State.do (State.setIdToCurrentLetRank member.id) <| \() ->
                        case member.annotation of
                            Just scheme ->
                                -- Trust the annotation
                                member.install scheme

                            Nothing ->
                                member.install (TypeI.mono (TypeI.id_ member.id))
                    )
                    members
                )
             <| \() ->
             State.do
                 (State.foldl
                     (\member accAcrossMembers ->
                         State.map
                             (\memberEqs ->
                                 List.foldr
                                     (\memberEq acc ->
                                         TypeEquation.dropLabel memberEq :: acc
                                     )
                                     accAcrossMembers
                                     memberEqs
                             )
                             member.equations
                     )
                     []
                     members
                 )
             <| \eqLists ->
             State.do
                 (eqLists
                     |> Unify.unifyMany cfg
                 )
             <| \() ->
             checkAnnotations cfg members
            )
        )
    <| \() ->
    State.traverseUnit
        (\member ->
            case member.annotation of
                Just _ ->
                    State.pureUnit

                Nothing ->
                    State.do (State.generalize (TypeI.id_ member.id)) <| \scheme ->
                    member.install scheme
        )
        members


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
    State.traverseUnit (\member -> checkOne cfg member) members


checkOne : UnifyConfig -> Member -> StateM ()
checkOne cfg member =
    case member.annotation of
        Nothing ->
            State.pureUnit

        Just (Forall boundVars annoMono) ->
            if List.isEmpty boundVars then
                State.pureUnit

            else
                State.do State.getSubst <| \subst ->
                let
                    ( finalMono, _, _ ) =
                        SubstitutionMap.substituteMono subst (TypeI.id_ member.id)
                in
                if shaderSlotsTooGeneral cfg annoMono finalMono then
                    let
                        ( pubAnno, pubFinal ) =
                            TypeI.toPublicPair cfg.moduleMapping annoMono finalMono
                    in
                    State.error
                        { moduleName = FullModuleName.toModuleName cfg.moduleName
                        , declarationNames = cfg.declarationNames
                        , details = TypeMismatch pubAnno pubFinal
                        }

                else if TypeI.monoHasTypeVars finalMono then
                    State.pureUnit

                else
                    let
                        ( pubAnno, pubFinal ) =
                            TypeI.toPublicPair cfg.moduleMapping annoMono finalMono
                    in
                    State.error
                        { moduleName = FullModuleName.toModuleName cfg.moduleName
                        , declarationNames = cfg.declarationNames
                        , details = TypeMismatch pubAnno pubFinal
                        }


{-|

    shader : Shader a b c
    shader =
        [glsl|
        attribute vec3 position;
    |]

is an error because the annotation is more general than the body.

-}
shaderSlotsTooGeneral : UnifyConfig -> MonoType -> MonoType -> Bool
shaderSlotsTooGeneral cfg annoMono finalMono =
    case annoMono of
        WebGLShader annoShader ->
            case finalMono of
                WebGLShader finalShader ->
                    List.map2 Tuple.pair
                        [ annoShader.attributes
                        , annoShader.uniforms
                        , annoShader.varyings
                        ]
                        [ finalShader.attributes
                        , finalShader.uniforms
                        , finalShader.varyings
                        ]
                        |> List.any
                            (\( annoSlot, finalSlot ) ->
                                case Unify.shaderSlotParts cfg.typeAliases annoSlot of
                                    Just anno ->
                                        case Unify.shaderSlotParts cfg.typeAliases finalSlot of
                                            Just final ->
                                                slotTooGeneral anno final

                                            Nothing ->
                                                False

                                    Nothing ->
                                        False
                            )

                _ ->
                    False

        _ ->
            False


slotTooGeneral :
    { extensionTypevar : MonoType, fields : Dict String MonoType }
    -> { extensionTypevar : MonoType, fields : Dict String MonoType }
    -> Bool
slotTooGeneral annoSlot finalSlot =
    case annoSlot.extensionTypevar of
        TypeVar _ ->
            if Dict.isEmpty annoSlot.fields then
                not (Dict.isEmpty finalSlot.fields)

            else
                not (Dict.isEmpty (Dict.diff annoSlot.fields finalSlot.fields))

        _ ->
            not (Dict.isEmpty (Dict.diff annoSlot.fields finalSlot.fields))
