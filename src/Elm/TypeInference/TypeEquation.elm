module Elm.TypeInference.TypeEquation exposing (TypeEquation, dropLabel, monoVarCounterpart, toString)

import Elm.TypeInference.Type as Type exposing (Id, Type(..))


type alias TypeEquation =
    ( Type, Type, String )


toString : TypeEquation -> String
toString ( t1, t2, source ) =
    [ t1, t2 ]
        |> List.map Type.toString
        |> String.join " ≡ "
        |> (\eq -> eq ++ " (" ++ source ++ ")")


dropLabel : TypeEquation -> ( Type, Type )
dropLabel ( t1, t2, _ ) =
    ( t1, t2 )


{-| Used in instantiation of generalized types (let-polymorphism).
Will return the type corresponding to the variable.
-}
monoVarCounterpart : Id -> TypeEquation -> Maybe Type
monoVarCounterpart id ( t1, t2, _ ) =
    let
        idType =
            Type.id id
    in
    if t1 == idType then
        Just t2

    else if t2 == idType then
        Just t1

    else
        Nothing
