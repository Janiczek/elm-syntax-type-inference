module Force exposing ( Entity, applyForce )

import Dict exposing (Dict)

type alias Entity comparable a =
    { a
        | x : Float
        , id : comparable
    }

applyForce : Dict comparable (Entity comparable a) -> Dict comparable (Entity comparable a)
applyForce entities =
    Dict.map (\_ ent -> { ent | x = 0 }) entities
