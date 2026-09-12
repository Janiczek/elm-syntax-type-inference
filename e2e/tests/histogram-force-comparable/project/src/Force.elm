module Force exposing (Entity, entity)


type alias Entity comparable =
    comparable


entity : Int -> Entity Int
entity index =
    index
