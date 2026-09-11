module Main exposing (allowedAttributes, isAllowed)

import Set exposing (Set)


allowedAttributes : Set String
allowedAttributes =
    Set.fromList [ "gap", "align-items" ]


isAllowed : String -> Bool
isAllowed name =
    Set.member name allowedAttributes
