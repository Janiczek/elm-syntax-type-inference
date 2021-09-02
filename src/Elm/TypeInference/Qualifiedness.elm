module Elm.TypeInference.Qualifiedness exposing
    ( PossiblyQualified(..)
    , Qualified(..)
    , fromModuleName
    )

{-| These phantom types are just simple wrappers for what they hold
(Maybe ModuleName and ModuleName), they exist just to give a name; just to make
stuff like `Type (Maybe ModuleName)` less confusing (-> `Type PossiblyQualified`)
-}

import Elm.Syntax.ModuleName exposing (ModuleName)


fromModuleName : ModuleName -> PossiblyQualified
fromModuleName moduleName =
    case moduleName of
        [] ->
            PossiblyQualified Nothing

        _ ->
            PossiblyQualified (Just moduleName)


type PossiblyQualified
    = PossiblyQualified (Maybe ModuleName)


type Qualified
    = Qualified ModuleName
