module Elm.TypeInference.Qualifiedness exposing
    ( PossiblyQualified(..)
    , Qualified(..)
    , fromModuleName
    )

{-| These phantom types are just simple wrappers for what they hold
(Maybe ModuleName and ModuleName), they exist just to give a name; just to make
stuff like `Type (Maybe ModuleName)` less confusing (-> `Type PossiblyQualified`)
-}

import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)


fromModuleName : ModuleName -> PossiblyQualified
fromModuleName moduleName =
    PossiblyQualified <| FullModuleName.fromModuleName moduleName


type PossiblyQualified
    = PossiblyQualified (Maybe FullModuleName)


type Qualified
    = Qualified FullModuleName
