module Elm.TypeInference.Interface exposing
    ( Interface
    , create
    , moduleIndex, values, typeAliases
    )

{-| What one module contributes to the modules that import it.

Inference runs one module at a time (Elm forbids import cycles, so a module can
always be inferred once its imports are done). An `Interface` is everything the
importing module needs: it replaces having the imported `File`s around.

The types in here are id-space independent: `State.generalizeWith` substitutes
before quantifying vars younger than the enclosing let, and
`State.lookupGlobalEnv` re-instantiates with fresh ids on every lookup. So an
interface stays valid no matter which `State` consumes it.

@docs Interface
@docs create
@docs moduleIndex, values, typeAliases

-}

import Dict exposing (Dict)
import Elm.Syntax.VarName exposing (VarName)
import Elm.TypeInference.ModuleIndex exposing (ModuleIndex)
import Elm.TypeInference.State exposing (GlobalKey)
import Elm.TypeInference.Type exposing (Type)
import Elm.TypeInference.Unify exposing (TypeAlias)


type Interface
    = Interface
        { moduleIndex : ModuleIndex
        , values : Dict VarName Type
        , typeAliases : Dict GlobalKey TypeAlias
        }


{-|

  - `moduleIndex` -- the declared/exposed names, imports and infix declarations
    that name resolution in the importing module needs.
  - `values` -- the module's _exposed_ values (functions, constructors, ports,
    record-alias constructors) with their generalized schemes.
  - `typeAliases` -- the module's own type aliases _plus_ every alias it
    inherited from its own imports. A type flowing out of this module's
    signatures can mention an alias the importer never imported itself, and
    `Unify.expandAlias` still has to be able to expand it. (Dependency aliases
    are not in here: they live in the `DependencyEnv`, which every module has
    anyway.)

-}
create :
    { moduleIndex : ModuleIndex
    , values : Dict VarName Type
    , typeAliases : Dict GlobalKey TypeAlias
    }
    -> Interface
create =
    Interface


moduleIndex : Interface -> ModuleIndex
moduleIndex (Interface i) =
    i.moduleIndex


values : Interface -> Dict VarName Type
values (Interface i) =
    i.values


typeAliases : Interface -> Dict GlobalKey TypeAlias
typeAliases (Interface i) =
    i.typeAliases
