module TypeLookupTable.Internal exposing (TypeLookupTable(..), empty)

import Dict exposing (Dict)
import Elm.TypeInference.ModuleIds as ModuleIds
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type exposing (Type)
import Elm.TypeInference.Type.Internal exposing (Id)
import RangeLike exposing (RangeLike)


type TypeLookupTable
    = TLT
        { nodeIds : Dict RangeLike Id
        , subst : SubstitutionMap.SubstitutionMap
        , moduleMapping : ModuleIds.Mapping
        , cache : Dict Id Type
        , pool : Dict String Type
        }


empty : TypeLookupTable
empty =
    TLT
        { nodeIds = Dict.empty
        , subst = SubstitutionMap.empty
        , moduleMapping = ModuleIds.empty
        , cache = Dict.empty
        , pool = Dict.empty
        }
