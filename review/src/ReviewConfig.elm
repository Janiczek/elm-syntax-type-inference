module ReviewConfig exposing (config)

import NoDebug.Log
import NoDebug.TodoOrToString
import NoSlowConcat
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoDebug.Log.rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests" ]
    , NoDebug.TodoOrToString.rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests" ]

    -- custom
    , NoSlowConcat.rule
    ]
