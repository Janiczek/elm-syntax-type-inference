# elm-syntax-type-inference

Infer types of [elm-syntax](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/) AST.

```elm
-- A contrived example, normally this would live split out
-- inside `update` Msg handlers, state saved into Model, etc.

Elm.TypeInference.dependencyEnv 
    { directDependencies = ["elm/core"]
    , allDependencies = [...] -- Parsed from ~/.elm files
    , sourcesToResolveAmbiguity = Dict.empty
    }
--> Ready depEnv

Elm.TypeInference.project
    (Just "my/package-name")
    depEnv
    parsedFiles
--> Ok project

Elm.TypeInference.inferModule
    ["MyModule","Internal"]
    project
--> ( Ok myModuleInternalTLT, newProject )

TypeLookupTable.get
    someRange
    myModuleInternalTLT
--> ( Just someType, newTLT )
```
