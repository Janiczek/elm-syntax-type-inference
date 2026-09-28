# elm-syntax-type-inference

Infer types of [elm-syntax](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/) AST.

```elm
-- A contrived example, normally this would live split out
-- inside `update` Msg handlers, state saved into Model, etc.

Elm.TypeInference.init
    { directDependencies = ["elm/core"]
    , allDependencies = [...] -- Parsed from ~/.elm files
    , sourcesToResolveAmbiguity = Dict.empty
    , projectPackageName = Just "my/package-name"
    , projectFiles = Dict.fromList [ ( [ "MyModule", "Internal" ], parsedFile ) ]
    }
--> Ok project

Elm.TypeInference.getType
    ["MyModule","Internal"]
    someRange
    project
--> ( Ok someType, newProject )

Elm.TypeInference.addFile updatedFile project
--> Ok newerProject

Elm.TypeInference.removeFile ["MyModule","Internal"] project
--> newerProject
```

If `project` fails with `details = NeedPackageSources needed`, read and parse
them and retry with them added to the `sourcesToResolveAmbiguity` field.
