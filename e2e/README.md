# e2e test suite

Runs `Elm.TypeInference.infer` on the folders in `tests/`.

`tests/<name>/expected.json` says the intended result of the test:

```json
{ 
  "expect": "pass" | "fail",
  "note": "any context you want to provide"
}
```

## Usage

```sh
# high-level
npm run test:e2e

# low-level
./e2e/run.mjs         # run all e2e tests
./e2e/run.mjs <name>  # run only e2e/tests/<name>

# uses `elm` from PATH; override with --compiler:
./e2e/run.mjs --compiler lamdera

# write `tests/<name>/inferred-types.txt` for debugging (off by default):
./e2e/run.mjs --write-types
```

Exit code is 0 if all pased, 1 otherwise.

Pass `--write-types` to write `tests/<name>/inferred-types.txt`.
