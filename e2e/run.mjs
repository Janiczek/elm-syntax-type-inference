#!/usr/bin/env node

// Vibeslopped (sorry!)

import fs from "node:fs";
import path from "node:path";
import os from "node:os";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";
import { execFileSync, spawn } from "node:child_process";
import solver from "elm-solve-deps-wasm";

solver.init();

const ELM_VERSION = "0.19.2";
const ELM_HOME = process.env.ELM_HOME || path.join(os.homedir(), ".elm");
const PACKAGES_DIR = path.join(ELM_HOME, ELM_VERSION, "packages");

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const require = createRequire(import.meta.url);
const TESTS_DIR = path.join(__dirname, "tests");
const ELM_JS = path.join(__dirname, "elm.js");

// `elm` from PATH unless --compiler points somewhere else.
let elmCompiler = "elm";
// Skip writing inferred-types.txt unless --write-types is passed.
let writeTypes = false;
// Machine-readable CSV on stdout instead of human-readable lines.
let csvMode = false;
let warmupDeps = true;
let buildMode = "auto"; // vs "skip" and "rebuild"
let jobs = 1;
// When --jobs > 1, skips CSV header and the summary:
let asShard = false;

function parseJobs(value) {
  const n = Number.parseInt(value, 10);
  if (!Number.isInteger(n) || n < 1) {
    console.error(`--jobs needs a positive integer, got: ${value}`);
    process.exit(1);
  }
  return n;
}

function parseArgs(argv) {
  const filters = [];

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];

    // Don't you love parsing arguments
    if (arg === "--compiler") {
      elmCompiler = argv[++i];
      if (!elmCompiler) {
        console.error("--compiler needs a path");
        process.exit(1);
      }
    } else if (arg.startsWith("--compiler=")) {
      elmCompiler = arg.slice("--compiler=".length);
    } else if (arg === "--write-types") {
      writeTypes = true;
    } else if (arg === "--no-write-types") {
      writeTypes = false;
    } else if (arg === "--csv") {
      csvMode = true;
    } else if (arg === "--no-csv") {
      csvMode = false;
    } else if (arg === "--warmup-deps") {
      warmupDeps = true;
    } else if (arg === "--no-warmup-deps") {
      warmupDeps = false;
    } else if (arg === "--skip-build") {
      buildMode = "skip";
    } else if (arg === "--rebuild") {
      buildMode = "rebuild";
    } else if (arg === "--jobs") {
      jobs = parseJobs(argv[++i]);
    } else if (arg.startsWith("--jobs=")) {
      jobs = parseJobs(arg.slice("--jobs=".length));
    } else if (arg === "--as-shard") {
      asShard = true;
    } else {
      filters.push(arg);
    }
  }

  return filters;
}

function readJson(p) {
  return JSON.parse(fs.readFileSync(p, "utf8"));
}

function formatError(e) {
  const message = e?.message ?? String(e);
  return `run.mjs error: ${message}`;
}

// Caches to make batch-testing faster:
const packageJsonCache = new Map(); // absolute path -> parsed JSON
const docsJsonCache = new Map(); // "name@version" -> parsed docs.json
const cachedVersionsCache = new Map(); // package name -> [versions]
const solutionCache = new Map(); // elm.json text -> { name: version }
const packageSourcesCache = new Map(); // "name@version" -> { name, sources }

function clearResolverCaches() {
  cachedVersionsCache.clear();
  solutionCache.clear();
}

// memoized readJson, called for files under PACKAGES_DIR
function readCachedPackageJson(p) {
  const hit = packageJsonCache.get(p);
  if (hit !== undefined) return hit;
  const parsed = readJson(p);
  packageJsonCache.set(p, parsed);
  return parsed;
}

function findElmFiles(dir) {
  return fs
    .readdirSync(dir, { recursive: true })
    .filter((f) => f.endsWith(".elm"))
    .map((f) => path.join(dir, f));
}

function findSourceFiles(projectDir, elmJson) {
  const sourceDirs = elmJson["source-directories"] || ["src"];
  return sourceDirs
    .map((sd) => path.resolve(projectDir, sd))
    .filter((dir) => fs.existsSync(dir))
    .flatMap(findElmFiles);
}

// Test only exposed modules (+ transitive deps - resovled in Runner.elm).
// Ignore other files in src/.
function exposedModulesFor(elmJson) {
  if (elmJson.type !== "package") return null;
  const em = elmJson["exposed-modules"];
  if (Array.isArray(em)) return em.filter((m) => typeof m === "string");
  if (em && typeof em === "object") {
    return Object.values(em)
      .flat()
      .filter((m) => typeof m === "string");
  }
  return null;
}

// Warm the ELM_HOME cache with an exposed file.
function pickWarmupFile(projectDir, sourceFiles, elmJson) {
  const exposed = exposedModulesFor(elmJson);
  if (exposed && exposed.length > 0) {
    for (const mod of exposed) {
      const suffix = path.join(...mod.split(".")) + ".elm";
      const hit = sourceFiles.find((f) => path.relative(projectDir, f).endsWith(suffix));
      if (hit) return hit;
    }
  }
  return sourceFiles[0];
}

// Runs `elm make` in the tested directory so the compiler downloads the deps.
// Compile errors are ignored (tests can be expected to fail).
// `elm-version` is relaxed to allow us to test with 0.19.2.
function ensureDependenciesCached(projectDir, sourceFiles) {
  if (sourceFiles.length === 0) return;

  const elmJsonPath = path.join(projectDir, "elm.json");
  const original = fs.readFileSync(elmJsonPath, "utf8");
  const elmJson = JSON.parse(original);
  const patched = elmJson.type === "package" && typeof elmJson["elm-version"] === "string";
  if (patched) {
    elmJson["elm-version"] = "0.1.0 <= v < 2.0.0";
    fs.writeFileSync(elmJsonPath, JSON.stringify(elmJson));
  }

  try {
    execFileSync(elmCompiler, ["make", sourceFiles[0], "--output=/dev/null"], {
      cwd: projectDir,
      stdio: "ignore",
    });
  } catch {
  } finally {
    if (patched) fs.writeFileSync(elmJsonPath, original);
  }
}

function versionReadyOnDisk(name, version) {
  if (!version) return false;
  const base = path.join(PACKAGES_DIR, name, version);
  return fs.existsSync(path.join(base, "elm.json")) && fs.existsSync(path.join(base, "src"));
}

function dependenciesReadyOnDisk(versions) {
  return Object.entries(versions).every(([name, version]) => versionReadyOnDisk(name, version));
}

function maybeWarmupDependencies(projectDir, warmupFiles, elmJson) {
  if (warmupFiles.length === 0) return null;
  let versions = null;
  try {
    versions = versionsFor(elmJson);
  } catch {
    versions = null;
  }
  if (
    versions !== null &&
    dependenciesReadyOnDisk(versions) &&
    fs.existsSync(path.join(projectDir, "elm-stuff"))
  ) {
    return versions;
  }
  ensureDependenciesCached(projectDir, warmupFiles);
  clearResolverCaches();
  return null;
}

function cachedVersions(name) {
  const hit = cachedVersionsCache.get(name);
  if (hit !== undefined) return hit;
  let entries;
  try {
    entries = fs.readdirSync(path.join(PACKAGES_DIR, name));
  } catch (e) {
    if (e?.code === "ENOENT") {
      return [];
    }
    throw e;
  }
  const versions = entries
    .filter((v) => /^\d+\.\d+\.\d+$/.test(v))
    .sort((a, b) => b.localeCompare(a, undefined, { numeric: true }));
  cachedVersionsCache.set(name, versions);
  return versions;
}

function packageDeps(name, version) {
  if (!version) return [];
  try {
    return Object.keys(readCachedPackageJson(path.join(PACKAGES_DIR, name, version, "elm.json")).dependencies || {});
  } catch (e) {
    if (e?.code === "ENOENT") {
      console.warn(`warning: missing elm.json for ${name}@${version}, assuming no transitive deps`);
      return [];
    }
    throw e;
  }
}

// docs.json is not always present in ~/.elm, download from package.elm-lang.org
async function loadDocsJson(name, version) {
  const key = `${name}@${version}`;
  const hit = docsJsonCache.get(key);
  if (hit !== undefined) return hit;
  const docsPath = path.join(PACKAGES_DIR, name, version, "docs.json");
  try {
    const docs = readJson(docsPath);
    docsJsonCache.set(key, docs);
    return docs;
  } catch (e) {
    if (e?.code !== "ENOENT") throw e;
  }

  console.warn(`warning: docs.json missing for ${name}@${version}, fetching from package.elm-lang.org...`);
  try {
    const res = await fetch(`https://package.elm-lang.org/packages/${name}/${version}/docs.json`);
    if (!res.ok) throw new Error(`HTTP ${res.status} ${res.statusText}`);
    const docs = await res.json();
    try {
      fs.mkdirSync(path.dirname(docsPath), { recursive: true });
      fs.writeFileSync(docsPath, JSON.stringify(docs));
    } catch {}
    docsJsonCache.set(key, docs);
    return docs;
  } catch (fetchError) {
    console.warn(
      `warning: could not fetch docs.json for ${name}@${version}: ${fetchError?.message ?? fetchError}. Using empty docs.`
    );
    return [];
  }
}

function directDependencyNames(elmJson) {
  switch (elmJson.type) {
    case "application": {
      const { direct } = elmJson.dependencies;
      return Object.keys(direct);
    }

    case "package":
      return Object.keys(elmJson.dependencies);

    default:
      throw new Error(`Unknown elm.json type: ${elmJson.type}`);
  }
}

// Resolve the whole graph, not just each package's newest cached version:
// transitive dependencies can further constrain a direct dependency's range.
function resolvePackageVersions(elmJson, readPackageJson, listVersions) {
  const key = JSON.stringify(elmJson);
  const hit = solutionCache.get(key);
  if (hit !== undefined) return hit;
  const solution = JSON.parse(
    solver.solve_deps(
      JSON.stringify(elmJson),
      false,
      {},
      (name, version) => JSON.stringify(readPackageJson(name, version)),
      listVersions
    )
  );
  const versions = { ...solution.direct, ...solution.indirect };
  solutionCache.set(key, versions);
  return versions;
}

function versionsFor(elmJson) {
  switch (elmJson.type) {
    case "application": {
      const { direct, indirect } = elmJson.dependencies;
      const test = elmJson["test-dependencies"] || {};
      return { ...direct, ...indirect, ...test.direct, ...test.indirect };
    }

    case "package":
      return resolvePackageVersions(
        elmJson,
        (name, version) => readCachedPackageJson(path.join(PACKAGES_DIR, name, version, "elm.json")),
        cachedVersions
      );

    default:
      throw new Error(`Unknown elm.json type: ${elmJson.type}`);
  }
}

async function resolveDependencies(elmJson, preResolved = null) {
  const versions = preResolved ?? versionsFor(elmJson);

  const dependencies = await Promise.all(
    Object.entries(versions).map(async ([name, version]) => ({
      name,
      dependsOn: packageDeps(name, version),
      docsJson: await loadDocsJson(name, version),
    }))
  );
  return { dependencies, versions };
}

// Lazily load dependency's `.elm` files for a `requestPackageSources` round-trip.
function loadPackageSources(name, version) {
  if (!version) {
    console.warn(`warning: no cached version for ${name}, continuing without its sources`);
    return { name, sources: [] };
  }
  const key = `${name}@${version}`;
  const hit = packageSourcesCache.get(key);
  if (hit !== undefined) return hit;
  let files;
  try {
    files = findElmFiles(path.join(PACKAGES_DIR, name, version, "src"));
  } catch (e) {
    if (e?.code === "ENOENT") {
      console.warn(`warning: no sources for ${name}@${version}, continuing without them`);
      return { name, sources: [] };
    }
    throw e;
  }
  const result = {
    name,
    sources: files.map((file) => ({ path: file, source: fs.readFileSync(file, "utf8") })),
  };
  packageSourcesCache.set(key, result);
  return result;
}

function buildRunner() {
  if (buildMode === "skip") {
    if (!fs.existsSync(ELM_JS)) doBuildRunner();
    return;
  }
  if (buildMode !== "rebuild" && isRunnerFresh()) return;
  doBuildRunner();
}

function collectRunnerInputs() {
  const inputs = [path.join(__dirname, "elm.json")];
  for (const dir of [path.join(__dirname, "src"), path.join(__dirname, "..", "src")]) {
    inputs.push(...findElmFiles(dir));
  }
  return inputs;
}

function isRunnerFresh() {
  let outStat;
  try {
    outStat = fs.statSync(ELM_JS);
  } catch {
    return false;
  }
  try {
    for (const input of collectRunnerInputs()) {
      if (fs.statSync(input).mtimeMs >= outStat.mtimeMs) return false;
    }
  } catch {
    return false; // unreadable input: fall back to rebuilding
  }
  return true;
}

function doBuildRunner() {
  try {
    execFileSync(elmCompiler, ["make", "src/Runner.elm", "--optimize", "--output=elm.js"], {
      cwd: __dirname,
      stdio: "pipe",
    });
  } catch (e) {
    process.stderr.write(e.stdout ?? "");
    process.stderr.write(e.stderr ?? "");
    throw e;
  }
}

function runLazy(flags, versions) {
  const { Elm } = require(ELM_JS);
  const app = Elm.Runner.init({ flags });
  const provided = new Set();
  const nowMs = () => Number(process.hrtime.bigint()) / 1e6;
  let rounds = 0;

  // Benchmark state.
  // We only hold last attempt (`inferenceStarted`..`inferenceStopped`).
  let inferenceStart = null;
  let inferenceStop = null;
  let finalResult = null;
  const outcome = new Promise((resolve) => {
    const cleanup = () => {
      app.ports.result.unsubscribe(onResult);
      app.ports.requestPackageSources.unsubscribe(onSourcesRequest);
      app.ports.inferenceStarted.unsubscribe(onInferenceStarted);
      app.ports.inferenceStopped.unsubscribe(onInferenceStopped);
    };
    const maybeFinish = () => {
      // `result` may arrive before `inferenceStopped`, wait for both
      if (finalResult !== null && (inferenceStop !== null || inferenceStart === null)) {
        cleanup();
        resolve({
          result: finalResult,
          inferenceMs: inferenceStop !== null ? inferenceStop - inferenceStart : null,
        });
      }
    };
    const onResult = (value) => {
      finalResult = value;
      maybeFinish();
    };
    const onSourcesRequest = (packages) => {
      rounds += 1;
      const fresh = packages.filter((name) => !provided.has(name));
      if (rounds > 10 || fresh.length === 0) {
        cleanup();
        resolve({
          result: {
            ok: false,
            error: `could not load package sources for: ${packages.join(", ")}`,
          },
          inferenceMs: null,
        });
        return;
      }
      const payload = fresh.map((name) => {
        provided.add(name);
        return loadPackageSources(name, versions[name]);
      });
      app.ports.providePackageSources.send(payload);
    };
    const onInferenceStarted = () => {
      inferenceStart = nowMs();
      inferenceStop = null;
      app.ports.beginInference.send(null);
    };
    const onInferenceStopped = () => {
      inferenceStop = nowMs();
      maybeFinish();
    };
    app.ports.result.subscribe(onResult);
    app.ports.requestPackageSources.subscribe(onSourcesRequest);
    app.ports.inferenceStarted.subscribe(onInferenceStarted);
    app.ports.inferenceStopped.subscribe(onInferenceStopped);
  });
  return { app, outcome };
}

function requestInferredTypes(app) {
  return new Promise((resolve) => {
    const handler = (value) => {
      app.ports.inferredTypes.unsubscribe(handler);
      resolve(value);
    };
    app.ports.inferredTypes.subscribe(handler);
    app.ports.requestInferredTypes.send(null);
  });
}

function discoverTests(filters) {
  const names = fs
    .readdirSync(TESTS_DIR, { withFileTypes: true })
    .filter((e) => e.isDirectory())
    .map((e) => e.name)
    .sort();

  if (filters.length === 0) return names;
  return names.filter((n) => filters.some((f) => n === f));
}

async function runTest(name) {
  if (!csvMode) process.stdout.write(name);
  const testDir = path.join(TESTS_DIR, name);
  const projectDir = path.join(testDir, "project");
  const expected = readJson(path.join(testDir, "expected.json"));
  const elmJson = readJson(path.join(projectDir, "elm.json"));

  try {
    const sourceFiles = findSourceFiles(projectDir, elmJson);
    let preResolved = null;
    if (warmupDeps) {
      preResolved = maybeWarmupDependencies(
        projectDir,
        [pickWarmupFile(projectDir, sourceFiles, elmJson)].filter(Boolean),
        elmJson
      );
    }

    const { dependencies, versions } = await resolveDependencies(elmJson, preResolved);
    const flags = {
      sources: sourceFiles.map((f) => ({
        path: path.relative(projectDir, f),
        source: fs.readFileSync(f, "utf8"),
      })),
      directDependencies: directDependencyNames(elmJson),
      allDependencies: dependencies,
      exposedModules: exposedModulesFor(elmJson),
      currentPackage: elmJson.type === "package" ? elmJson.name : null,
    };

    const start = process.hrtime.bigint();
    const { app, outcome } = runLazy(flags, versions);
    const { result, inferenceMs } = await outcome;
    const elapsedSeconds =
      inferenceMs !== null ? inferenceMs / 1000 : Number(process.hrtime.bigint() - start) / 1e9;

    const passed = result.ok === (expected.expect === "pass");
    const report = { name, expected, result, passed, elapsedSeconds };
    printReport(report);

    // Outside benchmarked time: only on success ask Elm to serialize
    // the tables and save them to a file. Skipped unless --write-types.
    if (writeTypes) {
      let inferredTypes = "";
      if (result.ok) {
        const out = csvMode ? process.stderr : process.stdout;
        if (out.isTTY) {
          out.write("Writing types to inferred-types.txt...");
        }
        inferredTypes = await requestInferredTypes(app);
        fs.writeFileSync(path.join(testDir, "inferred-types.txt"), inferredTypes, "utf8");
        if (out.isTTY) {
          out.clearLine(0);
          out.cursorTo(0);
        }
      } else {
        fs.writeFileSync(path.join(testDir, "inferred-types.txt"), inferredTypes, "utf8");
      }
    }

    return report;
  } catch (e) {
    // e.g. dependency solver found no valid solution
    // report as failure and let the suite continue.
    const result = { ok: false, error: formatError(e) };
    const report = { name, expected, result, passed: false, elapsedSeconds: 0 };
    printReport(report);
    return report;
  }
}

function csvEscape(value) {
  const s = String(value ?? "");
  return /[",\r\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
}

function printReport({ name, expected, result, passed, elapsedSeconds }) {
  const actual = result.ok ? "pass" : "fail";
  if (csvMode) {
    const rawError = result.ok ? "" : (result.error ?? "");
    const error = expected.expect === "fail" && passed ? "" : rawError;
    // `test,` prefix already written early in main loop, finish rest of line.
    console.log(
      [expected.expect, actual, passed, elapsedSeconds.toFixed(4), error].map(csvEscape).join(",")
    );
    return;
  }
  const suffix = passed ? "" : `  (expected: ${expected.expect}, actual: ${actual})`;
  console.log(` ${passed ? "✓ PASS" : "✗ FAIL"} (${elapsedSeconds.toFixed(3)}s)${suffix}`);

  if (!result.ok && result.error) {
    console.log(`    error: ${result.error}`);
  }
}

async function main() {
  const filters = parseArgs(process.argv.slice(2));

  buildRunner();

  const names = discoverTests(filters);
  if (names.length === 0) {
    console.error(`No tests found under ${TESTS_DIR}${filters.length ? ` matching: ${filters.join(", ")}` : ""}`);
    process.exit(1);
  }

  if (!asShard && jobs > 1 && names.length > 1) {
    await runSharded(names, Math.min(jobs, names.length));
    return;
  }

  let passedCount = 0;
  if (csvMode && !asShard) {
    console.log("test,expected,actual,passed,seconds,error");
  }
  for (const name of names) {
    if (csvMode) process.stdout.write(`${csvEscape(name)},`);
    try {
      const report = await runTest(name);
      if (report.passed) passedCount++;
    } catch (e) {
      // runTest already handles errors after expected.json loads;
      // this is a last resort (e.g. missing expected.json) so the suite continues.
      // CSV `test,` prefix already written above, finish rest of line.
      const error = formatError(e);
      if (csvMode) {
        console.log(["?", "fail", false, (0).toFixed(3), error].map(csvEscape).join(","));
      } else {
        console.log(` ✗ FAIL (0.000s)`);
        console.log(`    error: ${error}`);
      }
    }
  }

  const summary = `${passedCount}/${names.length} test${names.length > 1 ? "s" : ""} passed`;
  if (asShard) {
    console.error(`${SHARD_DONE_PREFIX}passed=${passedCount} total=${names.length}`);
  } else if (csvMode) {
    console.error("");
    console.error(summary);
  } else {
    console.log("");
    console.log(summary);
  }
  process.exit(passedCount === names.length ? 0 : 1);
}

// --- --jobs sharding -------------------------------------------------------

const SHARD_DONE_PREFIX = "run.mjs shard done: ";

function parentArgsForShard() {
  const raw = process.argv.slice(2);
  const out = [];
  for (let i = 0; i < raw.length; i++) {
    const a = raw[i];
    if (a === "--jobs") {
      i++;
      continue;
    }
    if (a.startsWith("--jobs=") || a === "--as-shard") continue;
    out.push(a);
  }
  return out;
}

async function runSharded(names, jobCount) {
  const buckets = Array.from({ length: jobCount }, () => []);
  names.forEach((n, i) => buckets[i % jobCount].push(n));
  const chunks = buckets.filter((b) => b.length > 0);
  if (csvMode) {
    console.log("test,expected,actual,passed,seconds,error");
  }
  let passedCount = 0;
  await Promise.all(chunks.map((chunk) => runShard(chunk).then((n) => (passedCount += n))));

  const summary = `${passedCount}/${names.length} test${names.length > 1 ? "s" : ""} passed`;
  if (csvMode) {
    console.error("");
    console.error(summary);
  } else {
    console.log("");
    console.log(summary);
  }
  process.exit(passedCount === names.length ? 0 : 1);
}

function runShard(chunk) {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [process.argv[1], ...parentArgsForShard(), "--as-shard", ...chunk], {
      stdio: ["ignore", "pipe", "pipe"],
    });
    let outBuf = "";
    let errBuf = "";
    let done = null;
    const takeDone = (line) => {
      const m = /^run\.mjs shard done: passed=(\d+) total=(\d+)$/.exec(line);
      if (m) {
        done = { passed: Number(m[1]), total: Number(m[2]) };
        return true;
      }
      return false;
    };
    child.stdout.on("data", (d) => {
      // Line-buffered (don't interleave mid-line)
      outBuf += d;
      let idx;
      while ((idx = outBuf.indexOf("\n")) >= 0) {
        process.stdout.write(outBuf.slice(0, idx + 1));
        outBuf = outBuf.slice(idx + 1);
      }
    });
    child.stderr.on("data", (d) => {
      errBuf += d;
      let idx;
      while ((idx = errBuf.indexOf("\n")) >= 0) {
        const line = errBuf.slice(0, idx);
        errBuf = errBuf.slice(idx + 1);
        if (!takeDone(line)) process.stderr.write(line + "\n");
      }
    });
    child.on("error", (e) => {
      process.stderr.write(`run.mjs shard failed to start (${chunk.length} tests): ${e?.message ?? e}\n`);
      resolve(0);
    });
    child.on("close", (code) => {
      if (outBuf.length > 0) process.stdout.write(outBuf);
      if (errBuf.length > 0) {
        if (!takeDone(errBuf)) process.stderr.write(errBuf);
      }
      if (done !== null && done.total === chunk.length) {
        resolve(done.passed);
      } else {
        process.stderr.write(`run.mjs shard exited (code=${code}) without reporting (${chunk.length} tests)\n`);
        resolve(0);
      }
    });
  });
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
