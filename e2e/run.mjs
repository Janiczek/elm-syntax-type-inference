#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import os from "node:os";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";
import { execFileSync } from "node:child_process";

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
    } else {
      filters.push(arg);
    }
  }

  return filters;
}

function readJson(p) {
  return JSON.parse(fs.readFileSync(p, "utf8"));
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

// Runs `elm make` in the tested directory so the compiler downloads the deps.
// Compile errors are ignored (tests can be expected to fail).
function ensureDependenciesCached(projectDir, sourceFiles) {
  if (sourceFiles.length === 0) return;
  try {
    execFileSync(elmCompiler, ["make", sourceFiles[0], "--output=/dev/null"], {
      cwd: projectDir,
      stdio: "ignore",
    });
  } catch {}
}

function latestCachedVersion(name) {
  let entries;
  try {
    entries = fs.readdirSync(path.join(PACKAGES_DIR, name));
  } catch (e) {
    if (e?.code === "ENOENT") {
      console.warn(`warning: no cached versions for ${name}, skipping`);
      return undefined;
    }
    throw e;
  }
  return entries
    .filter((v) => /^\d+\.\d+\.\d+$/.test(v))
    .sort((a, b) => a.localeCompare(b, undefined, { numeric: true }))
    .at(-1);
}

function packageDeps(name, version) {
  if (!version) return [];
  try {
    return Object.keys(readJson(path.join(PACKAGES_DIR, name, version, "elm.json")).dependencies || {});
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
  const docsPath = path.join(PACKAGES_DIR, name, version, "docs.json");
  try {
    return readJson(docsPath);
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

// Resolves elm.json dependencies to exact versions (for packages, pick latest version).
// Loads their docs.json.
async function resolveDependencies(elmJson) {
  const versions = {};

  switch (elmJson.type) {
    case "application": {
      const { direct, indirect } = elmJson.dependencies;
      const test = elmJson["test-dependencies"] || {};
      Object.assign(versions, direct, indirect, test.direct, test.indirect);
      break;
    }

    case "package": {
      const queue = Object.keys(elmJson.dependencies);
      while (queue.length) {
        const name = queue.shift();
        if (versions[name]) continue;
        const version = latestCachedVersion(name);
        if (!version) continue;
        versions[name] = version;
        queue.push(...packageDeps(name, version));
      }
      break;
    }

    default:
      throw new Error(`Unknown elm.json type: ${elmJson.type}`);
  }

  const result = [];
  for (const [name, version] of Object.entries(versions)) {
    result.push({
      name,
      dependsOn: packageDeps(name, version),
      docsJson: await loadDocsJson(name, version),
    });
  }
  return result;
}

function buildRunner() {
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

function runOnce(flags) {
  const { Elm } = require(ELM_JS);
  const app = Elm.Runner.init({ flags });
  const resultPromise = new Promise((resolve) => {
    const handler = (value) => {
      app.ports.result.unsubscribe(handler);
      resolve(value);
    };
    app.ports.result.subscribe(handler);
  });
  return { app, resultPromise };
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

  const sourceFiles = findSourceFiles(projectDir, elmJson);
  ensureDependenciesCached(projectDir, sourceFiles);

  const flags = {
    sources: sourceFiles.map((f) => ({
      path: path.relative(projectDir, f),
      source: fs.readFileSync(f, "utf8"),
    })),
    directDependencies: directDependencyNames(elmJson),
    allDependencies: await resolveDependencies(elmJson),
  };

  const start = process.hrtime.bigint();
  const { app, resultPromise } = runOnce(flags);
  const result = await resultPromise;
  const elapsedSeconds = Number(process.hrtime.bigint() - start) / 1e9;

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
}

function csvEscape(value) {
  const s = String(value ?? "");
  return /[",\r\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
}

function printReport({ name, expected, result, passed, elapsedSeconds }) {
  const actual = result.ok ? "pass" : "fail";
  if (csvMode) {
    const error = result.ok ? "" : (result.error ?? "");
    console.log(
      [name, expected.expect, actual, passed, elapsedSeconds.toFixed(3), error].map(csvEscape).join(",")
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

  let passedCount = 0;
  if (csvMode) {
    console.log("test,expected,actual,passed,seconds,error");
  }
  for (const name of names) {
    const report = await runTest(name);
    if (report.passed) passedCount++;
  }

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

main();
