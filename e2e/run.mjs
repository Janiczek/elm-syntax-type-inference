#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import os from "node:os";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";
import { execFileSync } from "node:child_process";

const ELM_VERSION = "0.19.1";
const ELM_HOME = process.env.ELM_HOME || path.join(os.homedir(), ".elm");
const PACKAGES_DIR = path.join(ELM_HOME, ELM_VERSION, "packages");

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const require = createRequire(import.meta.url);
const TESTS_DIR = path.join(__dirname, "tests");
const ELM_JS = path.join(__dirname, "elm.js");

// `elm` from PATH unless --compiler points somewhere else.
let elmCompiler = "elm";

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
  return fs
    .readdirSync(path.join(PACKAGES_DIR, name))
    .filter((v) => /^\d+\.\d+\.\d+$/.test(v))
    .sort((a, b) => a.localeCompare(b, undefined, { numeric: true }))
    .at(-1);
}

function packageDeps(name, version) {
  return Object.keys(readJson(path.join(PACKAGES_DIR, name, version, "elm.json")).dependencies || {});
}

// Resolves elm.json dependencies to exact versions (for packages, pick latest version).
// Loads their docs.json.
function resolveDependencies(elmJson) {
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
        versions[name] = version;
        queue.push(...packageDeps(name, version));
      }
      break;
    }

    default:
      throw new Error(`Unknown elm.json type: ${elmJson.type}`);
  }

  return Object.entries(versions).map(([name, version]) => ({
    name,
    dependsOn: packageDeps(name, version),
    docsJson: readJson(path.join(PACKAGES_DIR, name, version, "docs.json")),
  }));
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
  return new Promise((resolve) => {
    Elm.Runner.init({ flags }).ports.result.subscribe(resolve);
  });
}

function discoverTests(filters) {
  const names = fs
    .readdirSync(TESTS_DIR, { withFileTypes: true })
    .filter((e) => e.isDirectory())
    .map((e) => e.name)
    .sort();

  if (filters.length === 0) return names;
  return names.filter((n) => filters.some((f) => n.includes(f)));
}

async function runTest(name) {
  const testDir = path.join(TESTS_DIR, name);
  const projectDir = path.join(testDir, "project");
  const expected = readJson(path.join(testDir, "expected.json"));
  const elmJson = readJson(path.join(projectDir, "elm.json"));

  const sourceFiles = findSourceFiles(projectDir, elmJson);
  ensureDependenciesCached(projectDir, sourceFiles);

  const result = await runOnce({
    sources: sourceFiles.map((f) => ({
      path: path.relative(projectDir, f),
      source: fs.readFileSync(f, "utf8"),
    })),
    dependencies: resolveDependencies(elmJson),
  });

  const passed = result.ok === (expected.expect === "pass");
  return { name, expected, result, passed };
}

function printReport({ name, expected, result, passed }) {
  const actual = result.ok ? "pass" : "fail";
  const suffix = passed ? "" : `  (expected: ${expected.expect}, actual: ${actual})`;
  console.log(`${passed ? "✓ PASS" : "✗ FAIL"}  ${name}${suffix}`);

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
  for (const name of names) {
    const report = await runTest(name);
    printReport(report);
    if (report.passed) passedCount++;
  }

  console.log("");
  console.log(`${passedCount}/${names.length} test${names.length > 1 ? "s" : ""} passed`);
  process.exit(passedCount === names.length ? 0 : 1);
}

main();
