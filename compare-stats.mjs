#!/usr/bin/env node
// compare-stats.mjs — compare timing distributions of two e2e CSV runs.
//
// Usage:
//   node compare-stats.mjs <fileA.csv> <fileB.csv> [--column "total ms"] [--no-color]
//
// Computes min, p25, p50, p75, p99, max over the chosen milliseconds column
// (default: "total ms") and prints them side-by-side, coloring the smaller
// value of each row green.
//
// Percentiles use linear interpolation on the sorted values
// (rank = p/100 * (n-1)), same method as numpy's default.

import { readFileSync } from "node:fs";
import { basename } from "node:path";

const GREEN = "\x1b[32m";
const RESET = "\x1b[0m";

function usage() {
  console.log(`Usage: node compare-stats.mjs <fileA.csv> <fileB.csv> [--column "total ms"] [--no-color]

Compares min/p25/p50/p75/p99/max of a milliseconds column side-by-side.
Available ms columns are auto-detected from the header (e.g. "total ms",
"inference ms", "resolution ms"). Smaller value per row is shown in green.`);
}

function parseArgs(argv) {
  const positionals = [];
  let column = "total ms";
  let noColor = !!process.env.NO_COLOR;
  for (const arg of argv) {
    if (arg === "--help" || arg === "-h") {
      usage();
      process.exit(0);
    } else if (arg === "--no-color") {
      noColor = true;
    } else if (arg.startsWith("--column=")) {
      column = arg.slice("--column=".length);
    } else if (arg === "--column") {
      throw new Error("--column requires a value, e.g. --column=\"total ms\"");
    } else if (arg.startsWith("--")) {
      throw new Error(`Unknown flag: ${arg}`);
    } else {
      positionals.push(arg);
    }
  }
  if (positionals.length !== 2) {
    throw new Error("Expected exactly two CSV files.\n" + "Usage: node compare-stats.mjs <fileA.csv> <fileB.csv>");
  }
  return { fileA: positionals[0], fileB: positionals[1], column, noColor };
}

function splitCsvLine(line) {
  return line.replace(/\r$/, "").split(",");
}

function loadColumnValues(filePath, columnName) {
  const raw = readFileSync(filePath, "utf8");
  const lines = raw.split("\n").filter((l) => l.trim() !== "");
  if (lines.length < 2) throw new Error(`${filePath}: no data rows found`);
  const header = splitCsvLine(lines[0]);
  let idx = header.findIndex((h) => h.trim() === columnName);
  if (idx === -1) {
    idx = header.findIndex((h) => h.trim().toLowerCase() === columnName.toLowerCase());
  }
  if (idx === -1) {
    throw new Error(
      `${filePath}: column "${columnName}" not found. Available: ${header.join(", ")}`
    );
  }
  const values = [];
  for (let i = 1; i < lines.length; i++) {
    const cols = splitCsvLine(lines[i]);
    const v = parseFloat((cols[idx] ?? "").trim());
    if (Number.isFinite(v)) values.push(v);
  }
  if (values.length === 0) throw new Error(`${filePath}: no numeric values in column "${columnName}"`);
  values.sort((a, b) => a - b);
  return values;
}

function percentile(sorted, p) {
  const n = sorted.length;
  if (n === 1) return sorted[0];
  const rank = (p / 100) * (n - 1);
  const lo = Math.floor(rank);
  const hi = Math.ceil(rank);
  if (lo === hi) return sorted[lo];
  const frac = rank - lo;
  return sorted[lo] * (1 - frac) + sorted[hi] * frac;
}

function statsFor(sorted) {
  return [
    ["min", sorted[0]],
    ["p25", percentile(sorted, 25)],
    ["p50", percentile(sorted, 50)],
    ["p75", percentile(sorted, 75)],
    ["p99", percentile(sorted, 99)],
    ["max", sorted[sorted.length - 1]],
  ];
}

function main() {
  let args;
  try {
    args = parseArgs(process.argv.slice(2));
  } catch (e) {
    console.error(e.message);
    usage();
    process.exit(1);
  }
  let a, b;
  try {
    a = loadColumnValues(args.fileA, args.column);
    b = loadColumnValues(args.fileB, args.column);
  } catch (e) {
    console.error(e.message);
    process.exit(1);
  }

  const statsA = statsFor(a);
  const statsB = statsFor(b);
  const nameA = `${basename(args.fileA)} (n=${a.length})`;
  const nameB = `${basename(args.fileB)} (n=${b.length})`;
  const statHead = "stat";
  const fmt = (v) => v.toFixed(2);

  const rows = statsA.map(([name, va], i) => [name, fmt(va), fmt(statsB[i][1]), va, statsB[i][1]]);

  const w0 = Math.max(statHead.length, ...rows.map((r) => r[0].length));
  const w1 = Math.max(nameA.length, ...rows.map((r) => r[1].length));
  const w2 = Math.max(nameB.length, ...rows.map((r) => r[2].length));

  const pad = (s, w, right = false) => (right ? s.padStart(w) : s.padEnd(w));
  const colorize = (text, green) => (green && !args.noColor ? `${GREEN}${text}${RESET}` : text);

  console.log(`column: ${args.column} (ms)`);
  console.log(`${pad(statHead, w0)}  ${pad(nameA, w1)}  ${pad(nameB, w2)}`);
  console.log(`${"-".repeat(w0)}  ${"-".repeat(w1)}  ${"-".repeat(w2)}`);
  for (const [name, sa, sb, va, vb] of rows) {
    const aGreen = va < vb;
    const bGreen = vb < va;
    const ca = colorize(pad(sa, w1, true), aGreen);
    const cb = colorize(pad(sb, w2, true), bGreen);
    console.log(`${pad(name, w0)}  ${ca}  ${cb}`);
  }
}

main();
