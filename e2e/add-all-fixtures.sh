#!/usr/bin/env bash
#
# Clone every package from all_packages_20260916.json as an e2e fixture.
#
# Usage:
#   ./e2e/add-all-fixtures.sh
#   SLEEP=5 ./e2e/add-all-fixtures.sh
#
# Env vars:
#   SLEEP=<sec>      sleep this long between attempts (default 2)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JSON_FILE="$SCRIPT_DIR/../all_packages_20260916.json"
TESTS_DIR="$SCRIPT_DIR/tests"
ADD_FIXTURE="$SCRIPT_DIR/add-fixture.sh"
FAILURES_LOG="$SCRIPT_DIR/add-all-fixtures.failures.log"

SLEEP="${SLEEP:-2}"

if ! command -v jq >/dev/null 2>&1; then
  echo "error: jq is required but not found on PATH" >&2
  exit 1
fi

if [ ! -f "$JSON_FILE" ]; then
  echo "error: package list not found: $JSON_FILE" >&2
  exit 1
fi

if [ ! -x "$ADD_FIXTURE" ]; then
  echo "error: add-fixture.sh not found or not executable: $ADD_FIXTURE" >&2
  exit 1
fi

warm_elm_cache() {
  local FIXTURE_NAME="$1"
  local DIR="$TESTS_DIR/$FIXTURE_NAME/project"
  if ! command -v elm >/dev/null 2>&1; then
    echo "warning: elm not found on PATH, skipping dependency pre-download" >&2
    return 0
  fi
  local FIRST_ELM
  FIRST_ELM="$(find "$DIR" -name '*.elm' -print 2>/dev/null | head -n 1 || true)"
  if [ -z "$FIRST_ELM" ]; then
    echo "warning: no .elm files in $DIR, skipping dependency pre-download" >&2
    return 0
  fi
  echo "Downloading dependencies with elm make for $FIXTURE_NAME..."
  if ! (cd "$DIR" && elm make "$FIRST_ELM" --output=/dev/null); then
    echo "warning: elm make failed for $FIXTURE_NAME, dependencies may not be cached" >&2
  fi
  rm -rf "$DIR/elm-stuff"
  return 0
}

total=0
skipped=0
succeeded=0
failed=0
attempted=0
FAILED_PACKAGES=()

mapfile -t PKG_LINES < <(jq -r '.[] | "\(.name) \(.version)"' "$JSON_FILE")

: > "$FAILURES_LOG"

for line in "${PKG_LINES[@]}"; do
  PKG="${line% *}"
  VERSION="${line##* }"
  NAME="${PKG//\//-}"

  total=$((total + 1))

  if [ -e "$TESTS_DIR/$NAME" ]; then
    echo "SKIP: $PKG ($VERSION) — $TESTS_DIR/$NAME already exists"
    skipped=$((skipped + 1))
    warm_elm_cache "$NAME"
    continue
  fi

  URL="https://github.com/$PKG.git"
  attempted=$((attempted + 1))

  echo "=== [$attempted] $PKG ($VERSION) -> $NAME ==="

  # Lenient per-package step: failure must not abort the loop.
  if "$ADD_FIXTURE" "$URL" "$NAME" "pass"; then
    echo "OK: $PKG"
    succeeded=$((succeeded + 1))
  else
    echo "FAILED: $PKG ($VERSION) — continuing with next package" >&2
    echo "$PKG $VERSION $URL" >> "$FAILURES_LOG"
    FAILED_PACKAGES+=("$PKG")
    failed=$((failed + 1))
  fi

  if [ "$SLEEP" != "0" ]; then
    sleep "$SLEEP"
  fi
done

echo ""
echo "Done. packages seen: $total, cloned ok: $succeeded, skipped (exists): $skipped, failed: $failed"
if [ "$failed" -gt 0 ]; then
  echo "Failed packages:"
  for p in "${FAILED_PACKAGES[@]}"; do
    echo "  - $p"
  done
  echo "Full list with versions/URLs: $FAILURES_LOG"
  exit 1
fi
