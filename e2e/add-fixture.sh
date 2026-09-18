#!/usr/bin/env bash
#
# Clone a git repo and turn it into an e2e fixture under e2e/tests/<name>.
#
# Usage:
#   ./e2e/add-fixture.sh <git-url> [name] [pass|fail]
#
# - <git-url>: repo to clone (shallow, depth 1)
# - [name]: fixture directory name under e2e/tests/ (default: repo basename, .git stripped)
# - [pass|fail]: value for expected.json's "expect" field (default: pass)
#
# Clones the latest published semver tag, not the default branch.
# Falls back to the default branch if the repo has no semver-looking tags.
#
# Only elm.json plus the module files reachable from its "source-directories"
# are copied into e2e/tests/<name>/project — non-Elm files (native shims,
# READMEs, other-language sources, etc.) are left behind.
#
# Also runs `elm make --output=/dev/null` in the new fixture project so the
# compiler downloads its deps into ~/.elm (best-effort, never fails fixture
# creation). Skip with SKIP_ELM_MAKE=1.

set -euo pipefail

if ! command -v jq >/dev/null 2>&1; then
  echo "error: jq is required but not found on PATH" >&2
  exit 1
fi

if [ $# -lt 1 ]; then
  echo "usage: $0 <git-url> [name] [pass|fail]" >&2
  exit 1
fi

URL="$1"
NAME="${2:-$(basename "$URL" .git)}"
EXPECT="${3:-pass}"

if [ "$EXPECT" != "pass" ] && [ "$EXPECT" != "fail" ]; then
  echo "error: expect must be 'pass' or 'fail', got '$EXPECT'" >&2
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FIXTURE_DIR="$SCRIPT_DIR/tests/$NAME"
PROJECT_DIR="$FIXTURE_DIR/project"

if [ -e "$FIXTURE_DIR" ]; then
  echo "error: $FIXTURE_DIR already exists" >&2
  exit 1
fi

CLONE_DIR="$(mktemp -d)"
trap 'rm -rf "$CLONE_DIR"' EXIT

# Clone the latest published (semver) tag instead of the default branch, so
# fixtures track released code rather than in-progress HEAD. Falls back to
# the default branch if the repo has no semver-looking tags.
LATEST_TAG="$(
  git ls-remote --tags --refs "$URL" 2>/dev/null \
    | awk '{print $2}' \
    | sed 's#^refs/tags/##' \
    | grep -E '^v?[0-9]+\.[0-9]+\.[0-9]+$' \
    | sed 's/^v//' \
    | sort -V \
    | tail -n 1 \
    || true
)"

if [ -n "$LATEST_TAG" ]; then
  # Re-derive the actual tag ref name (may have had a "v" prefix).
  TAG_REF="$(
    git ls-remote --tags --refs "$URL" 2>/dev/null \
      | awk '{print $2}' \
      | sed 's#^refs/tags/##' \
      | grep -E "^v?${LATEST_TAG}\$" \
      | head -n 1
  )"
  echo "Cloning $URL at tag $TAG_REF..."
  git clone --depth 1 --quiet --branch "$TAG_REF" "$URL" "$CLONE_DIR"
else
  echo "warning: no semver tags found for $URL, cloning default branch" >&2
  git clone --depth 1 --quiet "$URL" "$CLONE_DIR"
fi

ELM_JSON="$CLONE_DIR/elm.json"
if [ ! -f "$ELM_JSON" ]; then
  echo "error: no elm.json found at the root of $URL" >&2
  exit 1
fi

mkdir -p "$PROJECT_DIR"
cp "$ELM_JSON" "$PROJECT_DIR/elm.json"

mapfile -t SOURCE_DIRS < <(jq -r '(.["source-directories"] // ["src"])[]' "$ELM_JSON")

for SRC_DIR in "${SOURCE_DIRS[@]}"; do
  SRC_PATH="$CLONE_DIR/$SRC_DIR"
  if [ ! -d "$SRC_PATH" ]; then
    echo "warning: source-directory '$SRC_DIR' not found, skipping" >&2
    continue
  fi
  DEST_PATH="$PROJECT_DIR/$SRC_DIR"
  mkdir -p "$(dirname "$DEST_PATH")"
  # Copy only .elm files, preserving directory structure.
  (cd "$SRC_PATH" && find . -name "*.elm" -print) | while read -r ELM_FILE; do
    mkdir -p "$DEST_PATH/$(dirname "$ELM_FILE")"
    cp "$SRC_PATH/$ELM_FILE" "$DEST_PATH/$ELM_FILE"
  done
done

cat > "$FIXTURE_DIR/expected.json" <<EOF
{
  "expect": "$EXPECT",
  "note": "Cloned from $URL"
}
EOF

# Pre-download deps into ~/.elm via `elm make --output=/dev/null`.
if [ "${SKIP_ELM_MAKE:-0}" != "1" ]; then
  if ! command -v elm >/dev/null 2>&1; then
    echo "warning: elm not found on PATH, skipping dependency pre-download" >&2
  else
    FIRST_ELM="$(find "$PROJECT_DIR" -name '*.elm' -print | head -n 1 || true)"
    if [ -z "$FIRST_ELM" ]; then
      echo "warning: no .elm files found, skipping dependency pre-download" >&2
    else
      echo "Downloading dependencies with elm make..."
      if (cd "$PROJECT_DIR" && elm make "$FIRST_ELM" --output=/dev/null); then
        echo "Dependencies cached in ~/.elm"
      else
        echo "warning: elm make failed, dependencies may not be cached" >&2
      fi
      rm -rf "$PROJECT_DIR/elm-stuff"
    fi
  fi
fi

echo "Created fixture at $FIXTURE_DIR"
echo "Run it with: ./e2e/run.mjs $NAME"
