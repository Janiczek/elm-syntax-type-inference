#!/usr/bin/env bash

COLOR_OFF="\e[0m";
DIM="\e[2m";

LOCKNAME=$(cat /dev/urandom | LC_ALL=C tr -cd 'a-f0-9' | head -c 16);
LOCKFILE="/tmp/elm-lock-${LOCKNAME}"

function compile {
  find src -name '*.elm' | xargs elm make --optimize --docs docs.json --output /dev/null
}

function run {
  (
  flock 200

  clear;
  tput reset;

  echo -en "${DIM}";
  date -R;
  echo -en "${COLOR_OFF}";

  compile;

  ) 200>"${LOCKFILE}"
}

run;

chokidar '**/*.elm' | while read WHATEVER; do
  run;
done;
