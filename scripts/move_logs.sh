#!/usr/bin/env bash
set -euo pipefail

# Script is in: <project>/scripts/
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"

LOGS_DIR="$ROOT_DIR/logs"
mkdir -p "$LOGS_DIR"

shopt -s nullglob
files=("$ROOT_DIR"/*_log)
shopt -u nullglob

if ((${#files[@]} == 0)); then
  echo "No *_log files found in project root: $ROOT_DIR"
  exit 0
fi

echo "Moving ${#files[@]} log file(s) to: $LOGS_DIR"
mv -v -- "${files[@]}" "$LOGS_DIR/"
