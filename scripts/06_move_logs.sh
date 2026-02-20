#!/usr/bin/env bash
set -euo pipefail

echo 'Moving log files...'

shopt -s nullglob
logs=( *_log )

if [ ${#logs[@]} -eq 0 ]; then
  echo 'No *_log files found in root.'
  exit 0
fi

date_dir="output/logs/$(date +%F)"
mkdir -p "$date_dir"

for f in "${logs[@]}"; do
  mv "$f" "$date_dir/"
done

echo "Moved ${#logs[@]} log files to $date_dir"
