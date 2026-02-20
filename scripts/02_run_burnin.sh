#!/usr/bin/env bash
set -euo pipefail

if [[ ! -x './nemoage' ]]; then
  echo 'ERROR: NEMO_BIN not found/executable. Create a symlink in project root, e.g.:'
  echo '  ln -sf /path/to/nemoage_binary ./nemoage && chmod +x ./nemoage'
  exit 1
fi

'./nemoage' 'input/ini_files/burnin/burnin_k30_b0.03.ini'

echo 'Burn-in finished.'
