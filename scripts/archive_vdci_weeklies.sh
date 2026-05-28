#!/usr/bin/env bash
# archive_vdci_weeklies.sh
#
# Moves individual VDCI weekly files for 2020-2024 to 1_input/vdci/archive/
# then moves the all_mosq annual files into 1_input/vdci/.
#
# Run from the repo root:  bash scripts/archive_vdci_weeklies.sh

set -euo pipefail

VDCI="1_input/vdci"
ALL_MOSQ="1_input/all_mosq"
ARCHIVE="${VDCI}/archive"

mkdir -p "${ARCHIVE}"

echo "=== Archiving 2020-2024 VDCI weekly files ==="
moved=0
for f in "${VDCI}"/*.csv; do
  # Match any 4-digit year in range 2020-2024 anywhere in the filename
  if [[ "$f" =~ (2020|2021|2022|2023|2024) ]]; then
    mv -v "$f" "${ARCHIVE}/"
    ((moved++))
  fi
done
echo "Archived ${moved} weekly file(s)."

echo ""
echo "=== Moving all_mosq annual files into vdci/ ==="
moved_am=0
for f in "${ALL_MOSQ}"/*.csv; do
  mv -v "$f" "${VDCI}/"
  ((moved_am++))
done
echo "Moved ${moved_am} all_mosq file(s)."

echo ""
echo "=== Done. Current vdci/ contents ==="
ls "${VDCI}"/*.csv
