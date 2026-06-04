#!/usr/bin/env bash
set -euo pipefail

tracked_raw="$(
  git ls-files |
    grep -E '(^|/)(_data/raw|data-raw/raw)(/|$)|(^|/)clea_(lc_)?[0-9]{8}[^/]*[.](RData|pdf|xlsx)$' ||
    true
)"

if [[ -n "${tracked_raw}" ]]; then
  echo "Raw CLEA files or raw-data directories are tracked:"
  echo "${tracked_raw}"
  exit 1
fi

history_raw="$(
  git rev-list --objects --all |
    grep -E ' (_data/raw|data-raw/raw)(/|$)| clea_(lc_)?[0-9]{8}[^/]*[.](RData|pdf|xlsx)$' ||
    true
)"

if [[ -n "${history_raw}" ]]; then
  echo "Raw CLEA files remain in Git history:"
  echo "${history_raw}"
  exit 1
fi

echo "No raw CLEA files are tracked or present in Git history."
