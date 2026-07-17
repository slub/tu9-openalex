#!/usr/bin/env bash
# Render individual Quarto pages into _site/ without rebuilding the whole site.
#
# Usage:
#   scripts/render.sh background.qmd              # one page
#   scripts/render.sh index.qmd downloads.qmd     # several pages
#   scripts/render.sh                             # all changed .qmd (git)
#
# Each page renders just like a normal build (R chunks execute, the
# post-render data-publish step runs), but only for the pages you name.
#
# Note: pages are rendered one at a time on purpose. `quarto render a b c`
# treats only the first argument as input and the rest as pandoc options, so
# we loop instead. For a live-reloading dev loop, use `quarto preview <page>`.
set -euo pipefail

cd "$(dirname "$0")/.."

pages=("$@")
if [ ${#pages[@]} -eq 0 ]; then
  # No args: render the .qmd files changed in the working tree (modified +
  # untracked), so editing and re-rendering is a single command.
  mapfile -t pages < <(git status --porcelain -- '*.qmd' | sed 's/^...//')
  if [ ${#pages[@]} -eq 0 ]; then
    echo "No changed .qmd files. Pass page paths to render them explicitly." >&2
    exit 0
  fi
  echo "Rendering changed pages: ${pages[*]}"
fi

for page in "${pages[@]}"; do
  echo "→ rendering $page"
  quarto render "$page"
done
