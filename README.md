# TU9 OpenAlex Metrics

Open-access monitoring for the [TU9](https://www.tu9.de/) universities,
built from [OpenAlex](https://openalex.org/) and **snapshotted over time**. The
headline is the open-access share of each university's **corresponding-author**
output (`corresponding_institution_ids`) — the lens that matches OpenAPC and
transformative-agreement accounting. A few entity-level indicators (works,
citations, h-index) are kept as context. Every university is also shown
**consolidated** with its affiliated organisations the way the
[Leiden Ranking](https://open.leidenranking.com/) defines (e.g. with its
university hospital).

The pipeline and website mirror the setup of the sibling
[`slub/tu9-jct-data`](https://github.com/slub/tu9-jct-data) repository (R +
`renv` + Quarto, refreshed by GitHub Actions and deployed to GitHub Pages).

## Institutions tracked

The nine universities of the TU9 alliance — one OpenAlex institution each.
Affiliated organisations (university hospitals, research institutes) are **not**
tracked as separate institutions; where they belong to a university they enter
only through the Leiden-consolidated view (see below). Edit
[`data-raw/institutions.csv`](data-raw/institutions.csv) to change the set.

## What is stored

- `data/metrics.csv` — one row per university **per snapshot**: the
  corresponding-author OA headline (period + reference year) and the context
  indicators (works, citations, h-index, i10-index, 2-year mean citedness).
- `data/ca_oa_by_year.csv` — corresponding-author works and OA share by
  publication year; `data/ca_oa_status.csv` — OA-status split for the reference
  year; `data/consolidated_ca_oa_by_year.csv` — the Leiden-consolidated variant.
- `data/counts_by_year.csv` — works and citations by publication year (most
  recent snapshot).
- `data/<slug>/…` — the same views per university.
- `data/snapshots/<slug>/<date>.json` — the full raw OpenAlex entity, archived
  per snapshot.
- `data/meta.json` — summary counts and last-updated date for the site.

## Running locally

```bash
Rscript -e 'renv::restore()'      # once, to materialise the R library

# The OpenAlex premium API key is read from OPENALEX_API_KEY. Keep it in the
# git-ignored `secret` file locally (see below) and source it before a run:
source ./secret
export OPENALEX_MAILTO="you@example.org"   # polite pool (optional)

Rscript scripts/fetch.R           # fetch + append a snapshot
Rscript scripts/gen_pages.R       # regenerate per-institution pages
quarto render                     # build the site into _site/
```

`scripts/render.sh <page.qmd>` rebuilds individual pages without a full render.

## Credentials

The OpenAlex **premium API key** is optional (the pipeline works on the free API
via the polite pool) and is read from the `OPENALEX_API_KEY` environment
variable. It must **not** be committed:

- **Locally:** keep it in the git-ignored `secret` file
  (`export OPENALEX_API_KEY=...`) and `source ./secret` before running.
- **In CI:** store it as the `OPENALEX_API_KEY` GitHub Actions secret; the
  refresh workflow reads it from there.

## Automation

- `.github/workflows/refresh.yml` — weekly (Mondays 05:00 UTC): fetches a fresh
  snapshot, commits the new data, rebuilds and deploys the site. Can be run on
  demand.
- `.github/workflows/pages.yml` — rebuilds and deploys the site from data
  already in the repo whenever site code changes.

## Licence

Code: MIT (`LICENSE`). Data: CC0 1.0 (`data/LICENSE`), following OpenAlex.
