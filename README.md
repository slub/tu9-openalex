# TU9 OpenAlex Metrics

Open-access monitoring for the [TU9](https://www.tu9.de/) universities,
built from [OpenAlex](https://openalex.org/) and **snapshotted over time**. The
headline is the open-access share of each university's **corresponding-author**
output (`corresponding_institution_ids`) — the lens that matches OpenAPC and
transformative-agreement accounting. A few entity-level indicators (works,
citations, h-index) are kept as context. Every university is shown three ways:
as the single OpenAlex institution, **consolidated** with its affiliated
organisations the way the [Leiden Ranking](https://open.leidenranking.com/)
defines (e.g. with its university hospital), and that same Leiden member set
restricted to CWTS Core sources via
`primary_location.source.is_core:true`.

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
  Works appear under three lenses — the institution alone excluding XPAC
  (`works_count`), the same including XPAC (`works_count_incl_xpac`, the entity's
  own figure), and additionally including its OpenAlex child institutions
  (`works_count_lineage_incl_xpac`, what openalex.org links to).
- `data/ca_oa_by_year.csv` — corresponding-author works and OA share by
  publication year; `data/ca_oa_status.csv` — OA-status split for the reference
  year; `data/consolidated_ca_oa_by_year.csv` — the Leiden-consolidated variant;
  `data/leiden_core_ca_oa_by_year.csv` — the Leiden-consolidated member set
  restricted to CWTS Core sources (`primary_location.source.is_core:true`).
- `data/counts_by_year.csv` — works and citations by publication year (most
  recent snapshot).
- `data/<slug>/…` — the same views per university.
- `data/snapshots/<slug>/<date>.json` — the full raw OpenAlex entity, archived
  per snapshot.
- `data/meta.json` — summary counts and last-updated date for the site.

## Running locally

```bash
Rscript -e 'renv::restore()'      # once, to materialise the R library

# The OpenAlex API key (free, optional) is read from OPENALEX_API_KEY. Keep it in
# git-ignored `secret` file locally (see below) and source it before a run:
source ./secret
export OPENALEX_MAILTO="you@example.org"   # polite pool (optional)

Rscript scripts/fetch.R           # fetch + append a snapshot
Rscript scripts/gen_pages.R       # regenerate per-institution pages
quarto render                     # build the site into _site/
```

`scripts/render.sh <page.qmd>` rebuilds individual pages without a full render.

## Credentials

The OpenAlex **API key** is optional and free. OpenAlex grants $0.10/day of
usage without a key and ten times that ($1/day) with a
[free key](https://developers.openalex.org/api-reference/authentication), which
covers 10,000 list+filter calls a day; single-entity lookups are not metered.

One refresh makes on the order of a hundred requests: one entity lookup per
university plus a handful of `group_by` calls each (works-by-year, lineage
works, corresponding-author OA denominator and numerator, DOAJ, OA-status
composition, and the same denominator/numerator pair again for the
Leiden-consolidated and the Core-source view). The count scales linearly with
the nine universities and the number of views, so adding a view moves it by
tens, not orders of magnitude — about 1 % of a single day's free-key
allowance, once a week. The free tier is ample; the paid plans are not
needed for this workload. To measure the current figure rather than estimate it,
count the calls through `curl::curl_fetch_memory`, which is the only place this
code touches the network.

The key is read from `OPENALEX_API_KEY` and must **not** be committed:

- **Locally:** keep it in the git-ignored `secret` file
  (`export OPENALEX_API_KEY=...`) and `source ./secret` before running.
- **In CI:** store it as the `OPENALEX_API_KEY` repository secret (a *secret*,
  not a variable); the refresh workflow reads it from there.

`OPENALEX_MAILTO` is the polite-pool contact address. It is configuration rather
than a credential, so in CI it is a repository **variable**; locally it falls
back to the address in `scripts/openalex.R`.

## Automation

- `.github/workflows/refresh.yml` — weekly (Mondays 05:00 UTC), and on demand
  from the Actions tab: fetches a fresh snapshot, renders the site and only then
  commits the new data, so a failed fetch, a failed validation or a broken
  render never reaches `main`. Most weeks are no-ops — OpenAlex itself updates
  roughly monthly, and the workflow skips the commit when nothing changed.
- `.github/workflows/pages.yml` — rebuilds and deploys the site from data
  already in the repo whenever site code changes.

## Licence

Code: MIT (`LICENSE`). Data: CC0 1.0 (`data/LICENSE`), following OpenAlex.
