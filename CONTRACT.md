# Data and Publication Contract

This document defines the guarantees that the TU9 Open Access Monitoring pipeline must preserve.
It is the starting point for changes to fetching, validation, stored products, metadata, rendering, and publication.
The R scripts implement the contract; the README and website explain the project to users.

## Purpose and scope

The project monitors open access for the configured TU9 universities using OpenAlex and stores snapshots over time.
Its headline is the open-access share of works whose corresponding author is affiliated with a university, using OpenAlex `corresponding_institution_ids`.
Each university is represented through five views:

1. the single configured OpenAlex institution;
2. that institution grouped with every institution in its own OpenAlex `lineage` (its full descendant tree), fetched live via an institutions search — OpenAlex/ROR's own automatically-derived hierarchy, independent of the Leiden mapping below;
3. that institution consolidated with the weight-1 `component` affiliates in the Leiden mapping;
4. the same consolidated member set restricted to works with any CWTS Core-source location, using `locations.source.is_core:true`;
5. the same consolidated member set restricted to works whose primary venue is a CWTS Core source, using `primary_location.source.is_core:true`.

Views 4 and 5 are two distinct readings of the same allow-list, computed as separate queries and never combined. They nest: view 5 ⊆ view 4 ⊆ view 3, for both the works count and its open-access subset.

View 2 (hierarchy) and view 3 (consolidated) are independent member sets computed from different sources and do not nest: neither is a subset of the other, and empirically the hierarchy figure exceeds the consolidated one for some TU9 universities and falls short for others. Both are guaranteed only to be at least as large as view 1 (single), checked separately in `scripts/validate.R`; no ordering between views 2 and 3 is asserted or should be.

Beside those nine per-institution views, the pipeline publishes one alliance-level reading per view: a single deduplicated OpenAlex query OR-ing every configured university's relevant member ids together, so a work whose corresponding author matches more than one member institution is counted once. Let *S* be the sum of the nine institutional CA-work counts already published for a view, *U* the distinct CA works that view's deduplicated query returns, and *U*<sub>OA</sub> its open-access subset. The landing page presents *U* ("distinct CA works") and *U*<sub>OA</sub>/*U* ("CA OA share", a direct ratio, not an average of the nine institutional shares).

`meta.json` additionally carries *S*, *S*<sub>OA</sub> (the OA-work counterpart), and (*S* − *U*)/*S* ("overlap_share", the share of *S* removed by deduplication) for every view. These are computed and validated with the same rigor as the displayed figures — see the validation-layer bullets below — but the site does not present overlap as a headline metric. It remains available as a validated deduplication diagnostic in `meta.json` and is derivable from `data/alliance_ca_oa_by_year.csv` together with the nine per-institution products. It must not be interpreted as a cooperation rate: a work can match more than one member either because a single corresponding author carries multiple affiliations or because multiple corresponding authors are affiliated with different member institutions; the aggregate count cannot distinguish between those cases.

The project is a prototype, but it publishes committed data through a public static website.
The appropriate standard is therefore strong validation at the commit and deployment boundaries, not independent proof of every upstream value or production-grade operational resilience.

## Core guarantee

> A refresh must not commit or deploy incomplete, structurally invalid, or internally contradictory data, and the website must accurately represent the committed products.

A guard belongs in the current contract when it protects a public claim or committed product, catches a realistic API, configuration, assembly, or writing failure, behaves deterministically, and has a reasonable maintenance cost.

## Authoritative sources

| Source | Authority |
|---|---|
| `data-raw/institutions.csv` | Configured institution set, names, slugs, OpenAlex IDs, and ROR IDs |
| `data-raw/leiden_affiliations.csv` | Consolidated member assignments after joint validation against the institution configuration |
| The run's snapshot date | Snapshot year, reference year, maximum publication year, and period end |
| Archived OpenAlex institution entities | Entity ID, XPAC-inclusive works, citations, h-index, i10-index, 2-year mean citedness, and their reconstructible yearly entity counts |
| OpenAlex institutions search (`lineage:<id>`) | The hierarchy view's member set for the current fetch only — not archived, so not independently reconstructible after the fact |
| OpenAlex grouped works responses | XPAC-excluded, lineage, corresponding-author, OA, DOAJ, status, hierarchy, consolidated, and both Core-source (primary-venue and any-location) figures for the current fetch |
| Global files under `data/` | Canonical written products from which per-institution exports are sliced |
| `data/metrics.csv` | Snapshot history and the source for reconstructible historical metadata such as first/latest snapshot |

Archived integer entity metrics are compared exactly.
The archived JSON is a lossy representation of floating-point values, so 2-year mean citedness is compared using the validator's documented tolerance rather than exact equality.

Grouped works responses are not archived.
Their published values can be checked for completeness, arithmetic, ordering, coverage, and agreement with other products, but cannot later be independently reconstructed from files in the repository.

## Published products

| Product | Key | Retention |
|---|---|---|
| `data/metrics.csv` | `(slug, snapshot_date)` | Full snapshot history |
| `data/counts_by_year.csv` | `(slug, year)` | Latest snapshot only |
| `data/ca_oa_by_year.csv` | `(slug, year)` | Latest snapshot only |
| `data/ca_oa_status.csv` | `(slug, year, oa_status)` | Latest snapshot only |
| `data/hierarchy_ca_oa_by_year.csv` | `(tu9_slug, year)` | Latest snapshot only |
| `data/consolidated_ca_oa_by_year.csv` | `(tu9_slug, year)` | Latest snapshot only |
| `data/leiden_core_ca_oa_by_year.csv` | `(tu9_slug, year)` | Latest snapshot only |
| `data/leiden_core_any_location_ca_oa_by_year.csv` | `(tu9_slug, year)` | Latest snapshot only |
| `data/alliance_ca_oa_by_year.csv` | `(view, year)` | Latest snapshot only, alliance-level only (no per-institution slice) |
| `data/meta.json` | One site summary plus one entry per institution, plus one alliance summary per view | Latest snapshot plus history-derived summary fields |
| `data/snapshots/<slug>/<date>.json` | `(slug, snapshot date)` | Raw institution entity archive |

Every CSV product is mandatory for every configured institution, with one exception: `data/alliance_ca_oa_by_year.csv` is an alliance-level product only, keyed by `view` rather than by institution, and intentionally has no per-institution slice under `data/<slug>/` — a deduplicated alliance-wide count has no single institution to slice it by.
Every other CSV product's files under `data/<slug>/` must have the same columns and rows as that institution's slice of the corresponding global file.

## Validation layers

### 1. Configuration

`read_institutions()` treats `data-raw/institutions.csv` as the root of trust.
It requires the expected columns, non-empty values, unique and path-safe slugs, unique OpenAlex and ROR IDs, and valid identifier formats.

`read_leiden_components()` validates the Leiden mapping jointly with the institution configuration.
Component rows must name a configured institution, carry the required identities, have weight 1 and a usable OpenAlex ID, agree with the configured university identity, be unique within the university, and not repeat the university itself.

Any fetch, validation, site, or publication path that interprets these inputs must use the shared readers rather than weaker inline parsing.

### 2. Run clock

`scripts/fetch.R` reads the snapshot date once per run.
The reference year is the snapshot year minus one, and the maximum publication year and period end are derived from that same clock.
Validation of archived yearly counts must be anchored to the snapshot under validation, never to the date on which validation happens to run.

### 3. In-memory snapshot

`validate_snapshot()` runs after all fetches have completed and before `data/` is written.
It requires:

- no recorded fetch failures;
- the exact configured institution set;
- every mandatory product and required column;
- unique composite keys and one consistent snapshot date;
- required reference-year rows and bounded, plausible publication years;
- finite, non-negative required numeric values;
- numerator, denominator, and share arithmetic for OA and DOAJ values;
- reference-year and period headlines that agree with their yearly rows;
- consolidated and both Core member sets that agree with the validated mapping; the hierarchy member set checked for self-consistency (own id included, `n_members` matches what was fetched) since there is no external mapping to check it against;
- valid lens ordering, including consolidated counts and hierarchy counts each independently not falling below the single-institution view (but not against each other), and the nested Core subset invariants (primary-venue Core ≤ any-location Core ≤ consolidated) on both the works count and its open-access subset;
- reconstructible entity metrics that agree with the archived entity;
- protection against an unexpectedly large drop from the previous snapshot;
- the alliance-level product: exactly the five expected views with stable identifiers, unique `(view, year)` keys, required year coverage and bounds, finite non-negative counts, `ca_oa_works ≤ ca_works` and share arithmetic per row, period figures that sum the yearly rows (not an average), *S* that agrees with the sum of the nine institutional rows it is derived from, *U* and *U*<sub>OA</sub> that do not exceed *S* and its OA counterpart, an alliance union no smaller than its largest single member institution, overlap equal to `(S − U) / S` and bounded to `[0, 1]` when defined, single ≤ hierarchy and single ≤ consolidated, and the Core nesting (primary ≤ any-location ≤ consolidated) on both works and OA works — with no ordering asserted between hierarchy and consolidated, mirroring the per-institution rule above.

`FORCE=1` may bypass only the large-drop guard rail.
It must not bypass completeness, schema, identity, arithmetic, membership, or date validation.

### 4. Written products

After writing, `validate_products()` reads the repository products back as a consumer would.
It requires:

- every mandatory global and per-institution file to exist and be non-empty;
- latest-only products to carry exactly `meta.updated`;
- exact institution coverage and no unconfigured data directories;
- every per-institution export to equal its global slice;
- `meta.json` to agree with the current CSV rows, configuration, mapping, and metrics history, including its alliance summary against the written `alliance_ca_oa_by_year.csv` and the nine institutional products each view's *S* is summed from;
- the alliance product to exist, be non-empty, current, and cover exactly the five expected views, and to have no per-institution slice under any `data/<slug>/`;
- the current raw entity for every institution to exist, parse, carry the expected ID, and support reconstructible fields;
- the current written products to pass `validate_snapshot()` again;
- all historical `metrics.csv` rows to have valid keys, dates, coverage, identities, required numerics, ordering, and row-level arithmetic.

Current and historical metrics rows share one required-numeric field list and one arithmetic implementation in `scripts/validate.R`.

### 5. Rendering and publication

`scripts/gen_pages.R` generates the configured institution pages and removes pages for institutions no longer configured.
Site readers fail on missing, empty, or stale current products.

Quarto runs:

- `scripts/gen_variables.R` before rendering to derive the footer update date from `meta.json`;
- `scripts/publish_data.R` after rendering to copy all required products, configuration files, licences, per-institution exports, and raw snapshots into `_site`.

Missing required assets are errors rather than reasons to publish a partial site.

## CI execution paths

### Weekly or manual refresh

`.github/workflows/refresh.yml` performs:

1. `Rscript scripts/fetch.R`;
2. `Rscript scripts/gen_pages.R`;
3. `quarto render`, including the pre-render and post-render hooks;
4. commit of changes under `data/` only after all previous steps succeed;
5. upload and deployment of `_site`.

`fetch.R` sources `openalex.R`, `validate.R`, and `validate_products.R`.
It validates once before writing and once after writing.

### Site-code push or manual deployment

`.github/workflows/pages.yml` does not fetch from OpenAlex.
It performs:

1. `Rscript scripts/validate_products.R` against the committed data;
2. `Rscript scripts/gen_pages.R`;
3. `quarto render`, including the same hooks;
4. upload and deployment of `_site`.

The two workflows share the `publish` concurrency group and must not deploy concurrently.

## Changing the schema

Schema changes are changes to the published contract, not only to a tibble or JSON writer.

Before adding, renaming, or removing a field, decide:

1. which source is authoritative for it;
2. whether it is required for all rows or may legitimately be absent;
3. whether it applies to the full history or only snapshots from an introduction date;
4. whether historical values can be reconstructed from archived entities or other committed sources;
5. whether it is displayed, copied into `meta.json`, or offered only as a download.

For a field change, review and update as applicable:

- the OpenAlex reader or derivation in `scripts/openalex.R` or `scripts/fetch.R`;
- the product row assembled in `scripts/fetch.R`;
- the required schema, numeric contract, and relationships in `scripts/validate.R`;
- written-history and metadata checks in `scripts/validate_products.R`;
- `meta.json` construction in `scripts/fetch.R`;
- explicit consumers in `scripts/site_helpers.R` and the QMD pages;
- the README, `background.qmd`, `downloads.qmd`, and this contract.

`append_dedup()` aligns old `metrics.csv` rows to the current schema when a refresh is written:

- adding a column fills historical rows with `NA` until they are deliberately backfilled;
- removing a column drops it from carried-forward history;
- renaming is equivalent to removing the old column and adding a new one unless an explicit migration preserves the values.

Because required numeric history may not contain `NA`, a new required numeric field needs an explicit history policy before the next refresh.
Choose one of the following deliberately:

- backfill it from committed raw entities or another authoritative source;
- permit absence before a documented introduction date;
- begin a new history series;
- deliberately reset the existing history.

Do not invent historical grouped-response values when the underlying responses were not archived.

Adding or removing an entire product also requires coordinated changes to:

- the in-memory snapshot and writer in `scripts/fetch.R`;
- global and per-institution product definitions in `scripts/validate_products.R`;
- schemas and semantic checks in `scripts/validate.R`;
- the `PRODUCTS` list in `scripts/publish_data.R`;
- site consumers and download documentation.

## Accepted limitations

The current contract deliberately accepts that:

- internally consistent but incorrect values returned by OpenAlex may pass;
- unarchived grouped responses cannot be independently reconstructed later, including the alliance-level deduplicated queries: `data/alliance_ca_oa_by_year.csv` is latest-snapshot only, with no historical series and no invented or backfilled prior values;
- the hierarchy view's exact member-ID list is fetched live and not archived anywhere, unlike the Leiden mapping's committed CSV, so it cannot be independently reconstructed after the fact either — only its count (`n_members`) is published and cross-checked for self-consistency;
- the snapshot date and `period_start` are policy inputs without a third on-disk authority;
- a failure after writing can leave a local checkout's `data/` dirty, although CI will not commit or deploy it;
- a refresh push can fail if `main` moved because the workflow does not rebase;
- the adversarial fixtures used during review were temporary scratchpad checks and are not stored in the repository;
- there is currently no permanent regression suite reproducing those fixtures.

These are known prototype boundaries, not permission to weaken the commit and deployment guarantees above.

## Required verification

For changes affecting the contract or its implementation, run as applicable:

```bash
Rscript --vanilla scripts/validate_products.R
Rscript --vanilla -e 'for (f in list.files("scripts", pattern = "\\.R$", full.names = TRUE)) parse(f)'
Rscript --vanilla -e 'renv::status()'
python3 -m py_compile scripts/leiden_affiliations.py
Rscript --vanilla scripts/gen_pages.R
quarto render
git diff --check
git status --short
```

Confirm that `data/` is unchanged unless the task explicitly includes a data refresh or migration.
Do not perform a live OpenAlex fetch unless the task explicitly requires it.

An implementation report should state which contract changed, what was verified, whether data changed, and which accepted limitations remain relevant.
Do not claim that no defects remain or broaden a bounded task into general production hardening.
