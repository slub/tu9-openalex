#!/usr/bin/env Rscript
# Fetch OpenAlex metrics for the TU9 universities and write the data views:
#
#   data/snapshots/<slug>/<date>.json  raw institution entity dump (one per run)
#   data/metrics.csv                   dated time series, one row per (date, inst)
#   data/counts_by_year.csv            works/citations by publication year (latest)
#   data/ca_oa_by_year.csv             corresponding-author OA share by year (latest)
#   data/ca_oa_status.csv              CA OA-status split for the reference year (latest)
#   data/consolidated_ca_oa_by_year.csv  Leiden-consolidated CA OA share by year (latest)
#   data/hierarchy_ca_oa_by_year.csv     OpenAlex/ROR-hierarchy CA OA share by year (latest)
#   data/leiden_core_ca_oa_by_year.csv   Core-source (primary venue) CA OA share by year (latest)
#   data/leiden_core_any_location_ca_oa_by_year.csv  Core-source (any location) CA OA share by year (latest)
#   data/<slug>/*.csv                  the same views for one institution
#   data/meta.json                     summary + last-updated date (for the site)
#
# The run is FAIL-LOUD and publishes atomically in the sense that matters here:
# everything is fetched into memory, then validated (scripts/validate.R), and
# only if every check passes is anything written under data/. An incomplete or
# inconsistent snapshot aborts the run and leaves the published data untouched.
# In particular a metric is never substituted across definitions -- if the
# XPAC-excluded works query fails, that institution fails; the XPAC-inclusive
# entity count is not silently written into the XPAC-excluded column.
#
# Works counts EXCLUDE the OpenAlex Expansion Pack (XPAC): they come from the
# works API with is_xpac:false. The citation indicators (cited_by_count /
# h-index / i10 / 2yr-mean-citedness) are taken from the institution entity,
# which OpenAlex publishes only as an XPAC-inclusive aggregate -- so those
# figures include XPAC, and works_count_incl_xpac records their denominator.
#
# The OA figures are computed with OpenAlex `group_by` on the corresponding-
# author lens (`corresponding_institution_ids`); the `ca_` prefix marks every OA
# figure as corresponding-author-level. No individual works are paged through --
# only server-side aggregate counts.
#
# Re-running on the same day is idempotent: same-date rows and the day's JSON
# snapshots are overwritten, not duplicated.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(jsonlite)
})

source("scripts/openalex.R")
source("scripts/validate.R")
source("scripts/validate_products.R")

# --- configuration ---------------------------------------------------------
GUARD_THRESHOLD <- 0.20  # abort if the total works count drops by more than
                         # this vs. the previous snapshot (data glitch)
OA_START_YEAR   <- 2013  # earliest publication year for the OA-by-year view
PERIOD_START    <- 2020  # start of the headline CA period (end = REF_YEAR)

# Only an explicit affirmative enables the override; a stray FORCE=0 must not.
force <- tolower(trimws(Sys.getenv("FORCE"))) %in% c("1", "true", "yes", "y")

# The date is read once and everything downstream derives from it: the reference
# year, the year bound applied to every query, and the bound validation checks.
# There is deliberately no override. OpenAlex serves only its current state, so
# a backdated run would not reconstruct an earlier snapshot -- it would stamp
# today's data with a date it does not belong to, which is precisely what the
# time series must not contain.
snapshot_date <- format(Sys.Date())
SNAPSHOT_YEAR <- as.integer(format(as.Date(snapshot_date), "%Y"))
openalex_set_year_cap(SNAPSHOT_YEAR)

# Reference year for the oa_status composition: the latest complete calendar
# year (the current year is still filling up).
REF_YEAR <- SNAPSHOT_YEAR - 1L

# Sum corresponding-author works and OA works over the headline period
# [PERIOD_START, REF_YEAR] from a per-year OA table (year, ca_works, ca_oa_works).
period_ca <- function(oa) {
  p <- oa[oa$year >= PERIOD_START & oa$year <= REF_YEAR, , drop = FALSE]
  w <- sum(p$ca_works, na.rm = TRUE)
  o <- sum(p$ca_oa_works, na.rm = TRUE)
  list(works = as.integer(w), oa_works = as.integer(o),
       share = if (w > 0) round(o / w, 4) else NA_real_)
}

inst   <- read_institutions()
# The Leiden mapping defines both the consolidated and the Core member sets, so
# it is a hard requirement. Substituting an empty mapping would fail OPEN: every
# university would silently lose its components, the consolidated products would
# be dropped as "not applicable", and validation would agree -- because it
# derives its expectations from that same empty mapping.
leiden <- read_leiden_components(inst = inst)
if (is.null(leiden))
  stop("data-raw/leiden_affiliations.csv is missing or unreadable; refusing to ",
       "publish without the Leiden consolidation mapping.", call. = FALSE)
if (nrow(leiden) == 0)
  stop("data-raw/leiden_affiliations.csv yielded no weight-1 `component` rows; ",
       "the mapping is present but unusable (schema change?). Refusing to ",
       "publish a snapshot in which no university has components.", call. = FALSE)
message("Institutions to fetch: ", nrow(inst))

# ===========================================================================
# 1. FETCH -- nothing is written in this phase
# ===========================================================================
failures       <- character()
entities       <- list()
metric_rows    <- list()
cby_rows       <- list()
oa_year_rows   <- list()
oa_status_rows <- list()
cons_year_rows <- list()
cons_by_slug   <- list()
hier_year_rows <- list()
hier_by_slug   <- list()
core_year_rows <- list()
core_by_slug   <- list()
core_any_year_rows <- list()
core_any_by_slug   <- list()

fail <- function(slug, what) {
  failures <<- c(failures, sprintf("%s: %s", slug, what))
  message("    -> ", slug, " incomplete (", what, ")")
}

for (i in seq_len(nrow(inst))) {
  row <- inst[i, ]
  message("  ", i, "/", nrow(inst), "  ", row$slug, " (", row$openalex_bare, ")")

  obj <- openalex_institution_reader(row$openalex_bare)
  if (is.null(obj)) { fail(row$slug, "institution entity"); next }
  entities[[row$slug]] <- obj
  m <- openalex_metrics(obj)

  # XPAC-excluded works, total and per year. No fallback: if this fails the
  # institution fails, because the entity count has a different definition.
  wby <- openalex_works_by_year(row$openalex_bare)
  if (is.null(wby) || is.na(wby$total)) { fail(row$slug, "works-by-year (XPAC-excluded)"); next }

  # Reconciliation figure: the institution INCLUDING its OpenAlex lineage (child
  # institutions) and including XPAC -- the population openalex.org's own profile
  # links to. Treated as mandatory like every other product, so a degraded run
  # fails rather than publishing a snapshot with a hole in it.
  lby <- openalex_works_lineage_by_year(row$openalex_bare)
  if (is.null(lby) || is.na(lby$total)) { fail(row$slug, "lineage works"); next }
  lineage_total <- lby$total

  oa <- openalex_ca_oa_by_year(row$openalex_bare, OA_START_YEAR)
  if (is.null(oa) || nrow(oa) == 0) { fail(row$slug, "corresponding-author OA by year"); next }

  doaj <- openalex_ca_doaj_by_year(row$openalex_bare, OA_START_YEAR)
  if (is.null(doaj)) { fail(row$slug, "DOAJ works by year"); next }
  # Align DOAJ counts to the OA years now, so both the metric row and the
  # per-year rows below can use them. A year the DOAJ query does not return has
  # no DOAJ-listed works, i.e. 0.
  dj <- doaj$ca_doaj_works[match(oa$year, doaj$year)]
  dj[is.na(dj)] <- 0L

  st <- openalex_ca_oa_status(row$openalex_bare, REF_YEAR)
  if (is.null(st) || nrow(st) == 0) { fail(row$slug, "OA-status composition"); next }

  ref <- oa[oa$year == REF_YEAR, ]
  pa  <- period_ca(oa)

  metric_rows[[length(metric_rows) + 1L]] <- tibble(
    snapshot_date         = snapshot_date,
    slug                  = row$slug,
    name                  = row$name,
    openalex_id           = row$openalex_id,
    ror_id                = row$ror_id,
    works_count           = wby$total,      # XPAC-excluded (works API)
    works_count_incl_xpac = m$works_count,  # entity aggregate: same id, incl. XPAC
    works_count_lineage_incl_xpac = lineage_total,  # + child institutions (openalex.org)
    cited_by_count        = m$cited_by_count,
    h_index               = m$h_index,
    i10_index             = m$i10_index,
    two_yr_mean_citedness = m$two_yr_mean_cited,
    ref_year              = REF_YEAR,
    ca_works_ref          = if (nrow(ref)) ref$ca_works    else NA_integer_,
    ca_oa_works_ref       = if (nrow(ref)) ref$ca_oa_works else NA_integer_,
    ca_oa_share_ref       = if (nrow(ref)) ref$ca_oa_share else NA_real_,
    ca_doaj_works_ref     = { d <- dj[oa$year == REF_YEAR]; if (length(d)) as.integer(d) else NA_integer_ },
    ca_doaj_share_ref     = { d <- dj[oa$year == REF_YEAR]; w <- oa$ca_works[oa$year == REF_YEAR]
                              if (length(d) && length(w) && w > 0) round(d / w, 4) else NA_real_ },
    period_start          = PERIOD_START,
    period_end            = REF_YEAR,
    ca_works_period       = pa$works,
    ca_oa_works_period    = pa$oa_works,
    ca_oa_share_period    = pa$share
  )

  oa_year_rows[[length(oa_year_rows) + 1L]] <- tibble(
    snapshot_date = snapshot_date, slug = row$slug, year = oa$year,
    ca_works = oa$ca_works, ca_oa_works = oa$ca_oa_works, ca_oa_share = oa$ca_oa_share,
    ca_doaj_works = as.integer(dj),
    ca_doaj_share = ifelse(oa$ca_works > 0, round(dj / oa$ca_works, 4), NA_real_))

  oa_status_rows[[length(oa_status_rows) + 1L]] <- tibble(
    snapshot_date = snapshot_date, slug = row$slug, year = REF_YEAR,
    oa_status = st$oa_status, ca_works = st$ca_works)

  # counts_by_year: the entity supplies the year range and the citations; the
  # works column comes from the XPAC-excluded query. A year the entity reports
  # but the works query does not return has no non-XPAC works, i.e. 0 -- that is
  # a value within the same definition, not a substituted entity count.
  cby <- openalex_counts_by_year(obj)
  if (nrow(cby) > 0) {
    idx <- match(cby$year, wby$by_year$year)
    works_excl <- wby$by_year$works_count[idx]
    works_excl[is.na(works_excl)] <- 0L
    # Same treatment for the lineage lens; a year the query does not return has
    # no works, and the validator's ordering invariant catches anything absurd.
    works_lin <- lby$by_year$works_count[match(cby$year, lby$by_year$year)]
    works_lin[is.na(works_lin)] <- 0L
    cby_rows[[length(cby_rows) + 1L]] <- tibble(
      snapshot_date         = snapshot_date,
      slug                  = row$slug,
      year                  = cby$year,
      works_count           = as.integer(works_excl),   # id, XPAC-excluded
      works_count_incl_xpac = cby$works_count,          # id, incl. XPAC (entity)
      works_count_lineage_incl_xpac = as.integer(works_lin), # + lineage, incl. XPAC
      cited_by_count        = cby$cited_by_count        # entity (incl. XPAC)
    )
  }
}

# Leiden-consolidated CA-OA view: each university OR-ed with its weight-1
# `component` affiliates. Every university gets one, as in the core view below:
# where a university has no components the member set is the university alone,
# which is its correct consolidated value. Producing it keeps the alliance
# totals summable over the same nine universities in all five views -- omitting
# these rows made the consolidated total smaller than the single-institution
# total, which reads as if consolidating lost works.
for (i in seq_len(nrow(inst))) {
  slug <- inst$slug[i]
  u    <- inst[i, ]
  comp <- leiden$affiliated_openalex_id[leiden$tu9_slug == slug]
  members <- unique(c(openalex_bare(u$openalex_id), openalex_bare(comp)))
  message("  consolidated ", slug, " (", length(members), " members)")
  coa <- openalex_ca_oa_by_year(members, OA_START_YEAR)
  if (is.null(coa) || nrow(coa) == 0) { fail(slug, "consolidated OA by year"); next }
  cref <- coa[coa$year == REF_YEAR, ]
  cpa  <- period_ca(coa)
  cons_by_slug[[slug]] <- list(
    n_members          = length(members),
    members            = members,
    ca_works_ref       = if (nrow(cref)) cref$ca_works    else NA_integer_,
    ca_oa_share_ref    = if (nrow(cref)) cref$ca_oa_share else NA_real_,
    ca_works_period    = cpa$works,
    ca_oa_share_period = cpa$share)
  cons_year_rows[[length(cons_year_rows) + 1L]] <- tibble(
    snapshot_date = snapshot_date, tu9_slug = slug, university_name = u$name,
    n_members = length(members), year = coa$year, ca_works = coa$ca_works,
    ca_oa_works = coa$ca_oa_works, ca_oa_share = coa$ca_oa_share)
}

# Multi-institutional hierarchy (OpenAlex/ROR) CA-OA view: each university
# OR-ed with every institution in ITS OWN OpenAlex `lineage` -- not the curated
# Leiden mapping above, but OpenAlex/ROR's own automatically-derived hierarchy.
# The two member sets are independent and neither is a subset of the other, so
# this view is not expected to sit between the single-institution and
# consolidated figures -- only to be at least as large as the single-
# institution one. Every university gets a row, even with zero descendants
# (member set = the university alone), for the same comparability reason as
# consolidated: a future change in OpenAlex/ROR's hierarchy should not require
# a schema change to pick up.
for (i in seq_len(nrow(inst))) {
  slug <- inst$slug[i]
  u    <- inst[i, ]
  children <- openalex_hierarchy_children(u$openalex_bare)
  if (is.null(children)) { fail(slug, "hierarchy children"); next }
  members <- unique(c(openalex_bare(u$openalex_id), children))
  message("  hierarchy ", slug, " (", length(members), " members)")
  hoa <- openalex_ca_oa_by_year(members, OA_START_YEAR)
  if (is.null(hoa) || nrow(hoa) == 0) { fail(slug, "hierarchy OA by year"); next }
  href <- hoa[hoa$year == REF_YEAR, ]
  hpa  <- period_ca(hoa)
  hier_by_slug[[slug]] <- list(
    n_members          = length(members),
    members            = members,
    ca_works_ref       = if (nrow(href)) href$ca_works    else NA_integer_,
    ca_oa_share_ref    = if (nrow(href)) href$ca_oa_share else NA_real_,
    ca_works_period    = hpa$works,
    ca_oa_share_period = hpa$share)
  hier_year_rows[[length(hier_year_rows) + 1L]] <- tibble(
    snapshot_date = snapshot_date, tu9_slug = slug, university_name = u$name,
    n_members = length(members), year = hoa$year, ca_works = hoa$ca_works,
    ca_oa_works = hoa$ca_oa_works, ca_oa_share = hoa$ca_oa_share)
}

# CWTS Core-source-filtered CA-OA views: same member set as the consolidated
# view, but every university gets one (even without components). The CWTS Core
# allow-list is read two distinct ways, as two separate queries that are never
# combined:
#   primary venue -> primary_location.source.is_core:true (leiden_core_*)
#   any location  -> locations.source.is_core:true        (leiden_core_any_location_*)
# The member set is computed once here and passed to both, so the two readings
# and the consolidated view are guaranteed to be the same set of universities.
core_summary <- function(coa, members) {
  cref <- coa[coa$year == REF_YEAR, ]
  cpa  <- period_ca(coa)
  list(
    n_members          = length(members),
    members            = members,
    ca_works_ref       = if (nrow(cref)) cref$ca_works    else NA_integer_,
    ca_oa_share_ref    = if (nrow(cref)) cref$ca_oa_share else NA_real_,
    ca_works_period    = cpa$works,
    ca_oa_share_period = cpa$share)
}
core_year_row <- function(coa, slug, name, members) {
  tibble(
    snapshot_date = snapshot_date, tu9_slug = slug, university_name = name,
    n_members = length(members), year = coa$year, ca_works = coa$ca_works,
    ca_oa_works = coa$ca_oa_works, ca_oa_share = coa$ca_oa_share)
}
for (i in seq_len(nrow(inst))) {
  slug <- inst$slug[i]
  u    <- inst[i, ]
  comp <- leiden$affiliated_openalex_id[leiden$tu9_slug == slug]
  members <- unique(c(openalex_bare(u$openalex_id), openalex_bare(comp)))
  message("  core ", slug, " (", length(members), " members)")

  coa <- openalex_ca_oa_by_year_core(members, OA_START_YEAR)
  if (is.null(coa) || nrow(coa) == 0) { fail(slug, "core OA by year (primary venue)"); next }
  core_by_slug[[slug]] <- core_summary(coa, members)
  core_year_rows[[length(core_year_rows) + 1L]] <- core_year_row(coa, slug, u$name, members)

  aoa <- openalex_ca_oa_by_year_core_any_location(members, OA_START_YEAR)
  if (is.null(aoa) || nrow(aoa) == 0) { fail(slug, "core OA by year (any location)"); next }
  core_any_by_slug[[slug]] <- core_summary(aoa, members)
  core_any_year_rows[[length(core_any_year_rows) + 1L]] <- core_year_row(aoa, slug, u$name, members)
}

empty_if_none <- function(rows) if (length(rows) > 0) bind_rows(rows) else tibble()
snap <- list(
  metrics        = empty_if_none(metric_rows),
  counts_by_year = empty_if_none(cby_rows),
  ca_oa_by_year  = empty_if_none(oa_year_rows),
  ca_oa_status   = empty_if_none(oa_status_rows),
  consolidated   = empty_if_none(cons_year_rows),
  hierarchy      = empty_if_none(hier_year_rows),
  core           = empty_if_none(core_year_rows),
  core_any       = empty_if_none(core_any_year_rows),
  cons_members   = cons_by_slug,
  hier_members   = hier_by_slug,
  core_members   = core_by_slug,
  core_any_members = core_any_by_slug,
  entities       = entities,
  failures       = failures
)

# ===========================================================================
# 2. VALIDATE -- abort before touching data/ if anything is wrong
# ===========================================================================
metrics_path <- "data/metrics.csv"
prev_metrics <- if (file.exists(metrics_path)) {
  read_csv(metrics_path, col_types = cols(.default = col_character()))
} else NULL

validate_snapshot(snap, inst, leiden, snapshot_date,
                  prev_metrics = prev_metrics,
                  guard_threshold = GUARD_THRESHOLD, force = force,
                  period_start = PERIOD_START)

# ===========================================================================
# 3. WRITE -- only reached when the snapshot is complete and consistent
# ===========================================================================

# Raw JSON archive (written here, not during the fetch, so a failed run cannot
# leave a half-updated snapshot directory behind).
for (slug in names(snap$entities)) {
  d <- file.path("data", "snapshots", slug)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  write_json(snap$entities[[slug]], file.path(d, paste0(snapshot_date, ".json")),
             auto_unbox = TRUE, pretty = TRUE, null = "null")
}

# Snapshot-over-time series; same-date rows are replaced, not duplicated.
append_dedup <- function(path, new_df, sort_cols) {
  new_df <- mutate(new_df, across(everything(), as.character))
  combined <- new_df
  if (file.exists(path)) {
    old <- read_csv(path, col_types = cols(.default = col_character()))
    old <- old[old$snapshot_date != snapshot_date, ]
    # Only carry history forward if any remains; binding an empty old frame
    # would otherwise re-introduce its columns after a schema change. Align the
    # old rows to the current schema so renamed/removed columns are dropped.
    if (nrow(old) > 0) {
      for (col in setdiff(names(new_df), names(old))) old[[col]] <- NA_character_
      combined <- bind_rows(old[, names(new_df)], new_df)
    }
  }
  combined <- combined[do.call(order, combined[sort_cols]), ]
  write_csv(combined, path, na = "")
  combined
}
all_metrics <- append_dedup(metrics_path, snap$metrics, c("snapshot_date", "slug"))

# Publication-year views: latest snapshot only (the snapshot_date column and the
# raw JSON archive preserve provenance).
write_csv(snap$counts_by_year[order(snap$counts_by_year$slug,
                                    -snap$counts_by_year$year), ],
          "data/counts_by_year.csv", na = "")
write_csv(snap$ca_oa_by_year[order(snap$ca_oa_by_year$slug,
                                   -snap$ca_oa_by_year$year), ],
          "data/ca_oa_by_year.csv", na = "")
write_csv(snap$ca_oa_status[order(snap$ca_oa_status$slug,
                                  -snap$ca_oa_status$ca_works), ],
          "data/ca_oa_status.csv", na = "")
if (nrow(snap$consolidated) > 0) {
  write_csv(snap$consolidated[order(snap$consolidated$tu9_slug,
                                    -snap$consolidated$year), ],
            "data/consolidated_ca_oa_by_year.csv", na = "")
} else if (file.exists("data/consolidated_ca_oa_by_year.csv")) {
  unlink("data/consolidated_ca_oa_by_year.csv")
}
if (nrow(snap$hierarchy) > 0) {
  write_csv(snap$hierarchy[order(snap$hierarchy$tu9_slug,
                                 -snap$hierarchy$year), ],
            "data/hierarchy_ca_oa_by_year.csv", na = "")
} else if (file.exists("data/hierarchy_ca_oa_by_year.csv")) {
  unlink("data/hierarchy_ca_oa_by_year.csv")
}
if (nrow(snap$core) > 0) {
  write_csv(snap$core[order(snap$core$tu9_slug, -snap$core$year), ],
            "data/leiden_core_ca_oa_by_year.csv", na = "")
} else if (file.exists("data/leiden_core_ca_oa_by_year.csv")) {
  unlink("data/leiden_core_ca_oa_by_year.csv")
}
if (nrow(snap$core_any) > 0) {
  write_csv(snap$core_any[order(snap$core_any$tu9_slug, -snap$core_any$year), ],
            "data/leiden_core_any_location_ca_oa_by_year.csv", na = "")
} else if (file.exists("data/leiden_core_any_location_ca_oa_by_year.csv")) {
  unlink("data/leiden_core_any_location_ca_oa_by_year.csv")
}

# Per-institution views. Products that do not apply are removed rather than left
# behind, so a stale file can never be read as a current one.
for (slug in inst$slug) {
  d <- file.path("data", slug)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  im <- all_metrics[all_metrics$slug == slug, ]
  write_csv(im[order(im$snapshot_date), ], file.path(d, "metrics.csv"), na = "")
  ic <- snap$counts_by_year[snap$counts_by_year$slug == slug, ]
  write_csv(ic[order(-ic$year), ], file.path(d, "counts_by_year.csv"), na = "")
  io <- snap$ca_oa_by_year[snap$ca_oa_by_year$slug == slug, ]
  write_csv(io[order(-io$year), ], file.path(d, "ca_oa_by_year.csv"), na = "")
  is_ <- snap$ca_oa_status[snap$ca_oa_status$slug == slug, ]
  write_csv(is_[order(-is_$ca_works), ], file.path(d, "ca_oa_status.csv"), na = "")

  cpath <- file.path(d, "consolidated_ca_oa_by_year.csv")
  ico <- if (nrow(snap$consolidated) > 0)
    snap$consolidated[snap$consolidated$tu9_slug == slug, ] else snap$consolidated
  if (nrow(ico) > 0) {
    write_csv(ico[order(-ico$year), ], cpath, na = "")
  } else if (file.exists(cpath)) {
    unlink(cpath)
  }

  hpath <- file.path(d, "hierarchy_ca_oa_by_year.csv")
  ihier <- if (nrow(snap$hierarchy) > 0)
    snap$hierarchy[snap$hierarchy$tu9_slug == slug, ] else snap$hierarchy
  if (nrow(ihier) > 0) {
    write_csv(ihier[order(-ihier$year), ], hpath, na = "")
  } else if (file.exists(hpath)) {
    unlink(hpath)
  }

  corepath <- file.path(d, "leiden_core_ca_oa_by_year.csv")
  icore <- if (nrow(snap$core) > 0)
    snap$core[snap$core$tu9_slug == slug, ] else snap$core
  if (nrow(icore) > 0) {
    write_csv(icore[order(-icore$year), ], corepath, na = "")
  } else if (file.exists(corepath)) {
    unlink(corepath)
  }

  coreanypath <- file.path(d, "leiden_core_any_location_ca_oa_by_year.csv")
  icoreany <- if (nrow(snap$core_any) > 0)
    snap$core_any[snap$core_any$tu9_slug == slug, ] else snap$core_any
  if (nrow(icoreany) > 0) {
    write_csv(icoreany[order(-icoreany$year), ], coreanypath, na = "")
  } else if (file.exists(coreanypath)) {
    unlink(coreanypath)
  }
}

# Drop data of institutions that are no longer configured.
for (d in setdiff(setdiff(list.dirs("data", recursive = FALSE, full.names = FALSE),
                          "snapshots"), inst$slug)) {
  unlink(file.path("data", d), recursive = TRUE)
  message("Removed data of unconfigured institution: ", d)
}
for (d in setdiff(list.dirs(file.path("data", "snapshots"), recursive = FALSE,
                            full.names = FALSE), inst$slug)) {
  if (nzchar(d)) unlink(file.path("data", "snapshots", d), recursive = TRUE)
}

# --- meta.json -------------------------------------------------------------
# Merge one view's per-slug summary (n_members, ca_works_ref, ca_oa_share_ref,
# ca_works_period, ca_oa_share_period) into `entry` under `<prefix>_*` names.
# One implementation shared by every extra view (consolidated, hierarchy, both
# Core readings) instead of four copies that could individually drift.
with_view_summary <- function(entry, prefix, summary) {
  if (is.null(summary)) return(entry)
  entry[[paste0(prefix, "_n_members")]]          <- summary$n_members
  entry[[paste0(prefix, "_ca_works_ref")]]       <- summary$ca_works_ref
  entry[[paste0(prefix, "_ca_oa_share_ref")]]    <- summary$ca_oa_share_ref
  entry[[paste0(prefix, "_ca_works_period")]]    <- summary$ca_works_period
  entry[[paste0(prefix, "_ca_oa_share_period")]] <- summary$ca_oa_share_period
  entry
}

latest <- snap$metrics
meta_inst <- lapply(seq_len(nrow(latest)), function(i) {
  r    <- latest[i, ]
  hist <- all_metrics[all_metrics$slug == r$slug, ]
  entry <- list(
    name           = r$name,
    slug           = r$slug,
    openalex_id    = r$openalex_id,
    ror_id         = r$ror_id,
    works_count    = r$works_count,
    cited_by_count = r$cited_by_count,
    h_index        = r$h_index,
    ca_works_ref   = r$ca_works_ref,
    ca_oa_share_ref = r$ca_oa_share_ref,
    ca_works_period    = r$ca_works_period,
    ca_oa_share_period = r$ca_oa_share_period,
    first_snapshot  = min(hist$snapshot_date),
    latest_snapshot = max(hist$snapshot_date)
  )
  entry <- with_view_summary(entry, "cons",     cons_by_slug[[r$slug]])
  entry <- with_view_summary(entry, "hier",     hier_by_slug[[r$slug]])
  entry <- with_view_summary(entry, "core",     core_by_slug[[r$slug]])
  entry <- with_view_summary(entry, "core_any", core_any_by_slug[[r$slug]])
  entry
})

comp_ids   <- openalex_bare(leiden$affiliated_openalex_id)
n_entities <- length(unique(c(openalex_bare(latest$openalex_id), comp_ids)))

meta <- list(
  updated         = snapshot_date,
  source          = "OpenAlex (corresponding-author works, CC0)",
  n_institutions  = nrow(latest),
  n_entities      = n_entities,
  n_snapshots     = length(unique(all_metrics$snapshot_date)),
  oa_ref_year     = REF_YEAR,
  oa_period_start = PERIOD_START,
  oa_period_end   = REF_YEAR,
  institutions    = meta_inst
)
write_json(meta, "data/meta.json", auto_unbox = TRUE, pretty = TRUE, null = "null")

# ===========================================================================
# 4. RE-VALIDATE -- what was actually written, as a reader will see it
# ===========================================================================
# Everything above this line was checked in memory. The write itself, the
# per-institution slices, meta.json and the raw archive are produced afterwards,
# so a defect there used to reach the site unopposed. This cannot un-write the
# files -- but the workflow commits only after validation AND a successful
# render, so a failure here still keeps bad data out of main and off the site.
validate_products(inst = inst)

message("Wrote metrics for ", nrow(snap$metrics), " institutions (snapshot ",
        snapshot_date, "); ", length(unique(all_metrics$snapshot_date)),
        " snapshot(s) on record.")
