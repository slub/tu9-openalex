#!/usr/bin/env Rscript
# Fetch OpenAlex metrics for the TU9 universities and write the data views:
#
#   data/snapshots/<slug>/<date>.json  raw institution entity dump (one per run)
#   data/metrics.csv                   dated time series, one row per (date, inst)
#   data/counts_by_year.csv            works/citations by publication year (latest)
#   data/ca_oa_by_year.csv             corresponding-author OA share by year (latest)
#   data/ca_oa_status.csv              CA OA-status split for the reference year (latest)
#   data/consolidated_ca_oa_by_year.csv  Leiden-consolidated CA OA share by year (latest)
#   data/leiden_core_ca_oa_by_year.csv   Core-source-filtered CA OA share by year (latest)
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

# --- configuration ---------------------------------------------------------
GUARD_THRESHOLD <- 0.20  # abort if the total works count drops by more than
                         # this vs. the previous snapshot (data glitch)
OA_START_YEAR   <- 2013  # earliest publication year for the OA-by-year view
PERIOD_START    <- 2020  # start of the headline CA period (end = REF_YEAR)

# Only an explicit affirmative enables the override; a stray FORCE=0 must not.
force <- tolower(trimws(Sys.getenv("FORCE"))) %in% c("1", "true", "yes", "y")

snapshot_date <- Sys.getenv("SNAPSHOT_DATE")
if (!nzchar(snapshot_date)) snapshot_date <- format(Sys.Date())

# Reference year for the oa_status composition: the latest complete calendar
# year (the current year is still filling up).
REF_YEAR <- as.integer(format(as.Date(snapshot_date), "%Y")) - 1L

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
leiden <- tryCatch(read_leiden_components(), error = function(e) NULL)
if (is.null(leiden)) leiden <- data.frame(tu9_slug = character(),
                                          affiliated_openalex_id = character())
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
core_year_rows <- list()
core_by_slug   <- list()

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

  oa <- openalex_ca_oa_by_year(row$openalex_bare, OA_START_YEAR)
  if (is.null(oa) || nrow(oa) == 0) { fail(row$slug, "corresponding-author OA by year"); next }

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
    works_count_incl_xpac = m$works_count,  # entity aggregate (incl. XPAC)
    cited_by_count        = m$cited_by_count,
    h_index               = m$h_index,
    i10_index             = m$i10_index,
    two_yr_mean_citedness = m$two_yr_mean_cited,
    ref_year              = REF_YEAR,
    ca_works_ref          = if (nrow(ref)) ref$ca_works    else NA_integer_,
    ca_oa_works_ref       = if (nrow(ref)) ref$ca_oa_works else NA_integer_,
    ca_oa_share_ref       = if (nrow(ref)) ref$ca_oa_share else NA_real_,
    period_start          = PERIOD_START,
    period_end            = REF_YEAR,
    ca_works_period       = pa$works,
    ca_oa_works_period    = pa$oa_works,
    ca_oa_share_period    = pa$share
  )

  oa_year_rows[[length(oa_year_rows) + 1L]] <- tibble(
    snapshot_date = snapshot_date, slug = row$slug, year = oa$year,
    ca_works = oa$ca_works, ca_oa_works = oa$ca_oa_works, ca_oa_share = oa$ca_oa_share)

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
    cby_rows[[length(cby_rows) + 1L]] <- tibble(
      snapshot_date         = snapshot_date,
      slug                  = row$slug,
      year                  = cby$year,
      works_count           = as.integer(works_excl), # XPAC-excluded
      works_count_incl_xpac = cby$works_count,        # entity (incl. XPAC)
      cited_by_count        = cby$cited_by_count      # entity (incl. XPAC)
    )
  }
}

# Leiden-consolidated CA-OA view: each university OR-ed with its weight-1
# `component` affiliates. Only universities that have components get one.
unis_with_components <- intersect(inst$slug, unique(leiden$tu9_slug))
for (slug in unis_with_components) {
  u <- inst[inst$slug == slug, ][1, ]
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

# CWTS Core-source-filtered CA-OA view: same member set as the consolidated
# view, but every university gets one (even without components), and works are
# additionally restricted to primary_location.source.is_core:true.
for (i in seq_len(nrow(inst))) {
  slug <- inst$slug[i]
  u    <- inst[i, ]
  comp <- leiden$affiliated_openalex_id[leiden$tu9_slug == slug]
  members <- unique(c(openalex_bare(u$openalex_id), openalex_bare(comp)))
  message("  core ", slug, " (", length(members), " members)")
  coa <- openalex_ca_oa_by_year_core(members, OA_START_YEAR)
  if (is.null(coa) || nrow(coa) == 0) { fail(slug, "core OA by year"); next }
  cref <- coa[coa$year == REF_YEAR, ]
  cpa  <- period_ca(coa)
  core_by_slug[[slug]] <- list(
    n_members          = length(members),
    members            = members,
    ca_works_ref       = if (nrow(cref)) cref$ca_works    else NA_integer_,
    ca_oa_share_ref    = if (nrow(cref)) cref$ca_oa_share else NA_real_,
    ca_works_period    = cpa$works,
    ca_oa_share_period = cpa$share)
  core_year_rows[[length(core_year_rows) + 1L]] <- tibble(
    snapshot_date = snapshot_date, tu9_slug = slug, university_name = u$name,
    n_members = length(members), year = coa$year, ca_works = coa$ca_works,
    ca_oa_works = coa$ca_oa_works, ca_oa_share = coa$ca_oa_share)
}

empty_if_none <- function(rows) if (length(rows) > 0) bind_rows(rows) else tibble()
snap <- list(
  metrics        = empty_if_none(metric_rows),
  counts_by_year = empty_if_none(cby_rows),
  ca_oa_by_year  = empty_if_none(oa_year_rows),
  ca_oa_status   = empty_if_none(oa_status_rows),
  consolidated   = empty_if_none(cons_year_rows),
  core           = empty_if_none(core_year_rows),
  cons_members   = cons_by_slug,
  core_members   = core_by_slug,
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
if (nrow(snap$core) > 0) {
  write_csv(snap$core[order(snap$core$tu9_slug, -snap$core$year), ],
            "data/leiden_core_ca_oa_by_year.csv", na = "")
} else if (file.exists("data/leiden_core_ca_oa_by_year.csv")) {
  unlink("data/leiden_core_ca_oa_by_year.csv")
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

  corepath <- file.path(d, "leiden_core_ca_oa_by_year.csv")
  icore <- if (nrow(snap$core) > 0)
    snap$core[snap$core$tu9_slug == slug, ] else snap$core
  if (nrow(icore) > 0) {
    write_csv(icore[order(-icore$year), ], corepath, na = "")
  } else if (file.exists(corepath)) {
    unlink(corepath)
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
  cons <- cons_by_slug[[r$slug]]
  if (!is.null(cons)) {
    entry$cons_n_members          <- cons$n_members
    entry$cons_ca_works_ref       <- cons$ca_works_ref
    entry$cons_ca_oa_share_ref    <- cons$ca_oa_share_ref
    entry$cons_ca_works_period    <- cons$ca_works_period
    entry$cons_ca_oa_share_period <- cons$ca_oa_share_period
  }
  core <- core_by_slug[[r$slug]]
  if (!is.null(core)) {
    entry$core_n_members          <- core$n_members
    entry$core_ca_works_ref       <- core$ca_works_ref
    entry$core_ca_oa_share_ref    <- core$ca_oa_share_ref
    entry$core_ca_works_period    <- core$ca_works_period
    entry$core_ca_oa_share_period <- core$ca_oa_share_period
  }
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

message("Wrote metrics for ", nrow(snap$metrics), " institutions (snapshot ",
        snapshot_date, "); ", length(unique(all_metrics$snapshot_date)),
        " snapshot(s) on record.")
