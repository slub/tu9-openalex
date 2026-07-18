#!/usr/bin/env Rscript
# Fetch institution metadata for the TU9-related institutions from OpenAlex and
# write the data views:
#
#   data/snapshots/<slug>/<date>.json  raw institution entity dump (one per run)
#   data/metrics.csv                   dated time series, one row per (date, inst)
#   data/counts_by_year.csv            works/citations by publication year (latest)
#   data/ca_oa_by_year.csv             corresponding-author OA share by year (latest)
#   data/ca_oa_status.csv              CA OA-status split for the reference year (latest)
#   data/consolidated_ca_oa_by_year.csv  Leiden-consolidated CA OA share by year (latest)
#   data/<slug>/*.csv                  the same views for one institution
#   data/meta.json                     summary + last-updated date (for the site)
#
# All counts EXCLUDE the OpenAlex Expansion Pack (XPAC) -- the works API's
# default -- so figures reflect scholarly output, not the ~190M lower-quality
# dataset/repository records added in 11/2025. Because the institution entity's
# own aggregates INCLUDE XPAC, works_count and works-by-year are sourced from the
# works API (XPAC-excluded); cited_by_count / h-index / i10 / 2yr-mean-citedness
# are kept from the entity (XPAC records carry ~no citations, so the effect on
# citation indicators is negligible).
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

# --- configuration ---------------------------------------------------------
GUARD_THRESHOLD <- 0.20  # abort the whole run if the total works count drops by
                         # more than this vs. the previous snapshot (data glitch)
OA_START_YEAR   <- 2013  # earliest publication year for the OA-by-year view
PERIOD_START    <- 2020  # start of the headline CA period (end = REF_YEAR)
force <- nzchar(Sys.getenv("FORCE"))
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

inst <- read_institutions()
message("Institutions to fetch: ", nrow(inst))

# --- collect ---------------------------------------------------------------
metric_rows    <- list()
cby_rows       <- list()
oa_year_rows   <- list()
oa_status_rows <- list()

for (i in seq_len(nrow(inst))) {
  row <- inst[i, ]
  message("  ", i, "/", nrow(inst), "  ", row$slug, " (", row$openalex_bare, ")")
  obj <- openalex_institution_reader(row$openalex_bare)
  if (is.null(obj)) next

  m <- openalex_metrics(obj)
  # works_count / works-by-year come XPAC-excluded from the works API (the entity
  # aggregates include the Expansion Pack); fall back to the entity only if the
  # works query fails.
  wby <- openalex_works_by_year(row$openalex_bare)
  works_count <- if (!is.null(wby) && !is.na(wby$total)) wby$total else m$works_count
  if (is.na(works_count) || works_count <= 0) {
    message("    skipped: implausible works_count for ", row$slug)
    next
  }

  # Raw JSON snapshot for this institution and date (archive / reproducibility).
  snap_dir <- file.path("data", "snapshots", row$slug)
  dir.create(snap_dir, recursive = TRUE, showWarnings = FALSE)
  write_json(obj, file.path(snap_dir, paste0(snapshot_date, ".json")),
             auto_unbox = TRUE, pretty = TRUE, null = "null")

  # OA metrics on the corresponding-author lens (best effort: a failure leaves
  # the OA fields NA but never drops the institution's entity metrics). The
  # `ca_` prefix marks these as corresponding-author-level, not whole-output.
  oa <- openalex_ca_oa_by_year(row$openalex_bare, OA_START_YEAR)
  ref <- if (!is.null(oa)) oa[oa$year == REF_YEAR, ] else NULL
  ca_works_ref    <- if (!is.null(ref) && nrow(ref)) ref$ca_works    else NA_integer_
  ca_oa_works_ref <- if (!is.null(ref) && nrow(ref)) ref$ca_oa_works else NA_integer_
  ca_oa_share_ref <- if (!is.null(ref) && nrow(ref)) ref$ca_oa_share else NA_real_
  # Headline over the period [PERIOD_START, REF_YEAR] (more robust than one year).
  pa <- if (!is.null(oa)) period_ca(oa) else list(works = NA_integer_,
                                                   oa_works = NA_integer_, share = NA_real_)

  if (!is.null(oa)) {
    oa_year_rows[[length(oa_year_rows) + 1L]] <- tibble(
      snapshot_date = snapshot_date,
      slug          = row$slug,
      year          = oa$year,
      ca_works      = oa$ca_works,
      ca_oa_works   = oa$ca_oa_works,
      ca_oa_share   = oa$ca_oa_share
    )
  }

  st <- openalex_ca_oa_status(row$openalex_bare, REF_YEAR)
  if (!is.null(st) && nrow(st) > 0) {
    oa_status_rows[[length(oa_status_rows) + 1L]] <- tibble(
      snapshot_date = snapshot_date,
      slug          = row$slug,
      year          = REF_YEAR,
      oa_status     = st$oa_status,
      ca_works      = st$ca_works
    )
  }

  metric_rows[[length(metric_rows) + 1L]] <- tibble(
    snapshot_date         = snapshot_date,
    slug                  = row$slug,
    name                  = row$name,
    type                  = row$type,
    openalex_id           = row$openalex_id,
    ror_id                = row$ror_id,
    works_count           = works_count,
    cited_by_count        = m$cited_by_count,
    h_index               = m$h_index,
    i10_index             = m$i10_index,
    two_yr_mean_citedness = m$two_yr_mean_cited,
    ref_year              = REF_YEAR,
    ca_works_ref          = ca_works_ref,
    ca_oa_works_ref       = ca_oa_works_ref,
    ca_oa_share_ref       = ca_oa_share_ref,
    period_start          = PERIOD_START,
    period_end            = REF_YEAR,
    ca_works_period       = pa$works,
    ca_oa_works_period    = pa$oa_works,
    ca_oa_share_period    = pa$share
  )

  # counts_by_year: keep the entity's year range and citations-per-year, but
  # replace works-per-year with the XPAC-excluded works figures (fall back to the
  # entity value for any year the works query did not return).
  cby <- openalex_counts_by_year(obj)
  if (nrow(cby) > 0) {
    works_by_year <- cby$works_count
    if (!is.null(wby) && nrow(wby$by_year) > 0) {
      m_idx <- match(cby$year, wby$by_year$year)
      repl <- wby$by_year$works_count[m_idx]
      works_by_year <- ifelse(is.na(repl), cby$works_count, repl)
    }
    cby_rows[[length(cby_rows) + 1L]] <- tibble(
      snapshot_date  = snapshot_date,
      slug           = row$slug,
      year           = cby$year,
      works_count    = works_by_year,
      cited_by_count = cby$cited_by_count
    )
  }
}

if (length(metric_rows) == 0) {
  stop("No institutions fetched successfully; aborting without touching data.")
}

new_metrics   <- bind_rows(metric_rows)
new_cby       <- bind_rows(cby_rows)
new_oa_year   <- bind_rows(oa_year_rows)
new_oa_status <- bind_rows(oa_status_rows)

# --- guard rail ------------------------------------------------------------
# Compare the total works count of this snapshot against the most recent
# previous snapshot in metrics.csv. A large drop usually means a bad fetch.
metrics_path <- "data/metrics.csv"
if (file.exists(metrics_path) && !force) {
  old <- read_csv(metrics_path, col_types = cols(
    .default = col_character(), works_count = col_integer()))
  old <- old[old$snapshot_date != snapshot_date, ]
  if (nrow(old) > 0) {
    last_date <- max(old$snapshot_date)
    old_total <- sum(old$works_count[old$snapshot_date == last_date], na.rm = TRUE)
    new_total <- sum(new_metrics$works_count, na.rm = TRUE)
    if (old_total > 0 && new_total < old_total * (1 - GUARD_THRESHOLD)) {
      stop(sprintf(paste0(
        "Guard rail tripped: total works fell from %d (%s) to %d (%s), ",
        "more than %.0f%%. Set FORCE=1 to override."),
        old_total, last_date, new_total, snapshot_date, GUARD_THRESHOLD * 100))
    }
  }
}

# --- merge with history (idempotent on snapshot_date) ----------------------
append_dedup <- function(path, new_df, key_cols, sort_cols) {
  new_df <- mutate(new_df, across(everything(), as.character))
  combined <- new_df
  if (file.exists(path)) {
    old <- read_csv(path, col_types = cols(.default = col_character()))
    old <- old[old$snapshot_date != snapshot_date, ]        # replace today's rows
    # Only carry history forward if any remains; binding an empty old frame would
    # otherwise re-introduce its columns (e.g. after a schema change). Align the
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

all_metrics <- append_dedup(metrics_path, new_metrics,
                            key_cols = c("snapshot_date", "slug"),
                            sort_cols = c("snapshot_date", "slug"))

# counts_by_year and the OA views are publication-year views; keep only the
# LATEST snapshot at the top level (the raw JSON archive and the snapshot column
# preserve provenance). metrics.csv remains the snapshot-over-time series.
write_csv(new_cby[order(new_cby$slug, -new_cby$year), ], "data/counts_by_year.csv",
          na = "")
if (nrow(new_oa_year) > 0) {
  write_csv(new_oa_year[order(new_oa_year$slug, -new_oa_year$year), ],
            "data/ca_oa_by_year.csv", na = "")
}
if (nrow(new_oa_status) > 0) {
  write_csv(new_oa_status[order(new_oa_status$slug, -new_oa_status$ca_works), ],
            "data/ca_oa_status.csv", na = "")
}

# --- per-institution views -------------------------------------------------
for (slug in unique(new_metrics$slug)) {
  d <- file.path("data", slug)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  im <- all_metrics[all_metrics$slug == slug, ]
  write_csv(im[order(im$snapshot_date), ], file.path(d, "metrics.csv"), na = "")
  ic <- new_cby[new_cby$slug == slug, ]
  write_csv(ic[order(-ic$year), ], file.path(d, "counts_by_year.csv"), na = "")
  if (nrow(new_oa_year) > 0) {
    io <- new_oa_year[new_oa_year$slug == slug, ]
    if (nrow(io) > 0)
      write_csv(io[order(-io$year), ], file.path(d, "ca_oa_by_year.csv"), na = "")
  }
  if (nrow(new_oa_status) > 0) {
    is_ <- new_oa_status[new_oa_status$slug == slug, ]
    if (nrow(is_) > 0)
      write_csv(is_[order(-is_$ca_works), ], file.path(d, "ca_oa_status.csv"), na = "")
  }
}

# --- Leiden-consolidated CA-OA view ----------------------------------------
# For each TU9 university, OR its OpenAlex id with those of its weight-1
# `component` affiliates (from the Leiden Ranking mapping) and recompute the
# corresponding-author OA share. This gives a "university incl. hospital/
# institutes" unit alongside the per-entity figures. Universities without
# components are skipped (their consolidated view equals the entity view).
leiden <- tryCatch(read_leiden_components(), error = function(e) NULL)
cons_year_rows <- list()
cons_by_slug   <- list()
if (!is.null(leiden)) {
  unis <- new_metrics[new_metrics$type == "university", ]
  for (k in seq_len(nrow(unis))) {
    u <- unis[k, ]
    comp <- leiden$affiliated_openalex_id[leiden$tu9_slug == u$slug]
    members <- unique(c(openalex_bare(u$openalex_id), openalex_bare(comp)))
    if (length(members) < 2) next
    message("  consolidated ", u$slug, " (", length(members), " members)")
    coa <- openalex_ca_oa_by_year(members, OA_START_YEAR)
    if (is.null(coa)) next
    cref <- coa[coa$year == REF_YEAR, ]
    cpa  <- period_ca(coa)
    cons_by_slug[[u$slug]] <- list(
      n_members         = length(members),
      ca_works_ref      = if (nrow(cref)) cref$ca_works    else NA_integer_,
      ca_oa_share_ref   = if (nrow(cref)) cref$ca_oa_share else NA_real_,
      ca_works_period   = cpa$works,
      ca_oa_share_period = cpa$share)
    cons_year_rows[[length(cons_year_rows) + 1L]] <- tibble(
      snapshot_date   = snapshot_date,
      tu9_slug        = u$slug,
      university_name = u$name,
      n_members       = length(members),
      year            = coa$year,
      ca_works        = coa$ca_works,
      ca_oa_works     = coa$ca_oa_works,
      ca_oa_share     = coa$ca_oa_share)
    write_csv(coa[order(-coa$year), ],
              file.path("data", u$slug, "consolidated_ca_oa_by_year.csv"), na = "")
  }
  if (length(cons_year_rows) > 0) {
    cons_all <- bind_rows(cons_year_rows)
    write_csv(cons_all[order(cons_all$tu9_slug, -cons_all$year), ],
              "data/consolidated_ca_oa_by_year.csv", na = "")
  }
}

# --- meta.json -------------------------------------------------------------
latest <- new_metrics
meta_inst <- lapply(seq_len(nrow(latest)), function(i) {
  r  <- latest[i, ]
  hist <- all_metrics[all_metrics$slug == r$slug, ]
  entry <- list(
    name           = r$name,
    slug           = r$slug,
    type           = r$type,
    openalex_id    = r$openalex_id,
    ror_id         = r$ror_id,
    works_count    = r$works_count,
    cited_by_count = r$cited_by_count,
    h_index        = r$h_index,
    ref_year       = r$ref_year,
    ca_works_ref   = r$ca_works_ref,
    ca_oa_share_ref = r$ca_oa_share_ref,
    period_start       = r$period_start,
    period_end         = r$period_end,
    ca_works_period    = r$ca_works_period,
    ca_oa_share_period = r$ca_oa_share_period,
    first_snapshot = min(hist$snapshot_date),
    latest_snapshot = max(hist$snapshot_date)
  )
  # Consolidated (Leiden) figures, only for universities that have components.
  cons <- cons_by_slug[[r$slug]]
  if (!is.null(cons)) {
    entry$cons_n_members         <- cons$n_members
    entry$cons_ca_works_ref      <- cons$ca_works_ref
    entry$cons_ca_oa_share_ref   <- cons$ca_oa_share_ref
    entry$cons_ca_works_period   <- cons$ca_works_period
    entry$cons_ca_oa_share_period <- cons$ca_oa_share_period
  }
  entry
})

# Total distinct OpenAlex institution entities involved: the tracked universities
# plus the Leiden `component` affiliates folded into the consolidated view.
comp_ids <- if (!is.null(leiden)) openalex_bare(leiden$affiliated_openalex_id) else character(0)
n_entities <- length(unique(c(openalex_bare(new_metrics$openalex_id), comp_ids)))

meta <- list(
  updated        = snapshot_date,
  source         = "OpenAlex (corresponding-author works, CC0)",
  n_institutions = nrow(latest),   # tracked universities
  n_entities     = n_entities,     # universities + consolidated component affiliates
  n_snapshots    = length(unique(all_metrics$snapshot_date)),
  oa_ref_year    = REF_YEAR,
  oa_period_start = PERIOD_START,
  oa_period_end   = REF_YEAR,
  institutions   = meta_inst
)
write_json(meta, "data/meta.json", auto_unbox = TRUE, pretty = TRUE, null = "null")

message("Wrote metrics for ", nrow(new_metrics), " institutions (snapshot ",
        snapshot_date, "); ", length(unique(all_metrics$snapshot_date)),
        " snapshot(s) on record.")
