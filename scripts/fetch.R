#!/usr/bin/env Rscript
# Fetch institution metadata for the TU9-related institutions from OpenAlex and
# write the data views:
#
#   data/snapshots/<slug>/<date>.json  raw institution entity dump (one per run)
#   data/metrics.csv                   dated time series, one row per (date, inst)
#   data/counts_by_year.csv            works/citations by publication year (latest)
#   data/<slug>/metrics.csv            one institution's metric time series
#   data/<slug>/counts_by_year.csv     one institution's counts by year (latest)
#   data/meta.json                     summary + last-updated date (for the site)
#
# The OpenAlex institution entity already ships the aggregated metrics we track
# (works_count, cited_by_count, summary_stats, counts_by_year), so this pipeline
# only RETRIEVES and records them over time -- it does not aggregate works.
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
force <- nzchar(Sys.getenv("FORCE"))
snapshot_date <- Sys.getenv("SNAPSHOT_DATE")
if (!nzchar(snapshot_date)) snapshot_date <- format(Sys.Date())

inst <- read_institutions()
message("Institutions to fetch: ", nrow(inst))

# --- collect ---------------------------------------------------------------
metric_rows <- list()
cby_rows    <- list()

for (i in seq_len(nrow(inst))) {
  row <- inst[i, ]
  message("  ", i, "/", nrow(inst), "  ", row$slug, " (", row$openalex_bare, ")")
  obj <- openalex_institution_reader(row$openalex_bare)
  if (is.null(obj)) next

  m <- openalex_metrics(obj)
  if (is.na(m$works_count) || m$works_count <= 0) {
    message("    skipped: implausible works_count for ", row$slug)
    next
  }

  # Raw JSON snapshot for this institution and date (archive / reproducibility).
  snap_dir <- file.path("data", "snapshots", row$slug)
  dir.create(snap_dir, recursive = TRUE, showWarnings = FALSE)
  write_json(obj, file.path(snap_dir, paste0(snapshot_date, ".json")),
             auto_unbox = TRUE, pretty = TRUE, null = "null")

  metric_rows[[length(metric_rows) + 1L]] <- tibble(
    snapshot_date         = snapshot_date,
    slug                  = row$slug,
    name                  = row$name,
    type                  = row$type,
    openalex_id           = row$openalex_id,
    ror_id                = row$ror_id,
    works_count           = m$works_count,
    cited_by_count        = m$cited_by_count,
    h_index               = m$h_index,
    i10_index             = m$i10_index,
    two_yr_mean_citedness = m$two_yr_mean_cited
  )

  cby <- openalex_counts_by_year(obj)
  if (nrow(cby) > 0) {
    cby_rows[[length(cby_rows) + 1L]] <- tibble(
      snapshot_date  = snapshot_date,
      slug           = row$slug,
      year           = cby$year,
      works_count    = cby$works_count,
      cited_by_count = cby$cited_by_count
    )
  }
}

if (length(metric_rows) == 0) {
  stop("No institutions fetched successfully; aborting without touching data.")
}

new_metrics <- bind_rows(metric_rows)
new_cby     <- bind_rows(cby_rows)

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
  if (file.exists(path)) {
    old <- read_csv(path, col_types = cols(.default = col_character()))
    old <- old[old$snapshot_date != snapshot_date, ]        # replace today's rows
    combined <- bind_rows(old, mutate(new_df, across(everything(), as.character)))
  } else {
    combined <- mutate(new_df, across(everything(), as.character))
  }
  combined <- combined[do.call(order, combined[sort_cols]), ]
  write_csv(combined, path, na = "")
  combined
}

all_metrics <- append_dedup(metrics_path, new_metrics,
                            key_cols = c("snapshot_date", "slug"),
                            sort_cols = c("snapshot_date", "slug"))

# counts_by_year is a publication-year view; keep only the LATEST snapshot at the
# top level (the raw JSON archive preserves every snapshot's counts_by_year).
write_csv(new_cby[order(new_cby$slug, -new_cby$year), ], "data/counts_by_year.csv",
          na = "")

# --- per-institution views -------------------------------------------------
for (slug in unique(new_metrics$slug)) {
  d <- file.path("data", slug)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  im <- all_metrics[all_metrics$slug == slug, ]
  write_csv(im[order(im$snapshot_date), ], file.path(d, "metrics.csv"), na = "")
  ic <- new_cby[new_cby$slug == slug, ]
  write_csv(ic[order(-ic$year), ], file.path(d, "counts_by_year.csv"), na = "")
}

# --- meta.json -------------------------------------------------------------
latest <- new_metrics
meta_inst <- lapply(seq_len(nrow(latest)), function(i) {
  r  <- latest[i, ]
  hist <- all_metrics[all_metrics$slug == r$slug, ]
  list(
    name           = r$name,
    slug           = r$slug,
    type           = r$type,
    openalex_id    = r$openalex_id,
    ror_id         = r$ror_id,
    works_count    = r$works_count,
    cited_by_count = r$cited_by_count,
    h_index        = r$h_index,
    first_snapshot = min(hist$snapshot_date),
    latest_snapshot = max(hist$snapshot_date)
  )
})

meta <- list(
  updated        = snapshot_date,
  source         = "OpenAlex (institution entities, CC0)",
  n_institutions = nrow(latest),
  n_snapshots    = length(unique(all_metrics$snapshot_date)),
  institutions   = meta_inst
)
write_json(meta, "data/meta.json", auto_unbox = TRUE, pretty = TRUE, null = "null")

message("Wrote metrics for ", nrow(new_metrics), " institutions (snapshot ",
        snapshot_date, "); ", length(unique(all_metrics$snapshot_date)),
        " snapshot(s) on record.")
