#!/usr/bin/env Rscript
# Product validation -- checks what is ON DISK, after writing.
#
# scripts/validate.R validates the snapshot while it is still in memory, so
# nothing incomplete is ever written. That left the second half of the pipeline
# unguarded: the write itself, the per-institution slices, meta.json and the raw
# JSON archive are all produced AFTER the last check, and a defect there reached
# the site unopposed. .github/workflows/pages.yml renders the committed data
# without running fetch.R at all, so on that path there was no validation
# whatsoever.
#
# This validates the published artefacts as a reader of the site sees them:
#
#   Rscript scripts/validate_products.R      # standalone (used by both workflows)
#
# and fetch.R calls validate_products() itself once the write is complete.

suppressPackageStartupMessages({
  library(readr)
  library(jsonlite)
})

validate_products <- function(data_dir = "data", raw_dir = "data-raw", inst = NULL) {
  issues <- character()
  add <- function(...) issues <<- c(issues, sprintf(...))
  n <- function(x) suppressWarnings(as.numeric(x))
  `%or%` <- function(a, b) if (is.null(a)) b else a

  if (is.null(inst))
    inst <- read_institutions(file.path(raw_dir, "institutions.csv"))
  expected <- sort(inst$slug)

  rd <- function(path) read_csv(path, col_types = cols(.default = col_character()),
                                progress = FALSE)
  # Compare row sets, not row order: the global files are sorted by slug and the
  # per-institution ones by year, so an order-sensitive comparison would report
  # every file as different. Tab is safe as a joiner -- no field here contains one.
  row_keys <- function(d) {
    if (nrow(d) == 0) return(character())
    d <- d[, sort(names(d)), drop = FALSE]
    sort(do.call(paste, c(lapply(d, as.character), sep = "\t")))
  }

  # Every product below is mandatory. There is no best-effort product left: the
  # alliance table sums all three OA views over the same nine universities, so a
  # missing one yields a wrong page, not a thinner one.
  products <- list(
    list(file = "metrics.csv",                    key = "slug",     series = TRUE),
    list(file = "counts_by_year.csv",             key = "slug",     series = FALSE),
    list(file = "ca_oa_by_year.csv",              key = "slug",     series = FALSE),
    list(file = "ca_oa_status.csv",               key = "slug",     series = FALSE),
    list(file = "consolidated_ca_oa_by_year.csv", key = "tu9_slug", series = FALSE),
    list(file = "leiden_core_ca_oa_by_year.csv",  key = "tu9_slug", series = FALSE))

  meta_path <- file.path(data_dir, "meta.json")
  if (!file.exists(meta_path))
    stop("Product validation failed: meta.json is missing (", meta_path, ")",
         call. = FALSE)
  meta <- jsonlite::read_json(meta_path, simplifyVector = FALSE)
  updated <- as.character(meta$updated %or% "")
  if (!nzchar(updated) || is.na(suppressWarnings(as.Date(updated))))
    add("meta.json has no usable `updated` date: '%s'", updated)

  loaded <- list()
  for (p in products) {
    path <- file.path(data_dir, p$file)
    if (!file.exists(path)) { add("required product missing: %s", path); next }
    d <- rd(path)
    if (nrow(d) == 0) { add("required product is empty: %s", path); next }
    if (!("snapshot_date" %in% names(d))) { add("%s has no snapshot_date column", path); next }
    if (!(p$key %in% names(d))) { add("%s has no %s column", path, p$key); next }
    dates <- unique(as.character(d$snapshot_date))
    if (p$series) {
      # metrics.csv keeps the history; the published snapshot must be in it.
      if (nzchar(updated) && !(updated %in% dates))
        add("%s carries no rows for the published snapshot %s", path, updated)
    } else {
      # The yearly views are latest-only: exactly one date, and it must be the
      # published one. Another date here is a stale product rendered as current.
      if (length(dates) != 1)
        add("%s mixes %d snapshot dates: %s", path, length(dates),
            paste(dates, collapse = ", "))
      else if (nzchar(updated) && dates != updated)
        add("%s is stale: snapshot_date %s, published %s", path, dates, updated)
    }
    cur <- if (p$series) d[as.character(d$snapshot_date) == updated, , drop = FALSE] else d
    got <- sort(unique(cur[[p$key]]))
    if (!identical(got, expected))
      add("%s covers [%s], expected [%s]", path,
          paste(got, collapse = ", "), paste(expected, collapse = ", "))
    loaded[[p$file]] <- d
  }

  # Per-institution exports must exist and be exactly the slice of the global
  # file. They are written in a separate loop from the global files, so nothing
  # guaranteed the two agreed.
  for (slug in expected) {
    for (p in products) {
      path <- file.path(data_dir, slug, p$file)
      if (!file.exists(path)) { add("per-institution product missing: %s", path); next }
      d <- rd(path)
      if (nrow(d) == 0) { add("per-institution product is empty: %s", path); next }
      g <- loaded[[p$file]]
      if (is.null(g)) next  # the global file was already reported
      if (!identical(sort(names(d)), sort(names(g)))) {
        add("%s has different columns than the global file", path); next
      }
      want <- g[g[[p$key]] == slug, , drop = FALSE]
      if (!identical(row_keys(d), row_keys(want)))
        add("%s does not equal its slice of %s (%d vs %d row(s))",
            path, p$file, nrow(d), nrow(want))
    }
    # The raw entity archive is the provenance behind every published figure.
    if (nzchar(updated)) {
      snap_file <- file.path(data_dir, "snapshots", slug, paste0(updated, ".json"))
      if (!file.exists(snap_file)) add("raw snapshot missing: %s", snap_file)
    }
  }

  # A directory for an institution that is no longer configured would still be
  # published by publish_data.R and served as if it were current.
  dirs <- setdiff(list.dirs(data_dir, recursive = FALSE, full.names = FALSE), "snapshots")
  stray <- setdiff(dirs[nzchar(dirs)], expected)
  if (length(stray) > 0)
    add("unconfigured institution director(ies) under %s: %s", data_dir,
        paste(stray, collapse = ", "))
  snap_dirs <- list.dirs(file.path(data_dir, "snapshots"), recursive = FALSE,
                         full.names = FALSE)
  stray_snap <- setdiff(snap_dirs[nzchar(snap_dirs)], expected)
  if (length(stray_snap) > 0)
    add("unconfigured snapshot director(ies): %s", paste(stray_snap, collapse = ", "))

  # meta.json drives the headline tables on every page, but is written after the
  # last in-memory check and was never compared against the products it claims
  # to summarise.
  mt <- loaded[["metrics.csv"]]
  if (!is.null(mt) && nzchar(updated)) {
    cur <- mt[as.character(mt$snapshot_date) == updated, , drop = FALSE]
    mi <- meta$institutions %or% list()
    got <- sort(vapply(mi, function(e) as.character(e$slug %or% ""), character(1)))
    if (!identical(got, expected))
      add("meta.json lists [%s], expected [%s]",
          paste(got, collapse = ", "), paste(expected, collapse = ", "))
    if (!identical(as.integer(meta$n_institutions %or% NA_integer_), length(expected)))
      add("meta.json n_institutions is %s, expected %d",
          paste(meta$n_institutions, collapse = ""), length(expected))
    for (e in mi) {
      slug <- as.character(e$slug %or% "")
      r <- cur[cur$slug == slug, , drop = FALSE]
      if (nrow(r) != 1) next  # coverage mismatch already reported
      for (f in c("works_count", "cited_by_count", "h_index",
                  "ca_works_ref", "ca_oa_share_ref",
                  "ca_works_period", "ca_oa_share_period")) {
        want <- n(r[[f]]); got_v <- n(e[[f]] %or% NA)
        if (!isTRUE(all.equal(want, got_v)))
          add("meta.json %s for %s is %s, metrics.csv says %s", f, slug,
              paste(got_v, collapse = ""), paste(want, collapse = ""))
      }
    }
  }

  if (length(issues) > 0)
    stop(sprintf("Product validation failed (%d issue(s)):\n  - %s",
                 length(issues), paste(issues, collapse = "\n  - ")), call. = FALSE)
  message("Product validation passed: ", length(expected),
          " institutions, all published products complete and consistent (",
          updated, ").")
  invisible(TRUE)
}

# Standalone use: `Rscript scripts/validate_products.R`. When this file is
# sourced (by fetch.R) sys.nframe() is non-zero, so the call below is skipped.
if (sys.nframe() == 0L) {
  source("scripts/openalex.R")
  validate_products()
}
