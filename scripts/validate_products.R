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
#
# Depends on scripts/openalex.R (configuration and mapping readers) and
# scripts/validate.R (validate_snapshot, which does the semantic checking);
# fetch.R sources both before this file.

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
  # Explicit format: bare as.Date() errors on an unparseable string instead of
  # returning NA, which would abort here rather than report the bad value.
  if (!nzchar(updated) || is.na(as.Date(updated, format = "%Y-%m-%d")))
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

  # Everything above is topology: which files exist, which dates they carry, who
  # they cover, whether the slices agree. That is not the same as the data being
  # right, and reporting "complete and consistent" on the strength of it
  # overstated what had been checked -- a wrong OA share, a mirrored 2099 row and
  # a falsified meta.json headline all passed.
  #
  # Rather than restating those rules here and letting the two copies drift, feed
  # the published files back through validate_snapshot(), the same function that
  # guards the fetch. The member summaries are taken from meta.json on purpose:
  # its consolidated and Core headlines are then checked against the yearly rows
  # in the CSVs, which is what nothing verified before.
  if (length(issues) == 0) {
    leiden <- read_leiden_components(file.path(raw_dir, "leiden_affiliations.csv"),
                                     inst = inst)
    if (is.null(leiden))
      add("Leiden mapping is missing; cannot verify the consolidated member sets")

    members_of <- function(slug) unique(c(
      openalex_bare(inst$openalex_id[inst$slug == slug]),
      openalex_bare(leiden$affiliated_openalex_id[leiden$tu9_slug == slug])))
    by_slug <- function(meta_prefix) {
      setNames(lapply(expected, function(s) {
        e <- Filter(function(x) identical(as.character(x$slug %or% ""), s),
                    meta$institutions %or% list())
        e <- if (length(e) > 0) e[[1]] else list()
        # `members` can only come from the Leiden mapping -- meta.json does not
        # list them -- but `n_members` is a published field, so take it from
        # meta.json and let validate_snapshot() reconcile the two. Recomputing it
        # here from the same mapping the member list comes from would have made
        # the check unfalsifiable.
        list(n_members       = n(e[[paste0(meta_prefix, "_n_members")]] %or% NA),
             members         = members_of(s),
             ca_works_ref    = n(e[[paste0(meta_prefix, "_ca_works_ref")]] %or% NA),
             ca_oa_share_ref = n(e[[paste0(meta_prefix, "_ca_oa_share_ref")]] %or% NA),
             ca_works_period = n(e[[paste0(meta_prefix, "_ca_works_period")]] %or% NA),
             ca_oa_share_period = n(e[[paste0(meta_prefix, "_ca_oa_share_period")]] %or% NA))
      }), expected)
    }

    mt <- loaded[["metrics.csv"]]
    ents <- list()
    for (slug in expected) {
      f <- file.path(data_dir, "snapshots", slug, paste0(updated, ".json"))
      # Existence was checked above; a snapshot that does not parse is just as
      # useless as one that is absent, and nothing read them until now.
      ents[[slug]] <- tryCatch(jsonlite::read_json(f, simplifyVector = FALSE),
                               error = function(e) {
                                 add("raw snapshot does not parse: %s (%s)", f,
                                     conditionMessage(e))
                                 NULL
                               })
    }

    snap <- list(
      metrics        = mt[as.character(mt$snapshot_date) == updated, , drop = FALSE],
      counts_by_year = loaded[["counts_by_year.csv"]],
      ca_oa_by_year  = loaded[["ca_oa_by_year.csv"]],
      ca_oa_status   = loaded[["ca_oa_status.csv"]],
      consolidated   = loaded[["consolidated_ca_oa_by_year.csv"]],
      core           = loaded[["leiden_core_ca_oa_by_year.csv"]],
      cons_members   = by_slug("cons"),
      core_members   = by_slug("core"),
      entities       = ents,
      failures       = character())

    period_start <- as.integer(meta$oa_period_start %or% NA)
    if (is.na(period_start)) add("meta.json has no oa_period_start")
    # force = TRUE skips only the coverage guard rail: it compares this snapshot
    # with the previous one, which is a question about change over time, not
    # about whether the published data is well formed. It already ran at fetch.
    sem <- tryCatch({
      validate_snapshot(snap, inst, leiden, updated,
                        prev_metrics = NULL, force = TRUE,
                        period_start = if (is.na(period_start)) 2020 else period_start)
      character()
    }, error = function(e) conditionMessage(e))
    if (length(sem) > 0 && nzchar(sem))
      add("semantic validation of the published products failed:\n    %s",
          gsub("\n", "\n    ", sem))

    # --- the published metrics HISTORY ------------------------------------
    # Only the rows of the current snapshot went through validate_snapshot()
    # above, but metrics.csv is published whole: the institution pages plot the
    # full series and the file is offered for download. A corrupted older row is
    # therefore just as visible as a current one, and nothing looked at it.
    #
    # This is deliberately a light contract -- shape, identity, and the row-level
    # invariants that hold regardless of age. Historical figures are NOT
    # re-derived from the archives: the grouped responses behind them were never
    # stored, so there is nothing to re-derive them from.
    if (!is.null(mt) && nrow(mt) > 0) {
      gone <- setdiff(c("snapshot_date", "slug", "name", "openalex_id", "ror_id",
                        "works_count", "works_count_incl_xpac",
                        "works_count_lineage_incl_xpac", "cited_by_count",
                        "h_index", "i10_index", "ca_works_ref", "ca_oa_share_ref",
                        "ca_works_period", "ca_oa_share_period"), names(mt))
      if (length(gone) > 0)
        add("metrics.csv lacks required column(s): %s", paste(gone, collapse = ", "))

      dates <- as.character(mt$snapshot_date)
      # Parse with an explicit format: bare as.Date() raises an error on an
      # unparseable string rather than returning NA, so it would abort with a
      # stack trace instead of reporting the bad value alongside every other
      # issue. The format is also the one the pipeline writes, so this pins the
      # shape as well as the validity.
      parsed <- as.Date(dates, format = "%Y-%m-%d")
      bad_date <- unique(dates[is.na(parsed)])
      if (length(bad_date) > 0)
        add("metrics.csv has unusable snapshot_date(s): %s",
            paste(utils::head(bad_date, 5), collapse = ", "))
      # A row dated after the published snapshot would render as a point beyond
      # the end of the series -- the time-series equivalent of the future-year
      # rows already rejected in the yearly views.
      up_date <- if (nzchar(updated)) as.Date(updated, format = "%Y-%m-%d") else NA
      if (!is.na(up_date)) {
        ahead <- unique(dates[!is.na(parsed) & parsed > up_date])
        if (length(ahead) > 0)
          add("metrics.csv has snapshot_date(s) beyond the published %s: %s",
              updated, paste(utils::head(ahead, 5), collapse = ", "))
      }
      id <- paste(mt$slug, dates, sep = "\r")
      dup <- unique(id[duplicated(id)])
      if (length(dup) > 0)
        add("metrics.csv has %d duplicate (slug, snapshot_date) row(s): %s",
            length(dup), paste(gsub("\r", "/", utils::head(dup, 5)), collapse = "; "))

      for (dt in sort(unique(dates))) {
        h <- mt[dates == dt, , drop = FALSE]
        got_h <- sort(unique(h$slug))
        if (!identical(got_h, expected))
          add("metrics.csv snapshot %s covers [%s], expected [%s]", dt,
              paste(got_h, collapse = ", "), paste(expected, collapse = ", "))
        for (i in seq_len(nrow(h))) {
          cfg <- inst[inst$slug == h$slug[i], , drop = FALSE]
          if (nrow(cfg) != 1) next  # already reported as a coverage problem
          for (f in c("name", "openalex_id", "ror_id")) {
            if (!identical(as.character(h[[f]][i]), as.character(cfg[[f]])))
              add("metrics.csv %s: %s for %s is '%s', the configuration says '%s'",
                  dt, f, h$slug[i], h[[f]][i], cfg[[f]])
          }
        }
        # Fields the pages actually display must be real numbers, in every row
        # of the series and not only the newest one.
        for (col in c("works_count", "works_count_incl_xpac",
                      "works_count_lineage_incl_xpac", "cited_by_count",
                      "h_index", "i10_index", "ca_works_ref", "ca_works_period")) {
          if (!(col %in% names(h))) next
          v <- n(h[[col]])
          bad <- which(is.na(v) | !is.finite(v) | v < 0)
          if (length(bad) > 0)
            add("metrics.csv %s: %s is missing, non-finite or negative for: %s",
                dt, col, paste(h$slug[bad], collapse = ", "))
        }
        metrics_row_invariants(h, add, prefix = sprintf("metrics.csv %s: ", dt))
      }
    }

    # (The n_members columns in the two yearly products are checked inside
    # validate_snapshot() against the same member sets, so they are not restated
    # here -- one copy of the rule, exercised on both paths.)

    # meta.json fields that no product check touched.
    if (!is.null(mt)) {
      cur <- mt[as.character(mt$snapshot_date) == updated, , drop = FALSE]
      for (e in meta$institutions %or% list()) {
        slug <- as.character(e$slug %or% "")
        r <- cur[cur$slug == slug, , drop = FALSE]
        if (nrow(r) != 1) next
        for (f in c("name", "openalex_id", "ror_id")) {
          if (!identical(as.character(e[[f]] %or% ""), as.character(r[[f]])))
            add("meta.json %s for %s is '%s', metrics.csv says '%s'", f, slug,
                as.character(e[[f]] %or% ""), as.character(r[[f]]))
        }
        # The site prints these as the span of the time series. metrics.csv keeps
        # the whole history, so both are reconstructible -- and neither was read.
        hist <- sort(as.character(mt$snapshot_date[mt$slug == slug]))
        if (length(hist) > 0) {
          for (fld in list(c("first_snapshot", hist[1]),
                           c("latest_snapshot", hist[length(hist)]))) {
            if (!identical(as.character(e[[fld[1]]] %or% ""), fld[2]))
              add("meta.json %s for %s is '%s', metrics.csv history says '%s'",
                  fld[1], slug, as.character(e[[fld[1]]] %or% ""), fld[2])
          }
        }
      }
      for (pair in list(c("oa_ref_year", "ref_year"),
                        c("oa_period_start", "period_start"),
                        c("oa_period_end", "period_end"))) {
        want <- unique(n(cur[[pair[2]]]))
        if (length(want) == 1 && !isTRUE(all.equal(n(meta[[pair[1]]] %or% NA), want)))
          add("meta.json %s is %s, metrics.csv %s is %s", pair[1],
              paste(meta[[pair[1]]], collapse = ""), pair[2], want)
      }
      # The provenance line printed under every table. It is a constant, so the
      # only way it can be wrong is by having been changed in one place and not
      # the other; pin it rather than leaving the one free-text field unchecked.
      want_source <- "OpenAlex (corresponding-author works, CC0)"
      if (!identical(as.character(meta$source %or% ""), want_source))
        add("meta.json source is '%s', expected '%s'",
            as.character(meta$source %or% ""), want_source)
      n_snap <- length(unique(as.character(mt$snapshot_date)))
      if (!identical(as.integer(meta$n_snapshots %or% NA_integer_), n_snap))
        add("meta.json n_snapshots is %s, metrics.csv holds %d",
            paste(meta$n_snapshots, collapse = ""), n_snap)
      if (!is.null(leiden)) {
        want_ent <- length(unique(c(openalex_bare(inst$openalex_id),
                                    openalex_bare(leiden$affiliated_openalex_id))))
        if (!identical(as.integer(meta$n_entities %or% NA_integer_), want_ent))
          add("meta.json n_entities is %s, the configuration and mapping give %d",
              paste(meta$n_entities, collapse = ""), want_ent)
      }
    }
  }

  if (length(issues) > 0)
    stop(sprintf("Product validation failed (%d issue(s)):\n  - %s",
                 length(issues), paste(issues, collapse = "\n  - ")), call. = FALSE)
  message("Product validation passed: ", length(expected),
          " institutions; published products complete, and semantically valid ",
          "under the same checks as the fetch (", updated, ").")
  invisible(TRUE)
}

# Standalone use: `Rscript scripts/validate_products.R`. When this file is
# sourced (by fetch.R) sys.nframe() is non-zero, so the call below is skipped.
if (sys.nframe() == 0L) {
  source("scripts/openalex.R")
  source("scripts/validate.R")   # supplies validate_snapshot()
  validate_products()
}
