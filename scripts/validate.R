#!/usr/bin/env Rscript
# Snapshot validation. `validate_snapshot()` is called by scripts/fetch.R after
# everything has been fetched into memory and BEFORE anything is written to
# data/. If any check fails the run aborts and the published data is left
# untouched -- an incomplete or internally inconsistent snapshot is never
# published (fail-loud).
#
# All problems are collected and reported together, so one run tells you
# everything that is wrong rather than one issue at a time.

# Required products for every configured institution. The consolidated view is
# required only for universities that actually have Leiden `component`
# affiliates -- for the others it is not merely optional, it must be ABSENT,
# because a leftover file would otherwise be mistaken for a current product.
validate_snapshot <- function(snap, inst, leiden_comp, snapshot_date,
                              prev_metrics = NULL, guard_threshold = 0.20,
                              force = FALSE) {
  issues <- character()
  add <- function(...) issues <<- c(issues, sprintf(...))

  expected <- sort(inst$slug)

  # --- 0. fetch failures ---------------------------------------------------
  if (length(snap$failures) > 0) {
    for (f in snap$failures) add("fetch failed: %s", f)
  }

  # --- 1. exact institution set -------------------------------------------
  got <- sort(unique(snap$metrics$slug))
  missing <- setdiff(expected, got)
  unexpected <- setdiff(got, expected)
  if (length(missing) > 0)
    add("missing institutions in metrics: %s", paste(missing, collapse = ", "))
  if (length(unexpected) > 0)
    add("unexpected institutions in metrics: %s", paste(unexpected, collapse = ", "))
  if (anyDuplicated(got) > 0)
    add("duplicate institution rows in metrics")

  # --- 2. mandatory products per institution -------------------------------
  need <- list(counts_by_year = snap$counts_by_year,
               ca_oa_by_year  = snap$ca_oa_by_year,
               ca_oa_status   = snap$ca_oa_status)
  for (nm in names(need)) {
    have <- unique(need[[nm]]$slug)
    gone <- setdiff(expected, have)
    if (length(gone) > 0)
      add("%s missing for: %s", nm, paste(gone, collapse = ", "))
  }
  for (slug in expected) {
    if (is.null(snap$entities[[slug]]))
      add("raw entity missing for: %s", slug)
  }

  # --- 3. consolidated view matches the Leiden mapping exactly --------------
  cons_expected <- sort(unique(leiden_comp$tu9_slug[leiden_comp$tu9_slug %in% expected]))
  cons_got <- if (nrow(snap$consolidated) > 0) sort(unique(snap$consolidated$tu9_slug)) else character()
  if (!identical(cons_expected, cons_got)) {
    add("consolidated view mismatch: expected {%s}, got {%s}",
        paste(cons_expected, collapse = ", "), paste(cons_got, collapse = ", "))
  }

  # --- 4. one consistent snapshot_date everywhere --------------------------
  for (nm in c("metrics", "counts_by_year", "ca_oa_by_year", "ca_oa_status",
               "consolidated")) {
    d <- snap[[nm]]
    if (is.null(d) || nrow(d) == 0) next
    bad <- setdiff(unique(as.character(d$snapshot_date)), snapshot_date)
    if (length(bad) > 0)
      add("%s carries foreign snapshot_date(s): %s", nm, paste(bad, collapse = ", "))
  }

  # --- 5. numeric invariants ----------------------------------------------
  m <- snap$metrics
  n <- function(x) suppressWarnings(as.numeric(x))
  if (any(n(m$works_count) <= 0, na.rm = TRUE))
    add("non-positive works_count for: %s",
        paste(m$slug[n(m$works_count) <= 0], collapse = ", "))
  # XPAC-excluded can never exceed the XPAC-inclusive entity count. This is the
  # invariant that catches a cross-definition fallback slipping back in.
  bad_xpac <- which(n(m$works_count) > n(m$works_count_incl_xpac))
  if (length(bad_xpac) > 0)
    add("works_count exceeds works_count_incl_xpac for: %s",
        paste(m$slug[bad_xpac], collapse = ", "))
  # The period window contains the reference year, so it cannot be smaller.
  bad_period <- which(n(m$ca_works_period) < n(m$ca_works_ref))
  if (length(bad_period) > 0)
    add("ca_works_period < ca_works_ref for: %s",
        paste(m$slug[bad_period], collapse = ", "))

  # OA numerator/denominator and share arithmetic, in both OA tables.
  check_oa <- function(d, label) {
    if (is.null(d) || nrow(d) == 0) return(invisible(NULL))
    w <- n(d$ca_works); o <- n(d$ca_oa_works); s <- n(d$ca_oa_share)
    if (any(o > w, na.rm = TRUE))
      add("%s: ca_oa_works exceeds ca_works in %d row(s)", label, sum(o > w, na.rm = TRUE))
    if (any(w < 0 | o < 0, na.rm = TRUE))
      add("%s: negative counts", label)
    expect <- ifelse(w > 0, round(o / w, 4), NA_real_)
    off <- which(!is.na(s) & !is.na(expect) & abs(s - expect) > 1e-9)
    if (length(off) > 0)
      add("%s: ca_oa_share does not match round(ca_oa_works/ca_works, 4) in %d row(s)",
          label, length(off))
  }
  check_oa(snap$ca_oa_by_year, "ca_oa_by_year")
  check_oa(snap$consolidated, "consolidated_ca_oa_by_year")

  # OA-status categories must add up to the reference-year CA works.
  if (nrow(snap$ca_oa_status) > 0) {
    tot <- tapply(n(snap$ca_oa_status$ca_works), snap$ca_oa_status$slug, sum)
    ref <- setNames(n(m$ca_works_ref), m$slug)
    for (slug in names(tot)) {
      # A slug with status rows but no metrics row is already reported above as
      # a missing institution; skip it rather than index out of bounds.
      if (!(slug %in% names(ref)) || is.na(ref[[slug]])) next
      if (!isTRUE(all.equal(unname(tot[[slug]]), unname(ref[[slug]]))))
        add("ca_oa_status total (%s) != ca_works_ref (%s) for %s",
            tot[[slug]], ref[[slug]], slug)
    }
  }

  # --- 6. guard rail against a collapse in coverage ------------------------
  if (!is.null(prev_metrics) && nrow(prev_metrics) > 0 && !force) {
    prev <- prev_metrics[prev_metrics$snapshot_date != snapshot_date, , drop = FALSE]
    if (nrow(prev) > 0) {
      last_date <- max(prev$snapshot_date)
      old_total <- sum(n(prev$works_count[prev$snapshot_date == last_date]), na.rm = TRUE)
      new_total <- sum(n(m$works_count), na.rm = TRUE)
      if (old_total > 0 && new_total < old_total * (1 - guard_threshold))
        add(paste0("guard rail: total works fell from %d (%s) to %d (%s), ",
                   "more than %.0f%%. Set FORCE=1 to override."),
            old_total, last_date, new_total, snapshot_date, guard_threshold * 100)
    }
  }

  if (length(issues) > 0) {
    stop(sprintf(
      "Snapshot validation failed (%d issue(s)); nothing was written:\n  - %s",
      length(issues), paste(issues, collapse = "\n  - ")), call. = FALSE)
  }
  message("Validation passed: ", length(expected), " institutions, all required products present.")
  invisible(TRUE)
}
