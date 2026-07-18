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
                              force = FALSE, period_start = NULL) {
  if (is.null(period_start)) period_start <- 2020
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
               ca_oa_status   = snap$ca_oa_status,
               core           = snap$core)
  for (nm in names(need)) {
    d <- need[[nm]]
    if (is.null(d) || nrow(d) == 0) next
    have <- if (nm == "core") unique(d$tu9_slug) else unique(d$slug)
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

  # --- 4. core view is present for every university -------------------------
  core_got <- if (nrow(snap$core) > 0) sort(unique(snap$core$tu9_slug)) else character()
  missing_core <- setdiff(expected, core_got)
  unexpected_core <- setdiff(core_got, expected)
  if (length(missing_core) > 0)
    add("core view missing for: %s", paste(missing_core, collapse = ", "))
  if (length(unexpected_core) > 0)
    add("unexpected universities in core view: %s", paste(unexpected_core, collapse = ", "))

  # --- 5. core member set equals consolidated member set where applicable -----
  for (slug in expected) {
    core_mem <- snap$core_members[[slug]]$members
    cons_mem <- snap$cons_members[[slug]]$members
    if (is.null(core_mem)) {
      add("core member list missing for: %s", slug)
      next
    }
    if (!is.null(cons_mem)) {
      if (!identical(sort(core_mem), sort(cons_mem)))
        add("core/cons member mismatch for %s", slug)
    } else {
      # Without Leiden components the consolidated view is not produced, but the
      # core view must still use the university id alone.
      if (length(core_mem) != 1L || core_mem[1] != openalex_bare(inst$openalex_id[inst$slug == slug]))
        add("core member set for %s differs from single institution id", slug)
    }
  }

  # --- 6. one consistent snapshot_date everywhere --------------------------
  for (nm in c("metrics", "counts_by_year", "ca_oa_by_year", "ca_oa_status",
               "consolidated", "core")) {
    d <- snap[[nm]]
    if (is.null(d) || nrow(d) == 0) next
    bad <- setdiff(unique(as.character(d$snapshot_date)), snapshot_date)
    if (length(bad) > 0)
      add("%s carries foreign snapshot_date(s): %s", nm, paste(bad, collapse = ", "))
  }

  # --- 7. numeric invariants ----------------------------------------------
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

  # OA numerator/denominator and share arithmetic, in all OA tables.
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
  check_oa(snap$core, "leiden_core_ca_oa_by_year")

  # Core is a subset of consolidated (where consolidated exists) for every
  # university and year. It is also a subset of the single-institution view,
  # but the stronger check against consolidated catches the member-set logic.
  if (nrow(snap$core) > 0 && nrow(snap$consolidated) > 0) {
    core_by_slug_year <- setNames(
      n(snap$core$ca_works),
      paste(snap$core$tu9_slug, snap$core$year, sep = "|"))
    cons_rows <- snap$consolidated
    for (i in seq_len(nrow(cons_rows))) {
      key <- paste(cons_rows$tu9_slug[i], cons_rows$year[i], sep = "|")
      core_w <- core_by_slug_year[key]
      if (!is.na(core_w) && core_w > n(cons_rows$ca_works[i]))
        add("core ca_works exceeds consolidated for %s year %s", cons_rows$tu9_slug[i], cons_rows$year[i])
    }
  }

  # Core headline-period totals must equal the sum of their yearly Core values.
  ref_year <- as.integer(format(as.Date(snapshot_date), "%Y")) - 1L
  period_years <- seq(period_start, ref_year)
  for (slug in expected) {
    core_slug <- snap$core[snap$core$tu9_slug == slug, ]
    if (nrow(core_slug) == 0) next
    p <- core_slug[core_slug$year %in% period_years, ]
    if (nrow(p) > 0) {
      cm <- snap$core_members[[slug]]
      if (!is.null(cm)) {
        expected_works <- sum(n(p$ca_works), na.rm = TRUE)
        expected_share <- if (expected_works > 0)
          round(sum(n(p$ca_oa_works), na.rm = TRUE) / expected_works, 4) else NA_real_
        if (!isTRUE(all.equal(expected_works, n(cm$ca_works_period))))
          add("core period works (%s) do not sum yearly values for %s", n(cm$ca_works_period), slug)
        if (!isTRUE(all.equal(expected_share, n(cm$ca_oa_share_period), na.rm = TRUE)))
          add("core period share does not match summed values for %s", slug)
      }
    }
  }

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

  # --- 8. guard rail against a collapse in coverage ------------------------
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
