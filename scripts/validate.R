#!/usr/bin/env Rscript
# Snapshot validation. `validate_snapshot()` is called by scripts/fetch.R after
# everything has been fetched into memory and BEFORE anything is written to
# data/. If any check fails the run aborts and the published data is left
# untouched -- an incomplete or internally inconsistent snapshot is never
# published (fail-loud).
#
# All problems are collected and reported together, so one run tells you
# everything that is wrong rather than one issue at a time.

# Every published numeric field must be present and finite. NA is never a
# legitimate published value: it means an upstream field was absent and the row
# was assembled anyway. Shared by every table so there is one implementation of
# "this column must be a real number".
require_num_cols <- function(d, cols, add, label, min_value = 0) {
  if (is.null(d) || nrow(d) == 0) return(invisible(NULL))
  n <- function(x) suppressWarnings(as.numeric(x))
  for (col in cols) {
    if (!(col %in% names(d))) {
      add("%s: required column %s is absent", label, col)
      next
    }
    v <- n(d[[col]])
    bad <- which(is.na(v) | !is.finite(v))
    if (length(bad) > 0)
      add("%s: %s is missing or non-finite in %d row(s)", label, col, length(bad))
    low <- which(!is.na(v) & is.finite(v) & v < min_value)
    if (length(low) > 0)
      add("%s: %s is below %s in %d row(s)", label, col, min_value, length(low))
  }
  invisible(NULL)
}

# The numeric fields every metrics row must carry, whatever its age. This list is
# the single definition of the metrics numeric contract: the current snapshot and
# the published history both validate against it, so the two cannot drift apart
# -- which is exactly how two_yr_mean_citedness and the OA numerators ended up
# checked for current rows and unchecked for historical ones.
METRICS_REQUIRED_NUMERIC <- c(
  "works_count", "works_count_incl_xpac", "works_count_lineage_incl_xpac",
  "cited_by_count", "h_index", "i10_index", "two_yr_mean_citedness",
  "ca_works_ref", "ca_oa_works_ref", "ca_doaj_works_ref",
  "ca_works_period", "ca_oa_works_period",
  "ref_year", "period_start", "period_end")

# The numeric, ordering and arithmetic contract for ANY row of metrics.csv,
# current or historical. `add` is the caller's issue collector; `prefix` labels
# the rows being checked (e.g. "metrics.csv 2026-07-18: ").
metrics_row_invariants <- function(m, add, prefix = "") {
  if (is.null(m) || nrow(m) == 0) return(invisible(NULL))
  n <- function(x) suppressWarnings(as.numeric(x))
  slugs <- function(i) paste(m$slug[i], collapse = ", ")

  require_num_cols(m, METRICS_REQUIRED_NUMERIC, add,
                   label = if (nzchar(prefix)) sub(":\\s*$", "", prefix) else "metrics")
  if (any(n(m$works_count) <= 0, na.rm = TRUE))
    add("%snon-positive works_count for: %s", prefix,
        slugs(which(n(m$works_count) <= 0)))
  # XPAC-excluded can never exceed the XPAC-inclusive entity count. This is the
  # invariant that catches a cross-definition fallback slipping back in.
  bad_xpac <- which(n(m$works_count) > n(m$works_count_incl_xpac))
  if (length(bad_xpac) > 0)
    add("%sworks_count exceeds works_count_incl_xpac for: %s", prefix, slugs(bad_xpac))
  # Widening the lens can only add works: the same id including XPAC cannot
  # exceed that id PLUS its lineage, also including XPAC.
  bad_lin <- which(n(m$works_count_incl_xpac) > n(m$works_count_lineage_incl_xpac))
  if (length(bad_lin) > 0)
    add("%sworks_count_incl_xpac exceeds works_count_lineage_incl_xpac for: %s",
        prefix, slugs(bad_lin))
  # The period window contains the reference year, so it cannot be smaller.
  bad_period <- which(n(m$ca_works_period) < n(m$ca_works_ref))
  if (length(bad_period) > 0)
    add("%sca_works_period < ca_works_ref for: %s", prefix, slugs(bad_period))

  # Numerator <= denominator, and each published share is the quotient it claims
  # to be. These are properties of a single metrics row, so they hold for the
  # history too.
  for (tri in list(c("ca_oa_works_ref", "ca_works_ref", "ca_oa_share_ref"),
                   c("ca_oa_works_period", "ca_works_period", "ca_oa_share_period"),
                   c("ca_doaj_works_ref", "ca_works_ref", "ca_doaj_share_ref"))) {
    if (!all(tri %in% names(m))) next
    num <- n(m[[tri[1]]]); den <- n(m[[tri[2]]]); sh <- n(m[[tri[3]]])
    over <- which(!is.na(num) & !is.na(den) & num > den)
    if (length(over) > 0)
      add("%s%s exceeds %s for: %s", prefix, tri[1], tri[2], slugs(over))
    # A share may be blank ONLY where there is nothing to divide by -- the
    # project's existing policy, kept as it stands. Requiring it wherever the
    # denominator is positive is the half that was missing: every check below
    # skips NA, so a blank share with real works underneath passed all of them.
    gone <- which(!is.na(den) & den > 0 & (is.na(sh) | !is.finite(sh)))
    if (length(gone) > 0)
      add("%s%s is missing or non-finite where %s > 0 for: %s", prefix, tri[3],
          tri[2], slugs(gone))
    expect <- ifelse(!is.na(den) & den > 0, round(num / den, 4), NA_real_)
    off <- which(!is.na(sh) & !is.na(expect) & abs(sh - expect) > 1e-9)
    if (length(off) > 0)
      add("%s%s does not match round(%s/%s, 4) for: %s", prefix, tri[3], tri[1],
          tri[2], slugs(off))
  }
  invisible(NULL)
}

# Required products for every configured institution. The consolidated view is
# required for ALL of them: a university without Leiden `component` affiliates
# consolidates to itself, which is its correct consolidated value, and producing
# it keeps the alliance views summable over the same universities. The member set
# is checked against the Leiden mapping rather than merely being present. Both
# CWTS Core readings (primary venue and any location) share that member set.
validate_snapshot <- function(snap, inst, leiden_comp, snapshot_date,
                              prev_metrics = NULL, guard_threshold = 0.20,
                              force = FALSE, period_start = NULL) {
  if (is.null(period_start)) period_start <- 2020
  issues <- character()
  add <- function(...) issues <<- c(issues, sprintf(...))

  expected <- sort(inst$slug)
  m <- snap$metrics
  n <- function(x) suppressWarnings(as.numeric(x))

  # Every published numeric field must be present and finite. NA is never a
  # legitimate published value: it means an upstream field was absent and the row
  # was assembled anyway. openalex_metrics() and openalex_counts_by_year() turn a
  # missing entity field into NA to keep the row shape stable, so without this
  # the shape survives and the content quietly does not.
  require_num <- function(d, cols, label, min_value = 0)
    require_num_cols(d, cols, add, label, min_value)
  # The reference year is the latest complete calendar year, as in fetch.R.
  ref_year <- as.integer(format(as.Date(snapshot_date), "%Y")) - 1L

  # --- 0. fetch failures ---------------------------------------------------
  if (length(snap$failures) > 0) {
    for (f in snap$failures) add("fetch failed: %s", f)
  }

  # --- 1. exact institution set -------------------------------------------
  # Check duplicates on the RAW slug vector: de-duplicating first would make the
  # duplicate test unfalsifiable.
  got_raw <- snap$metrics$slug
  got <- sort(unique(got_raw))
  missing <- setdiff(expected, got)
  unexpected <- setdiff(got, expected)
  if (length(missing) > 0)
    add("missing institutions in metrics: %s", paste(missing, collapse = ", "))
  if (length(unexpected) > 0)
    add("unexpected institutions in metrics: %s", paste(unexpected, collapse = ", "))
  if (anyDuplicated(got_raw) > 0)
    add("duplicate institution rows in metrics: %s",
        paste(unique(got_raw[duplicated(got_raw)]), collapse = ", "))
  # The identity columns are what every page and every download show beside the
  # figures. Coverage was checked, the identities were not -- a name or id that
  # had drifted from the configuration would misattribute the whole row.
  for (i in seq_len(nrow(m))) {
    cfg <- inst[inst$slug == m$slug[i], , drop = FALSE]
    if (nrow(cfg) != 1) next  # already reported as a coverage problem
    for (f in c("name", "openalex_id", "ror_id")) {
      if (!identical(as.character(m[[f]][i]), as.character(cfg[[f]])))
        add("metrics %s for %s is '%s', the configuration says '%s'",
            f, m$slug[i], m[[f]][i], cfg[[f]])
    }
  }

  # --- 2. mandatory products, their shape, and per-institution coverage -----
  # Shape first. Every value-level check below indexes a column by name, and in R
  # a missing column yields NULL, so the comparison quietly does nothing instead
  # of failing: dropping ror_id or the DOAJ columns entirely used to pass.
  schemas <- list(
    metrics = c("snapshot_date", "slug", "name", "openalex_id", "ror_id",
                "works_count", "works_count_incl_xpac",
                "works_count_lineage_incl_xpac", "cited_by_count", "h_index",
                "i10_index", "two_yr_mean_citedness", "ref_year",
                "ca_works_ref", "ca_oa_works_ref", "ca_oa_share_ref",
                "ca_doaj_works_ref", "ca_doaj_share_ref",
                "period_start", "period_end", "ca_works_period",
                "ca_oa_works_period", "ca_oa_share_period"),
    counts_by_year = c("snapshot_date", "slug", "year", "works_count",
                       "works_count_incl_xpac", "works_count_lineage_incl_xpac",
                       "cited_by_count"),
    ca_oa_by_year = c("snapshot_date", "slug", "year", "ca_works", "ca_oa_works",
                      "ca_oa_share", "ca_doaj_works", "ca_doaj_share"),
    ca_oa_status = c("snapshot_date", "slug", "year", "oa_status", "ca_works"),
    consolidated = c("snapshot_date", "tu9_slug", "university_name", "n_members",
                     "year", "ca_works", "ca_oa_works", "ca_oa_share"),
    core = c("snapshot_date", "tu9_slug", "university_name", "n_members",
             "year", "ca_works", "ca_oa_works", "ca_oa_share"),
    core_any = c("snapshot_date", "tu9_slug", "university_name", "n_members",
                 "year", "ca_works", "ca_oa_works", "ca_oa_share"))
  for (nm in names(schemas)) {
    d <- snap[[nm]]
    if (is.null(d)) { add("%s is absent from the snapshot", nm); next }
    gone <- setdiff(schemas[[nm]], names(d))
    if (length(gone) > 0)
      add("%s lacks required column(s): %s", nm, paste(gone, collapse = ", "))
  }

  need <- list(counts_by_year = snap$counts_by_year,
               ca_oa_by_year  = snap$ca_oa_by_year,
               ca_oa_status   = snap$ca_oa_status,
               core           = snap$core,
               core_any       = snap$core_any)
  for (nm in names(need)) {
    d <- need[[nm]]
    have <- if (is.null(d) || nrow(d) == 0) {
      character()
    } else if (nm %in% c("core", "core_any")) {
      unique(d$tu9_slug)
    } else {
      unique(d$slug)
    }
    gone <- setdiff(expected, have)
    if (length(gone) > 0)
      add("%s missing for: %s", nm, paste(gone, collapse = ", "))
  }
  for (slug in expected) {
    if (is.null(snap$entities[[slug]]))
      add("raw entity missing for: %s", slug)
  }

  # --- 3. consolidated view present for every university, member set correct --
  # Presence is required for all nine so the alliance totals stay summable over
  # the same set in every view. The member-set check keeps the Leiden mapping
  # authoritative: a university without components must consolidate to itself.
  cons_got <- if (nrow(snap$consolidated) > 0) sort(unique(snap$consolidated$tu9_slug)) else character()
  missing_cons <- setdiff(expected, cons_got)
  unexpected_cons <- setdiff(cons_got, expected)
  if (length(missing_cons) > 0)
    add("consolidated view missing for: %s", paste(missing_cons, collapse = ", "))
  if (length(unexpected_cons) > 0)
    add("unexpected universities in consolidated view: %s",
        paste(unexpected_cons, collapse = ", "))
  for (slug in expected) {
    cons_mem <- snap$cons_members[[slug]]$members
    if (is.null(cons_mem)) {
      add("consolidated member list missing for: %s", slug)
      next
    }
    want <- unique(c(openalex_bare(inst$openalex_id[inst$slug == slug]),
                     openalex_bare(leiden_comp$affiliated_openalex_id[
                       leiden_comp$tu9_slug == slug])))
    if (!identical(sort(cons_mem), sort(want)))
      add("consolidated member set for %s does not match the Leiden mapping", slug)
  }

  # --- 4. both Core views are present for every university ------------------
  # The primary-venue and any-location Core readings each cover all nine, so the
  # four alliance views stay summable over the same universities.
  for (view in c("core", "core_any")) {
    got <- if (nrow(snap[[view]]) > 0) sort(unique(snap[[view]]$tu9_slug)) else character()
    missing_v <- setdiff(expected, got)
    unexpected_v <- setdiff(got, expected)
    if (length(missing_v) > 0)
      add("%s view missing for: %s", view, paste(missing_v, collapse = ", "))
    if (length(unexpected_v) > 0)
      add("unexpected universities in %s view: %s", view, paste(unexpected_v, collapse = ", "))
  }

  # --- 5. both Core member sets equal the consolidated member set ------------
  # All three cover every university, so this holds unconditionally: each Core
  # reading is the consolidated set with a source filter, never a different set.
  for (view in c("core", "core_any")) {
    members_of_view <- snap[[paste0(view, "_members")]]
    for (slug in expected) {
      view_mem <- members_of_view[[slug]]$members
      cons_mem <- snap$cons_members[[slug]]$members
      if (is.null(view_mem)) {
        add("%s member list missing for: %s", view, slug)
        next
      }
      if (is.null(cons_mem)) next  # already reported by check 3
      if (!identical(sort(view_mem), sort(cons_mem)))
        add("%s/cons member mismatch for %s", view, slug)
    }
  }

  # --- 6. the reference year is present in every view ----------------------
  # Presence of *some* rows is not enough: every headline figure on the site is
  # either the reference year or the period ending in it. A view that came back
  # without that year would leave ca_works_ref as NA, which the arithmetic
  # checks skip and the OA-status cross-check used to skip explicitly -- so an
  # incomplete snapshot could be published with blanks where the headline
  # belongs. Require exactly one row: none means missing, more than one means
  # the year was fetched or bound twice.
  ref_views <- list(ca_oa_by_year = list(d = snap$ca_oa_by_year, key = "slug"),
                    consolidated  = list(d = snap$consolidated,  key = "tu9_slug"),
                    core          = list(d = snap$core,          key = "tu9_slug"),
                    core_any      = list(d = snap$core_any,      key = "tu9_slug"))
  for (nm in names(ref_views)) {
    d <- ref_views[[nm]]$d
    key <- ref_views[[nm]]$key
    if (is.null(d) || nrow(d) == 0) next  # absence already reported by 2/3/4
    for (slug in expected) {
      k <- sum(d[[key]] == slug & n(d$year) == ref_year, na.rm = TRUE)
      if (k != 1)
        add("%s: expected exactly 1 row for %s in reference year %d, found %d",
            nm, slug, ref_year, k)
    }
  }
  # Every output table has a composite key that must identify a row uniquely.
  # Check 6 above pins this down for the reference year only; a repeated row in
  # any other year is just as wrong and worse to spot, because period_ca() sums
  # the window and would silently inflate the headline works figure. The group
  # reader rejects duplicate keys at the source; this holds for the tables as
  # assembled, including the per-slug binding the reader never sees.
  keys <- list(
    counts_by_year = c("slug", "year"),
    ca_oa_by_year  = c("slug", "year"),
    ca_oa_status   = c("slug", "year", "oa_status"),
    consolidated   = c("tu9_slug", "year"),
    core           = c("tu9_slug", "year"),
    core_any       = c("tu9_slug", "year"))
  for (nm in names(keys)) {
    d <- snap[[nm]]
    if (is.null(d) || nrow(d) == 0) next
    k <- keys[[nm]]
    if (!all(k %in% names(d))) {
      add("%s: expected key column(s) %s are absent", nm, paste(k, collapse = ", "))
      next
    }
    id <- do.call(paste, c(lapply(k, function(x) as.character(d[[x]])), sep = "\r"))
    dup <- unique(id[duplicated(id)])
    if (length(dup) > 0)
      add("%s: %d duplicate row(s) for key (%s): %s", nm, length(dup),
          paste(k, collapse = ", "),
          paste(gsub("\r", "/", utils::head(dup, 5)), collapse = "; "))
  }

  # No view may carry a publication year beyond the snapshot year. OpenAlex has
  # mis-dated records (a 2035 stamp turned up at RWTH); the works queries always
  # bounded the window, the corresponding-author queries did not, so the two
  # paths disagreed. Assert the bound rather than trusting each call site.
  snap_year <- as.integer(format(as.Date(snapshot_date), "%Y"))
  for (nm in c("counts_by_year", "ca_oa_by_year", "consolidated", "core", "core_any")) {
    d <- snap[[nm]]
    if (is.null(d) || nrow(d) == 0) next
    ahead <- sort(unique(n(d$year)[n(d$year) > snap_year]))
    if (length(ahead) > 0)
      add("%s: publication year(s) beyond the snapshot year %d: %s",
          nm, snap_year, paste(ahead, collapse = ", "))
  }

  # ... and the headline figures derived from it must be populated.
  for (i in seq_len(nrow(m))) {
    w <- n(m$ca_works_ref[i]); p <- n(m$ca_works_period[i])
    if (is.na(w)) add("ca_works_ref is missing for %s", m$slug[i])
    if (is.na(p)) add("ca_works_period is missing for %s", m$slug[i])
    # A share may only be blank where there is nothing to divide by.
    if (!is.na(w) && w > 0 && is.na(n(m$ca_oa_share_ref[i])))
      add("ca_oa_share_ref is missing for %s", m$slug[i])
    if (!is.na(p) && p > 0 && is.na(n(m$ca_oa_share_period[i])))
      add("ca_oa_share_period is missing for %s", m$slug[i])
    if (!is.na(w) && w > 0 && is.na(n(m$ca_doaj_share_ref[i])))
      add("ca_doaj_share_ref is missing for %s", m$slug[i])
  }

  # --- 7. one consistent snapshot_date everywhere --------------------------
  for (nm in c("metrics", "counts_by_year", "ca_oa_by_year", "ca_oa_status",
               "consolidated", "core", "core_any")) {
    d <- snap[[nm]]
    if (is.null(d) || nrow(d) == 0) next
    bad <- setdiff(unique(as.character(d$snapshot_date)), snapshot_date)
    if (length(bad) > 0)
      add("%s carries foreign snapshot_date(s): %s", nm, paste(bad, collapse = ", "))
  }

  # --- 8. numeric invariants ----------------------------------------------
  # Entity-derived fields first: these come straight from the OpenAlex entity and
  # were previously only ever compared against each other, so a degraded entity
  # could publish blanks that every comparison then skipped.
  # (The metrics fields themselves are covered by metrics_row_invariants() below,
  # which validates METRICS_REQUIRED_NUMERIC -- the one list both the current
  # snapshot and the published history use.)
  require_num(snap$counts_by_year,
              c("year", "works_count", "works_count_incl_xpac",
                "works_count_lineage_incl_xpac", "cited_by_count"),
              "counts_by_year")
  require_num(snap$ca_oa_status, c("year", "ca_works"), "ca_oa_status")
  # A publication year below this is a parsing accident, not a record.
  for (nm in c("counts_by_year", "ca_oa_by_year", "consolidated", "core", "core_any")) {
    d <- snap[[nm]]
    if (is.null(d) || nrow(d) == 0) next
    early <- sort(unique(n(d$year)[!is.na(n(d$year)) & n(d$year) < 1800]))
    if (length(early) > 0)
      add("%s: implausible publication year(s): %s", nm,
          paste(early, collapse = ", "))
  }

  metrics_row_invariants(m, add)

  # OA numerator/denominator and share arithmetic, in all OA tables.
  check_oa <- function(d, label) {
    if (is.null(d) || nrow(d) == 0) return(invisible(NULL))
    w <- n(d$ca_works); o <- n(d$ca_oa_works); s <- n(d$ca_oa_share)
    # A blank count or share must never be published. Every check below skips NA
    # (via na.rm or an explicit !is.na guard), so without this a row of blanks
    # would pass all of them -- and period_ca() sums with na.rm = TRUE, so the
    # totals would come out quietly understated rather than empty. The group
    # reader now rejects such rows at the source; this catches any other route.
    blank <- which(is.na(w) | is.na(o) | (!is.na(w) & w > 0 & is.na(s)))
    if (length(blank) > 0)
      add("%s: missing ca_works/ca_oa_works/ca_oa_share in %d row(s)",
          label, length(blank))
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
  # DOAJ is a subset of the corresponding-author works it is counted from.
  if (!is.null(snap$ca_oa_by_year) && nrow(snap$ca_oa_by_year) > 0) {
    d <- snap$ca_oa_by_year
    blank_dj <- which(is.na(n(d$ca_doaj_works)))
    if (length(blank_dj) > 0)
      add("ca_oa_by_year: ca_doaj_works is missing in %d row(s)", length(blank_dj))
    bad_dj <- which(n(d$ca_doaj_works) > n(d$ca_works))
    if (length(bad_dj) > 0)
      add("ca_oa_by_year: ca_doaj_works exceeds ca_works in %d row(s)", length(bad_dj))
  }
  # Per-year lens ordering, but only between the two figures that come from the
  # LIVE works API: widening id -> lineage can only add works. The middle column
  # (works_count_incl_xpac) is the institution ENTITY's precomputed counts_by_year,
  # which lags the live API -- for the current, still-growing year the live
  # XPAC-excluded count can legitimately exceed the entity's XPAC-inclusive one,
  # so comparing across those two sources would fail on a data property rather
  # than a defect.
  cb <- snap$counts_by_year
  if (!is.null(cb) && nrow(cb) > 0 &&
      "works_count_lineage_incl_xpac" %in% names(cb)) {
    bad_lens <- which(n(cb$works_count) > n(cb$works_count_lineage_incl_xpac))
    if (length(bad_lens) > 0)
      add("counts_by_year: id works exceed lineage works in %d row(s) (first: %s %s)",
          length(bad_lens), cb$slug[bad_lens[1]], cb$year[bad_lens[1]])
  }
  check_oa(snap$consolidated, "consolidated_ca_oa_by_year")
  check_oa(snap$core, "leiden_core_ca_oa_by_year")
  check_oa(snap$core_any, "leiden_core_any_location_ca_oa_by_year")

  # Nested subset invariants across the Core readings and the consolidated view.
  # Every filter genuinely narrows the previous one, over the same member set:
  #   primary-venue Core (primary_location.source.is_core:true)
  #     is a subset of
  #   any-location Core  (locations.source.is_core:true -- the primary location
  #     is one of the locations, so any work with a Core primary venue also has a
  #     Core location)
  #     is a subset of
  #   the unconstrained consolidated view.
  # is_oa:true narrows numerator and denominator identically, so the relationship
  # holds for the OA-works count as well as the works count. Checked per
  # (university, year); the error names both the affected view and institution.
  subset_le <- function(inner, inner_label, outer, outer_label) {
    if (is.null(inner) || nrow(inner) == 0 ||
        is.null(outer) || nrow(outer) == 0) return(invisible(NULL))
    ub_w  <- setNames(n(outer$ca_works),    paste(outer$tu9_slug, outer$year, sep = "|"))
    ub_oa <- setNames(n(outer$ca_oa_works), paste(outer$tu9_slug, outer$year, sep = "|"))
    for (i in seq_len(nrow(inner))) {
      key <- paste(inner$tu9_slug[i], inner$year[i], sep = "|")
      if (!(key %in% names(ub_w))) {
        add("%s row for %s year %s has no %s counterpart to check against",
            inner_label, inner$tu9_slug[i], inner$year[i], outer_label)
        next
      }
      if (n(inner$ca_works[i]) > ub_w[[key]])
        add("%s ca_works (%s) exceeds %s ca_works (%s) for %s year %s",
            inner_label, inner$ca_works[i], outer_label, ub_w[[key]],
            inner$tu9_slug[i], inner$year[i])
      if (n(inner$ca_oa_works[i]) > ub_oa[[key]])
        add("%s ca_oa_works (%s) exceeds %s ca_oa_works (%s) for %s year %s",
            inner_label, inner$ca_oa_works[i], outer_label, ub_oa[[key]],
            inner$tu9_slug[i], inner$year[i])
    }
  }
  subset_le(snap$core,     "primary-venue Core", snap$core_any,     "any-location Core")
  subset_le(snap$core_any, "any-location Core",  snap$consolidated, "consolidated")

  # Headline period totals must equal the sum of the yearly values they are
  # derived from -- in every view, not only in Core, which was the only one
  # checked. period_ca() sums with na.rm = TRUE, so a dropped year surfaces as a
  # quietly smaller headline rather than as an error; this is what makes that
  # visible. The single-institution headline lives in metrics, the other two in
  # the member summaries that also feed meta.json.
  period_years <- seq(period_start, ref_year)
  period_views <- list(
    list(label = "single-institution", rows = snap$ca_oa_by_year, key = "slug",
         works = function(s) n(m$ca_works_period[m$slug == s]),
         oa_works = function(s) n(m$ca_oa_works_period[m$slug == s]),
         share = function(s) n(m$ca_oa_share_period[m$slug == s])),
    list(label = "consolidated", rows = snap$consolidated, key = "tu9_slug",
         works = function(s) n(snap$cons_members[[s]]$ca_works_period),
         oa_works = NULL,  # not published for this view
         share = function(s) n(snap$cons_members[[s]]$ca_oa_share_period)),
    list(label = "core", rows = snap$core, key = "tu9_slug",
         works = function(s) n(snap$core_members[[s]]$ca_works_period),
         oa_works = NULL,
         share = function(s) n(snap$core_members[[s]]$ca_oa_share_period)),
    list(label = "core_any", rows = snap$core_any, key = "tu9_slug",
         works = function(s) n(snap$core_any_members[[s]]$ca_works_period),
         oa_works = NULL,
         share = function(s) n(snap$core_any_members[[s]]$ca_oa_share_period)))
  for (v in period_views) {
    d <- v$rows
    if (is.null(d) || nrow(d) == 0) next
    for (slug in expected) {
      p <- d[d[[v$key]] == slug & n(d$year) %in% period_years, , drop = FALSE]
      if (nrow(p) == 0) next
      want_w <- sum(n(p$ca_works), na.rm = TRUE)
      want_o <- sum(n(p$ca_oa_works), na.rm = TRUE)
      want_s <- if (want_w > 0) round(want_o / want_w, 4) else NA_real_
      got_w <- v$works(slug); got_s <- v$share(slug)
      if (length(got_w) != 1 || !isTRUE(all.equal(want_w, got_w)))
        add("%s: period works for %s do not sum the yearly values (headline %s, sum %s)",
            v$label, slug, paste(got_w, collapse = "/"), want_w)
      if (length(got_s) != 1 || !isTRUE(all.equal(want_s, got_s)))
        add("%s: period share for %s does not match the summed yearly values",
            v$label, slug)
      # The OA numerator is published beside the denominator and the share, but
      # was itself derived from nothing: checking works and share left the middle
      # column free to be any number at all.
      if (!is.null(v$oa_works)) {
        got_o <- v$oa_works(slug)
        if (length(got_o) != 1 || !isTRUE(all.equal(want_o, got_o)))
          add("%s: period OA works for %s do not sum the yearly values (headline %s, sum %s)",
              v$label, slug, paste(got_o, collapse = "/"), want_o)
      }
    }
  }

  # The reference-year headline must equal the reference-year row it is taken
  # from. metrics and the yearly table are written from the same fetch, but
  # nothing tied them together, so a slip in the derivation would show up on the
  # site as two different numbers for the same thing.
  if (!is.null(snap$ca_oa_by_year) && nrow(snap$ca_oa_by_year) > 0) {
    oy <- snap$ca_oa_by_year
    for (i in seq_len(nrow(m))) {
      slug <- m$slug[i]
      r <- oy[oy$slug == slug & n(oy$year) == n(m$ref_year[i]), , drop = FALSE]
      if (nrow(r) != 1) next  # reported by check 6
      # The DOAJ pair rides in the same row and is shown on the same page, but was
      # only ever checked for internal arithmetic -- the yearly row and the
      # headline could disagree and both remain self-consistent.
      for (pair in list(c("ca_works_ref", "ca_works"),
                        c("ca_oa_works_ref", "ca_oa_works"),
                        c("ca_oa_share_ref", "ca_oa_share"),
                        c("ca_doaj_works_ref", "ca_doaj_works"),
                        c("ca_doaj_share_ref", "ca_doaj_share"))) {
        if (!isTRUE(all.equal(n(m[[pair[1]]][i]), n(r[[pair[2]]]))))
          add("metrics %s (%s) disagrees with ca_oa_by_year %s (%s) for %s",
              pair[1], m[[pair[1]]][i], pair[2], r[[pair[2]]], slug)
      }
    }
  }

  # The same tie for the other two views. Their reference-year headlines live in
  # the member summaries -- which is what meta.json publishes and what every
  # institution page shows -- and nothing derived them from the yearly rows
  # underneath. Only the period figures were tied down, so a consolidated
  # reference headline could be off by any amount and pass.
  ref_headlines <- list(
    list(label = "consolidated", rows = snap$consolidated, sum = snap$cons_members),
    list(label = "core",         rows = snap$core,         sum = snap$core_members),
    list(label = "core_any",     rows = snap$core_any,     sum = snap$core_any_members))
  for (v in ref_headlines) {
    d <- v$rows
    if (is.null(d) || nrow(d) == 0) next
    for (slug in expected) {
      r <- d[d$tu9_slug == slug & n(d$year) == ref_year, , drop = FALSE]
      if (nrow(r) != 1) next  # reported by check 6
      s <- v$sum[[slug]]
      for (pair in list(c("ca_works_ref", "ca_works"),
                        c("ca_oa_share_ref", "ca_oa_share"))) {
        got <- n(s[[pair[1]]])
        if (length(got) != 1 || !isTRUE(all.equal(got, n(r[[pair[2]]]))))
          add("%s %s for %s (%s) disagrees with its reference-year row (%s)",
              v$label, pair[1], slug, paste(got, collapse = "/"), r[[pair[2]]])
      }
      # n_members is published beside those figures as the count of what was
      # OR-ed together. The member LIST is checked against the Leiden mapping;
      # the number shown next to it was never checked against that list.
      if (!is.null(s$members) &&
          !isTRUE(all.equal(n(s$n_members), length(s$members))))
        add("%s n_members for %s is %s, the member set has %d",
            v$label, slug, paste(s$n_members, collapse = "/"), length(s$members))
      if ("n_members" %in% names(d)) {
        rows_n <- unique(n(d$n_members[d$tu9_slug == slug]))
        if (!is.null(s$members) &&
            !identical(rows_n, as.numeric(length(s$members))))
          add("%s: n_members in the yearly rows for %s is %s, the member set has %d",
              v$label, slug, paste(rows_n, collapse = "/"), length(s$members))
      }
    }
  }

  # The reference year and the period window are published as columns, and every
  # check above that mentions them reads them back out of the same row. Anchor
  # them to the snapshot date instead, which is the only thing that determines
  # them: fetch.R derives ref_year from it and sets period_end to the same value.
  for (i in seq_len(nrow(m))) {
    for (fld in list(c("ref_year", ref_year), c("period_end", ref_year),
                     c("period_start", period_start))) {
      if (!isTRUE(all.equal(n(m[[fld[1]]][i]), as.numeric(fld[2]))))
        add("metrics %s for %s is %s, expected %s",
            fld[1], m$slug[i], m[[fld[1]]][i], fld[2])
    }
  }

  # The archived entity is the provenance behind the context indicators, and its
  # presence was the whole check -- an empty object satisfied it. Reconcile the
  # figures actually taken from it, so a truncated or substituted archive can no
  # longer sit under numbers it does not support.
  for (i in seq_len(nrow(m))) {
    slug <- m$slug[i]
    obj <- snap$entities[[slug]]
    if (is.null(obj)) next  # reported by check 2
    got_id <- openalex_bare(as.character(obj$id %||% ""))
    want_id <- openalex_bare(as.character(inst$openalex_id[inst$slug == slug]))
    if (!identical(got_id, want_id)) {
      add("raw entity for %s is id '%s', the configuration says '%s'",
          slug, got_id, want_id)
      next  # the wrong entity: comparing its figures would report noise
    }
    em <- openalex_metrics(obj)
    # The counts are integers and must match exactly. The mean-citedness is not:
    # write_json() serialises doubles at its default four significant digits, so
    # the ARCHIVE is a rounded copy of what metrics.csv carries at full
    # precision. Comparing exactly would fail on every institution, on a
    # serialisation property rather than a defect.
    for (pair in list(c("works_count_incl_xpac", "works_count"),
                      c("cited_by_count", "cited_by_count"),
                      c("h_index", "h_index"), c("i10_index", "i10_index"))) {
      if (!isTRUE(all.equal(n(m[[pair[1]]][i]), n(em[[pair[2]]]))))
        add("metrics %s for %s is %s, the archived entity says %s",
            pair[1], slug, m[[pair[1]]][i], paste(em[[pair[2]]], collapse = ""))
    }
    if (!isTRUE(all.equal(n(m$two_yr_mean_citedness[i]), n(em$two_yr_mean_cited),
                          tolerance = 1e-4)))
      add("metrics two_yr_mean_citedness for %s is %s, the archived entity says %s",
          slug, m$two_yr_mean_citedness[i], paste(em$two_yr_mean_cited, collapse = ""))

    # counts_by_year takes its year range, its XPAC-inclusive works column and
    # its citations straight from the same entity, so all three are
    # reconstructible. Only the XPAC-EXCLUDED and lineage columns come from the
    # unarchived works API; comparing those against the entity would be the
    # cross-definition substitution this pipeline exists to prevent.
    cb <- snap$counts_by_year
    if (!is.null(cb) && nrow(cb) > 0 && "year" %in% names(cb)) {
      # Anchored to the snapshot under validation, never to the process clock,
      # so a committed snapshot reconstructs identically whenever it is checked.
      ecb <- openalex_counts_by_year(obj, year_cap = snap_year)
      rows <- cb[cb$slug == slug, , drop = FALSE]
      if (!identical(sort(n(rows$year)), sort(n(ecb$year)))) {
        add("counts_by_year years for %s do not match the archived entity (%d vs %d row(s))",
            slug, nrow(rows), nrow(ecb))
      } else {
        j <- match(n(rows$year), n(ecb$year))
        for (pair in list(c("works_count_incl_xpac", "works_count"),
                          c("cited_by_count", "cited_by_count"))) {
          off <- which(n(rows[[pair[1]]]) != n(ecb[[pair[2]]])[j])
          if (length(off) > 0)
            add("counts_by_year %s for %s disagrees with the archived entity in %d year(s) (first: %s)",
                pair[1], slug, length(off), rows$year[off[1]])
        }
      }
    }
  }

  # The consolidated and Core products restate the university's name beside every
  # row. metrics.name was checked against the configuration; these were not, so
  # the same drift would show up unopposed on exactly the pages that carry the
  # consolidated headline.
  for (nm in c("consolidated", "core")) {
    d <- snap[[nm]]
    if (is.null(d) || nrow(d) == 0 || !("university_name" %in% names(d))) next
    for (slug in expected) {
      got <- unique(as.character(d$university_name[d$tu9_slug == slug]))
      want <- as.character(inst$name[inst$slug == slug])
      if (length(got) > 0 && !identical(got, want))
        add("%s university_name for %s is '%s', the configuration says '%s'",
            nm, slug, paste(got, collapse = "/"), want)
    }
  }

  # Consolidating ORs the university with its component affiliates, so it can
  # only add works -- the site states this as a property of the data. It is also
  # the invariant whose violation started this: the consolidated total once came
  # out below the single-institution total and nothing objected.
  if (!is.null(snap$consolidated) && nrow(snap$consolidated) > 0 &&
      !is.null(snap$ca_oa_by_year) && nrow(snap$ca_oa_by_year) > 0) {
    single <- setNames(n(snap$ca_oa_by_year$ca_works),
                       paste(snap$ca_oa_by_year$slug, snap$ca_oa_by_year$year, sep = "|"))
    for (i in seq_len(nrow(snap$consolidated))) {
      key <- paste(snap$consolidated$tu9_slug[i], snap$consolidated$year[i], sep = "|")
      s <- single[key]
      if (is.na(s)) {
        add("consolidated row for %s year %s has no single-institution counterpart",
            snap$consolidated$tu9_slug[i], snap$consolidated$year[i])
      } else if (n(snap$consolidated$ca_works[i]) < s) {
        add("consolidated ca_works (%s) is below the single-institution value (%s) for %s year %s",
            snap$consolidated$ca_works[i], s,
            snap$consolidated$tu9_slug[i], snap$consolidated$year[i])
      }
    }
  }

  # OA statuses are a closed vocabulary and the composition is fetched for the
  # reference year alone. An unknown category would silently join the totals.
  if (!is.null(snap$ca_oa_status) && nrow(snap$ca_oa_status) > 0) {
    allowed <- c("gold", "hybrid", "green", "bronze", "diamond", "closed")
    seen <- setdiff(unique(snap$ca_oa_status$oa_status), allowed)
    if (length(seen) > 0)
      add("ca_oa_status: unexpected category/categories: %s", paste(seen, collapse = ", "))
    off_year <- setdiff(unique(n(snap$ca_oa_status$year)), ref_year)
    if (length(off_year) > 0)
      add("ca_oa_status: row(s) for year(s) other than the reference year %d: %s",
          ref_year, paste(off_year, collapse = ", "))
  }

  # DOAJ counts ride along in the single-institution table; their share is
  # computed the same way as the OA share and was never checked.
  if (!is.null(snap$ca_oa_by_year) && nrow(snap$ca_oa_by_year) > 0) {
    d <- snap$ca_oa_by_year
    w <- n(d$ca_works); dj <- n(d$ca_doaj_works); sh <- n(d$ca_doaj_share)
    blank_sh <- which(!is.na(w) & w > 0 & is.na(sh))
    if (length(blank_sh) > 0)
      add("ca_oa_by_year: ca_doaj_share is missing in %d row(s)", length(blank_sh))
    if (any(dj < 0, na.rm = TRUE)) add("ca_oa_by_year: negative ca_doaj_works")
    expect <- ifelse(w > 0, round(dj / w, 4), NA_real_)
    off <- which(!is.na(sh) & !is.na(expect) & abs(sh - expect) > 1e-9)
    if (length(off) > 0)
      add("ca_oa_by_year: ca_doaj_share does not match round(ca_doaj_works/ca_works, 4) in %d row(s)",
          length(off))
  }

  # OA-status categories must add up to the reference-year CA works.
  if (nrow(snap$ca_oa_status) > 0) {
    tot <- tapply(n(snap$ca_oa_status$ca_works), snap$ca_oa_status$slug, sum)
    ref <- setNames(n(m$ca_works_ref), m$slug)
    for (slug in names(tot)) {
      # A slug with status rows but no metrics row is already reported above as
      # a missing institution; skip it rather than index out of bounds.
      if (!(slug %in% names(ref))) next
      if (!isTRUE(all.equal(unname(tot[[slug]]), unname(ref[[slug]]))))
        add("ca_oa_status total (%s) != ca_works_ref (%s) for %s",
            tot[[slug]], ref[[slug]], slug)
    }
  }

  # --- 9. guard rail against a collapse in coverage ------------------------
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
