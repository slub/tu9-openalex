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
# required for ALL of them: a university without Leiden `component` affiliates
# consolidates to itself, which is its correct consolidated value, and producing
# it keeps the three views summable over the same universities. The member set
# is checked against the Leiden mapping rather than merely being present.
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
  require_num <- function(d, cols, label, min_value = 0) {
    if (is.null(d) || nrow(d) == 0) return(invisible(NULL))
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
  }
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
               core           = snap$core)
  for (nm in names(need)) {
    d <- need[[nm]]
    have <- if (is.null(d) || nrow(d) == 0) {
      character()
    } else if (nm == "core") {
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

  # --- 4. core view is present for every university -------------------------
  core_got <- if (nrow(snap$core) > 0) sort(unique(snap$core$tu9_slug)) else character()
  missing_core <- setdiff(expected, core_got)
  unexpected_core <- setdiff(core_got, expected)
  if (length(missing_core) > 0)
    add("core view missing for: %s", paste(missing_core, collapse = ", "))
  if (length(unexpected_core) > 0)
    add("unexpected universities in core view: %s", paste(unexpected_core, collapse = ", "))

  # --- 5. core member set equals consolidated member set ---------------------
  # Both views now cover every university, so this holds unconditionally: core
  # is the consolidated set with a source filter, never a different set.
  for (slug in expected) {
    core_mem <- snap$core_members[[slug]]$members
    cons_mem <- snap$cons_members[[slug]]$members
    if (is.null(core_mem)) {
      add("core member list missing for: %s", slug)
      next
    }
    if (is.null(cons_mem)) next  # already reported by check 3
    if (!identical(sort(core_mem), sort(cons_mem)))
      add("core/cons member mismatch for %s", slug)
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
                    core          = list(d = snap$core,          key = "tu9_slug"))
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
    core           = c("tu9_slug", "year"))
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
  for (nm in c("counts_by_year", "ca_oa_by_year", "consolidated", "core")) {
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
               "consolidated", "core")) {
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
  require_num(m, c("works_count", "works_count_incl_xpac",
                   "works_count_lineage_incl_xpac", "cited_by_count",
                   "h_index", "i10_index", "two_yr_mean_citedness",
                   "ca_oa_works_ref", "ca_oa_works_period",
                   "ca_doaj_works_ref",
                   "ref_year", "period_start", "period_end"), "metrics")
  require_num(snap$counts_by_year,
              c("year", "works_count", "works_count_incl_xpac",
                "works_count_lineage_incl_xpac", "cited_by_count"),
              "counts_by_year")
  require_num(snap$ca_oa_status, c("year", "ca_works"), "ca_oa_status")
  # A publication year below this is a parsing accident, not a record.
  for (nm in c("counts_by_year", "ca_oa_by_year", "consolidated", "core")) {
    d <- snap[[nm]]
    if (is.null(d) || nrow(d) == 0) next
    early <- sort(unique(n(d$year)[!is.na(n(d$year)) & n(d$year) < 1800]))
    if (length(early) > 0)
      add("%s: implausible publication year(s): %s", nm,
          paste(early, collapse = ", "))
  }

  if (any(n(m$works_count) <= 0, na.rm = TRUE))
    add("non-positive works_count for: %s",
        paste(m$slug[n(m$works_count) <= 0], collapse = ", "))
  # XPAC-excluded can never exceed the XPAC-inclusive entity count. This is the
  # invariant that catches a cross-definition fallback slipping back in.
  bad_xpac <- which(n(m$works_count) > n(m$works_count_incl_xpac))
  if (length(bad_xpac) > 0)
    add("works_count exceeds works_count_incl_xpac for: %s",
        paste(m$slug[bad_xpac], collapse = ", "))
  # Widening the lens can only add works: the same id including XPAC cannot
  # exceed that id PLUS its lineage, also including XPAC.
  bad_lin <- which(n(m$works_count_incl_xpac) > n(m$works_count_lineage_incl_xpac))
  if (length(bad_lin) > 0)
    add("works_count_incl_xpac exceeds works_count_lineage_incl_xpac for: %s",
        paste(m$slug[bad_lin], collapse = ", "))
  # The period window contains the reference year, so it cannot be smaller.
  bad_period <- which(n(m$ca_works_period) < n(m$ca_works_ref))
  if (length(bad_period) > 0)
    add("ca_works_period < ca_works_ref for: %s",
        paste(m$slug[bad_period], collapse = ", "))

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

  # Core draws from the same member set as the consolidated view, so it can never
  # be larger than the set it is filtered out of. Consolidated now covers every
  # university, so it supplies the bound in all cases; the single-institution
  # fallback below is kept only so a missing consolidated row degrades to a
  # weaker check instead of skipping the comparison silently.
  if (nrow(snap$core) > 0) {
    upper <- setNames(n(snap$ca_oa_by_year$ca_works),
                      paste(snap$ca_oa_by_year$slug, snap$ca_oa_by_year$year, sep = "|"))
    source_of <- setNames(rep("single-institution", length(upper)), names(upper))
    if (nrow(snap$consolidated) > 0) {
      ck <- paste(snap$consolidated$tu9_slug, snap$consolidated$year, sep = "|")
      upper[ck] <- n(snap$consolidated$ca_works)
      source_of[ck] <- "consolidated"
    }
    for (i in seq_len(nrow(snap$core))) {
      key <- paste(snap$core$tu9_slug[i], snap$core$year[i], sep = "|")
      ub <- upper[key]
      if (is.na(ub)) {
        add("core row for %s year %s has no %s counterpart to check against",
            snap$core$tu9_slug[i], snap$core$year[i], "single-institution")
      } else if (n(snap$core$ca_works[i]) > ub) {
        add("core ca_works (%s) exceeds its %s upper bound (%s) for %s year %s",
            snap$core$ca_works[i], source_of[[key]], ub,
            snap$core$tu9_slug[i], snap$core$year[i])
      }
    }
  }

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
         share = function(s) n(m$ca_oa_share_period[m$slug == s])),
    list(label = "consolidated", rows = snap$consolidated, key = "tu9_slug",
         works = function(s) n(snap$cons_members[[s]]$ca_works_period),
         share = function(s) n(snap$cons_members[[s]]$ca_oa_share_period)),
    list(label = "core", rows = snap$core, key = "tu9_slug",
         works = function(s) n(snap$core_members[[s]]$ca_works_period),
         share = function(s) n(snap$core_members[[s]]$ca_oa_share_period)))
  for (v in period_views) {
    d <- v$rows
    if (is.null(d) || nrow(d) == 0) next
    for (slug in expected) {
      p <- d[d[[v$key]] == slug & n(d$year) %in% period_years, , drop = FALSE]
      if (nrow(p) == 0) next
      want_w <- sum(n(p$ca_works), na.rm = TRUE)
      want_s <- if (want_w > 0)
        round(sum(n(p$ca_oa_works), na.rm = TRUE) / want_w, 4) else NA_real_
      got_w <- v$works(slug); got_s <- v$share(slug)
      if (length(got_w) != 1 || !isTRUE(all.equal(want_w, got_w)))
        add("%s: period works for %s do not sum the yearly values (headline %s, sum %s)",
            v$label, slug, paste(got_w, collapse = "/"), want_w)
      if (length(got_s) != 1 || !isTRUE(all.equal(want_s, got_s)))
        add("%s: period share for %s does not match the summed yearly values",
            v$label, slug)
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
      for (pair in list(c("ca_works_ref", "ca_works"),
                        c("ca_oa_works_ref", "ca_oa_works"),
                        c("ca_oa_share_ref", "ca_oa_share"))) {
        if (!isTRUE(all.equal(n(m[[pair[1]]][i]), n(r[[pair[2]]]))))
          add("metrics %s (%s) disagrees with ca_oa_by_year %s (%s) for %s",
              pair[1], m[[pair[1]]][i], pair[2], r[[pair[2]]], slug)
      }
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
