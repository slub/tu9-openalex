# Helper functions for the OpenAlex Institutions API.
# Docs: https://developers.openalex.org/api-reference/institutions
# OpenAlex data is released under CC0.
#
# These helpers are deliberately small and dependency-light so that a
# non-developer can read them top to bottom. They mirror the shape of
# scripts/jct.R in the sibling tu9-jct-data repository.

suppressPackageStartupMessages({
  library(readr)
  library(jsonlite)
  library(curl)
})

# --- HTTP layer -------------------------------------------------------------
# Requests go through one status-aware helper so transient failures are retried
# and credentials never reach a log. Policy:
#   * retry on 429, 5xx and transport errors (timeout, connection reset)
#   * fail fast on other 4xx -- a bad filter will not fix itself
#   * bounded exponential backoff with jitter, honouring Retry-After
# The budget is deliberately small: the pipeline is fail-loud, so it is better
# to abort a run than to stall a scheduled job for many minutes.
OPENALEX_ATTEMPTS  <- 3L    # total tries per request
OPENALEX_MAX_WAIT  <- 30    # seconds of cumulative backoff per request
OPENALEX_TIMEOUT   <- 60    # seconds per individual request

# Remove the API key from anything we are about to print. Errors and warnings
# can echo the full request URL, which carries `api_key=` as a query parameter.
openalex_redact <- function(x) {
  x <- paste(as.character(x), collapse = " ")
  x <- gsub("api_key=[^&[:space:]]*", "api_key=<redacted>", x)
  key <- Sys.getenv("OPENALEX_API_KEY")
  if (nzchar(key)) x <- gsub(key, "<redacted>", x, fixed = TRUE)
  x
}

# Perform one GET with retries. Returns the response body as text, or NULL when
# the request could not be completed. `what` is a short human label used in
# messages so that no full URL (and hence no key) is ever logged.
openalex_get <- function(url, what) {
  waited <- 0
  for (attempt in seq_len(OPENALEX_ATTEMPTS)) {
    h <- curl::new_handle(timeout = OPENALEX_TIMEOUT, connecttimeout = 20,
                          useragent = paste0("tu9-openalex (", openalex_mailto(), ")"))
    res <- tryCatch(curl::curl_fetch_memory(url, handle = h),
                    error = function(e) structure(list(err = conditionMessage(e)),
                                                  class = "openalex_transport_error"))

    if (!inherits(res, "openalex_transport_error")) {
      if (res$status_code >= 200 && res$status_code < 300) {
        return(rawToChar(res$content))
      }
      # Permanent client errors: no point retrying.
      if (res$status_code >= 400 && res$status_code < 500 && res$status_code != 429) {
        message(sprintf("    %s: HTTP %d (permanent) -- %s", what, res$status_code,
                        openalex_redact(substr(rawToChar(res$content), 1, 200))))
        return(NULL)
      }
      retry_after <- suppressWarnings(as.numeric(
        curl::parse_headers_list(res$headers)[["retry-after"]]))
      reason <- sprintf("HTTP %d", res$status_code)
    } else {
      retry_after <- NA_real_
      reason <- openalex_redact(res$err)
    }

    if (attempt == OPENALEX_ATTEMPTS) {
      message(sprintf("    %s: giving up after %d attempts -- %s",
                      what, OPENALEX_ATTEMPTS, reason))
      return(NULL)
    }
    # Exponential backoff with jitter, capped by the remaining budget.
    wait <- min(2^attempt + stats::runif(1, 0, 1), OPENALEX_MAX_WAIT - waited)
    if (!is.na(retry_after)) wait <- min(max(wait, retry_after), OPENALEX_MAX_WAIT - waited)
    if (wait <= 0) {
      message(sprintf("    %s: backoff budget exhausted -- %s", what, reason))
      return(NULL)
    }
    message(sprintf("    %s: %s, retrying in %.1fs (attempt %d/%d)",
                    what, reason, wait, attempt, OPENALEX_ATTEMPTS))
    Sys.sleep(wait)
    waited <- waited + wait
  }
  NULL
}

# Contact e-mail for the OpenAlex "polite pool" (faster, more reliable). Set the
# OPENALEX_MAILTO environment variable to override; falls back to a repo contact.
openalex_mailto <- function() {
  m <- Sys.getenv("OPENALEX_MAILTO")
  if (nzchar(m)) m else "openalex@slub-dresden.de"
}

# Normalise an OpenAlex institution id to its bare short form, e.g.
# "https://openalex.org/I78650965" -> "I78650965".
openalex_bare <- function(x) {
  x <- trimws(x)
  sub("^https?://openalex\\.org/", "", x)
}

# Full API URL for a single institution entity (looked up by its short id).
# Adds the polite-pool `mailto` and, if OPENALEX_API_KEY is set, the (free)
# `api_key` (kept out of the repo; see the `secret` file / GitHub Actions).
openalex_institution_url <- function(id) {
  url <- paste0("https://api.openalex.org/institutions/", openalex_bare(id),
                "?mailto=", utils::URLencode(openalex_mailto(), reserved = TRUE))
  key <- Sys.getenv("OPENALEX_API_KEY")
  if (nzchar(key)) {
    url <- paste0(url, "&api_key=", utils::URLencode(key, reserved = TRUE))
  }
  url
}

# Fetch one institution entity as a parsed list. Returns NULL on any
# network/parse error so that one bad id never aborts the whole run (the
# guard rail in fetch.R catches systematic, large-scale failures instead).
openalex_institution_reader <- function(id) {
  txt <- openalex_get(openalex_institution_url(id),
                      paste0("institution ", openalex_bare(id)))
  if (is.null(txt)) return(NULL)
  tryCatch(
    {
      obj <- fromJSON(txt, simplifyVector = FALSE)
      if (is.null(obj$id)) stop("no id in response")
      obj
    },
    error = function(e) {
      message("    institution ", openalex_bare(id), ": unparsable response -- ",
              openalex_redact(conditionMessage(e)))
      NULL
    }
  )
}

# --- works aggregation (group_by) ------------------------------------------
# The OA metrics use OpenAlex's `group_by`, which returns server-side aggregated
# counts -- we never page through individual works. The denominator is the
# corresponding-author lens (`corresponding_institution_ids`), which matches the
# OpenAPC / transformative-agreement view of an institution's output.
# See https://blog.openalex.org/a-big-improvement-to-our-corresponding-author-data/

# Build a /works URL with a filter and a group_by dimension (+ polite pool and,
# if set, the api_key).
openalex_works_group_url <- function(filter, group_by, include_xpac = FALSE) {
  url <- paste0(
    "https://api.openalex.org/works",
    "?filter=", utils::URLencode(filter, reserved = TRUE),
    "&group_by=", utils::URLencode(group_by, reserved = TRUE),
    "&mailto=", utils::URLencode(openalex_mailto(), reserved = TRUE))
  if (include_xpac) url <- paste0(url, "&include_xpac=true")
  key <- Sys.getenv("OPENALEX_API_KEY")
  if (nzchar(key)) url <- paste0(url, "&api_key=", utils::URLencode(key, reserved = TRUE))
  url
}

# Run one grouped works query. Returns a data frame (key, count) or NULL on any
# network/parse error, so a single failed query never aborts the run. The total
# number of matching works (meta.count) is attached as attr(df, "total").
#
# XPAC note: every /works filter built here appends `is_xpac:false` EXPLICITLY,
# so the Expansion Pack (XPAC; ~190M lower-quality dataset/repository records
# added 11/2025) is excluded by our query, not merely by OpenAlex's current
# default. `is_xpac:false` needs no `include_xpac=true` and returns exactly the
# default result set, but stating it keeps the exclusion reproducible if that
# default ever changes. See
# https://developers.openalex.org/guides/key-concepts#xpac-expansion-pack
XPAC_EXCLUDE <- "is_xpac:false"

openalex_group_reader <- function(filter, group_by, include_xpac = FALSE) {
  # `filter` can carry institution ids but never the key, so it is safe context.
  what <- paste0("works group_by=", group_by)
  txt <- openalex_get(openalex_works_group_url(filter, group_by, include_xpac), what)
  if (is.null(txt)) return(NULL)
  tryCatch(
    {
      obj <- fromJSON(txt, simplifyVector = FALSE)
      g <- obj$group_by
      if (is.null(g)) stop("no group_by in response")
      df <- data.frame(
        key   = vapply(g, function(x) as.character(x$key %||% NA), character(1)),
        count = vapply(g, function(x) as.integer(x$count %||% NA), integer(1)),
        stringsAsFactors = FALSE
      )
      # `key` and `count` are required group fields. Carrying a missing one as NA
      # would make a degraded-but-successful response indistinguishable from a
      # legitimately absent group downstream: openalex_ca_oa_by_year() maps an
      # absent numerator year to 0, so an NA count would be published as "no OA
      # works" rather than as a failure. Reject the whole response instead --
      # openalex_get()'s caller then fails the institution, which is the
      # fail-loud behaviour the pipeline promises.
      # https://developers.openalex.org/guides/grouping
      bad <- which(is.na(df$key) | is.na(df$count))
      if (length(bad) > 0)
        stop(sprintf("%d of %d groups lack a key or count", length(bad), nrow(df)))
      # A group_by is a partition: one row per distinct key. A repeated key would
      # survive as two rows of the same year, which period_ca() sums -- so the
      # headline works figure would come out too high with nothing to show for
      # it. There is no reading under which a duplicate key is meaningful.
      dup <- unique(df$key[duplicated(df$key)])
      if (length(dup) > 0)
        stop(sprintf("duplicate group key(s): %s",
                     paste(utils::head(dup, 5), collapse = ", ")))
      attr(df, "total") <- as.integer(obj$meta$count %||% NA)
      df
    },
    error = function(e) {
      message("    ", what, ": unparsable response -- ",
              openalex_redact(conditionMessage(e)))
      NULL
    }
  )
}

# Total number of works matching a filter (meta.count only). `include_xpac`
# switches the Expansion Pack back ON, which is needed to reproduce the figure
# OpenAlex's own institution profile links to.
openalex_works_count <- function(filter, include_xpac = FALSE, what = "works count") {
  url <- paste0("https://api.openalex.org/works",
                "?filter=", utils::URLencode(filter, reserved = TRUE),
                "&per_page=1",
                "&mailto=", utils::URLencode(openalex_mailto(), reserved = TRUE))
  if (include_xpac) url <- paste0(url, "&include_xpac=true")
  key <- Sys.getenv("OPENALEX_API_KEY")
  if (nzchar(key)) url <- paste0(url, "&api_key=", utils::URLencode(key, reserved = TRUE))
  txt <- openalex_get(url, what)
  if (is.null(txt)) return(NA_integer_)
  tryCatch(as.integer(fromJSON(txt, simplifyVector = FALSE)$meta$count),
           error = function(e) {
             message("    ", what, ": unparsable response -- ",
                     openalex_redact(conditionMessage(e)))
             NA_integer_
           })
}

# Works attributed to an institution INCLUDING its OpenAlex lineage -- that is,
# its child institutions -- and including XPAC. This is the population OpenAlex's
# own institution profile links to, and it is carried purely so the figures here
# can be reconciled against what a visitor sees on openalex.org. Note that the
# lineage is OpenAlex's parent/child hierarchy, which is NOT the same set as the
# Leiden `component` affiliates: a university hospital linked only as `related`
# (e.g. Dresden) is outside the lineage but inside the Leiden consolidation.
openalex_works_lineage_total <- function(inst_id) {
  id <- openalex_bare(inst_id)
  openalex_works_count(paste0("authorships.institutions.lineage:", id),
                       include_xpac = TRUE,
                       what = paste0("lineage works ", id))
}

# XPAC-excluded works of an institution (all authorship positions), broken down
# by publication year. `inst_ids` may be one id or several (OR-ed). Returns
# list(total = <int>, by_year = data.frame(year, works_count)) or NULL. This is
# the entity `works_count` semantics minus the Expansion Pack, so we source the
# tracked works figures from here instead of the (XPAC-inclusive) entity.
openalex_works_by_year <- function(inst_ids) {
  ids <- paste(openalex_bare(inst_ids), collapse = "|")
  g <- openalex_group_reader(
    paste0("authorships.institutions.id:", ids, ",", XPAC_EXCLUDE),
    "publication_year")
  if (is.null(g)) return(NULL)
  yr <- suppressWarnings(as.integer(g$key))
  keep <- !is.na(yr) & yr <= openalex_year_cap()
  list(
    total = attr(g, "total"),
    by_year = data.frame(year = yr[keep], works_count = g$count[keep],
                         stringsAsFactors = FALSE)
  )
}

# Works by publication year INCLUDING the institution's OpenAlex lineage (its
# child institutions) and including XPAC -- the per-year counterpart of
# openalex_works_lineage_total(). Carried for reconciliation with openalex.org
# only; no metric on the site is derived from it.
# Returns list(total = <int>, by_year = data.frame(year, works_count)). The
# total comes from the same response's meta.count, so the headline figure and
# the per-year breakdown can never disagree -- and it costs one request, not two.
openalex_works_lineage_by_year <- function(inst_id) {
  id <- openalex_bare(inst_id)
  g <- openalex_group_reader(paste0("authorships.institutions.lineage:", id),
                             "publication_year", include_xpac = TRUE)
  if (is.null(g)) return(NULL)
  yr <- suppressWarnings(as.integer(g$key))
  keep <- !is.na(yr) & yr <= openalex_year_cap()
  list(total = attr(g, "total"),
       by_year = data.frame(year = yr[keep], works_count = g$count[keep],
                            stringsAsFactors = FALSE))
}

# Corresponding-author (CA) OA share by publication year, from `start_year`
# onward. `inst_ids` may be one id or several: several are OR-ed in the
# corresponding_institution_ids filter, so a work counts once if its
# corresponding author sits at ANY of them (used for the Leiden-consolidated
# view: university + its component affiliates). All figures are on the
# corresponding-author lens, hence the `ca_` prefix. Returns
# (year, ca_works, ca_oa_works, ca_oa_share) or NULL if either query fails.
#
# `extra_filter` is appended to both denominator and numerator filters. It lets
# the same CA/OA calculation be reused for the CWTS Core-source view without
# duplicating the arithmetic:
#   openalex_ca_oa_by_year(ids, start_year,
#                          extra_filter = "primary_location.source.is_core:true")
openalex_ca_oa_by_year <- function(inst_ids, start_year, extra_filter = NULL) {
  ids <- paste(openalex_bare(inst_ids), collapse = "|")
  base_filter <- paste0("corresponding_institution_ids:", ids, ",", XPAC_EXCLUDE)
  if (!is.null(extra_filter) && nzchar(extra_filter)) {
    base_filter <- paste0(base_filter, ",", extra_filter)
  }
  denom <- openalex_group_reader(base_filter, "publication_year")
  numer <- openalex_group_reader(
    paste0(base_filter, ",is_oa:true"),
    "publication_year")
  if (is.null(denom) || is.null(numer)) return(NULL)

  denom$year <- suppressWarnings(as.integer(denom$key))
  numer$year <- suppressWarnings(as.integer(numer$key))
  # Bound the window at both ends, as openalex_works_by_year() already does.
  # OpenAlex carries occasional mis-dated records -- one RWTH work is stamped
  # 2035 -- and without an upper bound they end up as stray rows in the yearly
  # tables and charts. They never reached the headline figures, which sum
  # [PERIOD_START, REF_YEAR], so the two code paths simply disagreed about what
  # a publication year may be.
  denom <- denom[!is.na(denom$year) & denom$year >= start_year &
                 denom$year <= openalex_year_cap(), ]
  if (nrow(denom) == 0) return(NULL)

  oa <- numer$count[match(denom$year, numer$year)]
  oa[is.na(oa)] <- 0L
  df <- data.frame(
    year        = denom$year,
    ca_works    = denom$count,
    ca_oa_works = as.integer(oa),
    stringsAsFactors = FALSE
  )
  df$ca_oa_share <- ifelse(df$ca_works > 0,
                           round(df$ca_oa_works / df$ca_works, 4), NA_real_)
  df[order(-df$year), , drop = FALSE]
}

# CWTS Core-source-filtered corresponding-author OA share by publication year.
# Uses the same member set as the Leiden-consolidated view but restricts works
# to sources on the CWTS Core sources allow-list. Two distinct readings of the
# same allow-list, computed as separate queries and never combined:
#
#   primary venue  -- primary_location.source.is_core:true: the work's PRIMARY
#                     venue is a Core source (the narrower reading);
#   any location   -- locations.source.is_core:true: AT LEAST ONE recorded
#                     location of the work is a Core source (the wider reading).
#
# Both share openalex_ca_oa_by_year()'s CA/OA arithmetic through extra_filter, so
# there is one implementation of the calculation and only the filter differs.
openalex_ca_oa_by_year_core <- function(inst_ids, start_year) {
  openalex_ca_oa_by_year(inst_ids, start_year,
                         extra_filter = "primary_location.source.is_core:true")
}

openalex_ca_oa_by_year_core_any_location <- function(inst_ids, start_year) {
  openalex_ca_oa_by_year(inst_ids, start_year,
                         extra_filter = "locations.source.is_core:true")
}

# Corresponding-author works published in DOAJ-listed journals, by publication
# year (primary_location.source.is_in_doaj). Kept out of the OA-status
# composition because it does not partition with those categories: every work has
# exactly one oa_status and they sum to the works total, while DOAJ listing is an
# independent yes/no cutting across them. Note this is NOT a level distinction --
# gold and diamond are journal properties too ("appeared in a fully open-access
# journal", diamond adding "without an APC"), which is why a DOAJ row would
# double-count rather than add a category.
openalex_ca_doaj_by_year <- function(inst_ids, start_year) {
  ids <- paste(openalex_bare(inst_ids), collapse = "|")
  g <- openalex_group_reader(
    paste0("corresponding_institution_ids:", ids,
           ",primary_location.source.is_in_doaj:true,", XPAC_EXCLUDE),
    "publication_year")
  if (is.null(g)) return(NULL)
  yr <- suppressWarnings(as.integer(g$key))
  # Same bound at both ends as every other yearly view. These counts are aligned
  # to the (already bounded) OA years in fetch.R, so an unbounded year could not
  # reach the output today -- but leaving one code path with a different notion
  # of a valid publication year is how the 2035 row got in.
  keep <- !is.na(yr) & yr >= start_year & yr <= openalex_year_cap()
  data.frame(year = yr[keep], ca_doaj_works = g$count[keep], stringsAsFactors = FALSE)
}

# Open-access status composition (gold/hybrid/green/bronze/diamond/closed) of an
# institution's corresponding-author works in a single reference year.
# `inst_ids` may be one id or several (OR-ed). Returns (oa_status, ca_works)
# or NULL.
openalex_ca_oa_status <- function(inst_ids, year) {
  ids <- paste(openalex_bare(inst_ids), collapse = "|")
  g <- openalex_group_reader(
    paste0("corresponding_institution_ids:", ids, ",publication_year:", year,
           ",", XPAC_EXCLUDE),
    "oa_status")
  if (is.null(g)) return(NULL)
  data.frame(oa_status = g$key, ca_works = g$count, stringsAsFactors = FALSE)
}

# Read the institution configuration (name, openalex_id, ror_id, slug)
# and add a bare OpenAlex id column.
#
# This file is the root of trust for the whole pipeline: fetch.R queries what it
# lists, and validate.R derives its expectations from it -- so a corrupted config
# would be validated against its own corruption, exactly as an incomplete Leiden
# mapping would be. Check it here, where the damage is still visible, rather than
# trusting it everywhere downstream.
read_institutions <- function(path = "data-raw/institutions.csv") {
  if (!file.exists(path))
    stop("institution configuration not found: ", path, call. = FALSE)
  inst <- read_csv(path, col_types = cols(.default = col_character()))
  req <- c("name", "openalex_id", "ror_id", "slug")
  gone <- setdiff(req, names(inst))
  if (length(gone) > 0)
    stop("institution configuration lacks column(s): ", paste(gone, collapse = ", "),
         call. = FALSE)
  if (nrow(inst) == 0)
    stop("institution configuration is empty: ", path, call. = FALSE)

  problems <- character()
  for (col in req) {
    blank <- which(is.na(inst[[col]]) | !nzchar(trimws(inst[[col]])))
    if (length(blank) > 0)
      problems <- c(problems, sprintf("blank %s in row(s) %s", col,
                                      paste(blank, collapse = ", ")))
  }
  dup_slug <- unique(inst$slug[duplicated(inst$slug)])
  if (length(dup_slug) > 0)
    problems <- c(problems, paste("duplicate slug(s):", paste(dup_slug, collapse = ", ")))
  dup_oa <- unique(inst$openalex_id[duplicated(openalex_bare(inst$openalex_id))])
  if (length(dup_oa) > 0)
    problems <- c(problems, paste("duplicate openalex_id(s):", paste(dup_oa, collapse = ", ")))
  # Two houses sharing a ROR id would be two rows describing one institution:
  # the consolidated member sets are keyed by slug but resolved through ROR, so
  # the duplicate would silently double-count in the alliance totals.
  dup_ror <- unique(inst$ror_id[duplicated(inst$ror_id)])
  if (length(dup_ror) > 0)
    problems <- c(problems, paste("duplicate ror_id(s):", paste(dup_ror, collapse = ", ")))
  # The slug is pasted straight into file and URL paths (data/<slug>/…,
  # institutions/<slug>.qmd, /institutions/<slug>/…), so it must be a plain
  # basename. Anything else escapes the directory it is supposed to name -- a
  # slug of "../outside-data" was accepted before this.
  bad_slug <- inst$slug[!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", inst$slug)]
  if (length(bad_slug) > 0)
    problems <- c(problems, paste("unsafe slug(s):", paste(bad_slug, collapse = ", ")))
  # An id in the wrong shape yields a 404 per institution rather than an obvious
  # configuration error, so check the shape here.
  bad_oa <- inst$openalex_id[!grepl("^I[0-9]+$", openalex_bare(inst$openalex_id))]
  if (length(bad_oa) > 0)
    problems <- c(problems, paste("malformed openalex_id(s):", paste(bad_oa, collapse = ", ")))
  bad_ror <- inst$ror_id[!grepl("^0[a-z0-9]{8}$", inst$ror_id)]
  if (length(bad_ror) > 0)
    problems <- c(problems, paste("malformed ror_id(s):", paste(bad_ror, collapse = ", ")))
  if (length(problems) > 0)
    stop("institution configuration is unusable (", path, "):\n  - ",
         paste(problems, collapse = "\n  - "), call. = FALSE)

  inst$openalex_bare <- openalex_bare(inst$openalex_id)
  inst
}

# Read the Leiden consolidation mapping (built by scripts/leiden_affiliations.py)
# and keep only the weight-1 `component` affiliates that carry an OpenAlex id --
# the members that make up a "university incl. its components" consolidated view.
# Returns NULL if the mapping is absent (the pipeline then skips consolidation).
# `inst` defaults to the configuration rather than to NULL on purpose: the
# mapping is only meaningful relative to it, and an optional cross-check is one
# a future call site forgets to ask for.
read_leiden_components <- function(path = "data-raw/leiden_affiliations.csv",
                                   inst = read_institutions()) {
  if (!file.exists(path)) return(NULL)
  la <- read_csv(path, col_types = cols(.default = col_character()))
  # The identity columns are required too: they are what makes the mapping
  # checkable against the configuration at all, and the site prints
  # affiliated_name. Without them the join key would again be taken on trust.
  req <- c("tu9_slug", "university_ror_id", "university_name", "relation_type",
           "weight", "affiliated_name", "affiliated_openalex_id")
  gone <- setdiff(req, names(la))
  if (length(gone) > 0)
    stop("Leiden mapping lacks column(s): ", paste(gone, collapse = ", "),
         " (", path, ")", call. = FALSE)
  # `component` implies weight 1 in the Leiden data, but check it explicitly so
  # the code enforces the contract the comment claims rather than assuming it.
  w <- suppressWarnings(as.numeric(la$weight))
  has_id <- !is.na(la$affiliated_openalex_id) & nzchar(la$affiliated_openalex_id)
  # Every row that calls itself a component must BE one: weight exactly 1 and a
  # usable OpenAlex id. Deriving "is a component" from the weight, as this did,
  # left the circularity half in place -- a component row with a blank or
  # unparsable weight fell out of the set being examined, so the missing-id check
  # never saw it and it was filtered away in silence. Judge membership by
  # relation_type alone and require the rest.
  claims_comp <- la$relation_type == "component"
  bad_weight <- which(claims_comp & (is.na(w) | w != 1))
  if (length(bad_weight) > 0)
    stop(sprintf("Leiden mapping has %d `component` row(s) whose weight is not 1 (%s): %s",
                 length(bad_weight), path,
                 paste(utils::head(la$tu9_slug[bad_weight], 5), collapse = ", ")),
         call. = FALSE)
  orphan <- which(claims_comp & !has_id)
  if (length(orphan) > 0)
    stop(sprintf("Leiden mapping has %d `component` row(s) without an OpenAlex id (%s): %s",
                 length(orphan), path,
                 paste(utils::head(la$tu9_slug[orphan], 5), collapse = ", ")),
         call. = FALSE)
  comp <- la[claims_comp, ]

  # The two fields that decide WHERE a component goes were still taken on trust.
  # `tu9_slug` is the join key: a slug naming no configured institution makes the
  # component vanish from every member set, and validation then derives the same
  # reduced expectation from the same mapping -- the original circularity, intact.
  # `affiliated_openalex_id` is pasted into the consolidated filter, so a value in
  # the wrong shape narrows the query instead of failing it. Neither is checkable
  # against the mapping alone, so validate them JOINTLY with institutions.csv.
  problems <- character()
  for (col in c("tu9_slug", "university_ror_id", "university_name",
                "affiliated_name", "affiliated_openalex_id")) {
    blank <- which(is.na(comp[[col]]) | !nzchar(trimws(comp[[col]])))
    if (length(blank) > 0)
      problems <- c(problems, sprintf("blank %s in %d component row(s)", col,
                                      length(blank)))
  }
  bad_id <- unique(comp$affiliated_openalex_id[
    !grepl("^I[0-9]+$", openalex_bare(comp$affiliated_openalex_id))])
  if (length(bad_id) > 0)
    problems <- c(problems, paste("malformed affiliated_openalex_id(s):",
                                  paste(utils::head(bad_id, 5), collapse = ", ")))
  unknown <- unique(comp$tu9_slug[!(comp$tu9_slug %in% inst$slug)])
  if (length(unknown) > 0)
    problems <- c(problems, paste("component(s) assigned to unconfigured slug(s):",
                                  paste(utils::head(unknown, 5), collapse = ", ")))
  # The mapping restates each university's identity. If those columns have
  # drifted from the configuration, the two files describe different houses and
  # the slug join is a coincidence rather than a fact.
  ident <- c(university_ror_id = "ror_id", university_name = "name")
  for (col in names(ident)) {
    for (s in intersect(unique(comp$tu9_slug), inst$slug)) {
      want <- as.character(inst[[ident[[col]]]][inst$slug == s])
      got <- unique(as.character(comp[[col]][comp$tu9_slug == s]))
      if (!identical(got, want))
        problems <- c(problems, sprintf(
          "%s for %s is '%s', the configuration says '%s'",
          col, s, paste(got, collapse = "/"), want))
    }
  }
  # The member set is built with unique(), so a repeated component or one equal
  # to the university itself would be absorbed silently -- and n_members, which
  # is published, would then disagree with the row count in the mapping. Neither
  # is a legitimate shape for the mapping to have.
  for (s in intersect(unique(comp$tu9_slug), inst$slug)) {
    ids <- openalex_bare(comp$affiliated_openalex_id[comp$tu9_slug == s])
    dup <- unique(ids[duplicated(ids)])
    if (length(dup) > 0)
      problems <- c(problems, sprintf("duplicate component(s) for %s: %s", s,
                                      paste(utils::head(dup, 5), collapse = ", ")))
    own <- openalex_bare(inst$openalex_id[inst$slug == s])
    if (own %in% ids)
      problems <- c(problems, sprintf(
        "component for %s is the university itself (%s)", s, own))
  }
  if (length(problems) > 0)
    stop("Leiden mapping disagrees with the institution configuration (", path,
         "):\n  - ", paste(problems, collapse = "\n  - "), call. = FALSE)
  comp
}

# Pull the entity-level aggregated metrics out of a parsed institution entity.
# Missing fields become NA so the row shape stays stable even if OpenAlex drops
# a field for some institution.
#
# XPAC note: the institution entity's aggregates INCLUDE the Expansion Pack. We
# therefore do NOT use `works_count` from here (it is sourced XPAC-excluded via
# openalex_works_by_year); it is returned only as a fallback. `cited_by_count`,
# `h_index`, `i10_index` and `2yr_mean_citedness` are kept from the entity,
# which is the only place OpenAlex publishes them -- so these figures include XPAC.
openalex_metrics <- function(obj) {
  n  <- function(x) if (is.null(x)) NA_integer_ else as.integer(x)
  d  <- function(x) if (is.null(x)) NA_real_    else as.numeric(x)
  ss <- obj$summary_stats
  list(
    works_count        = n(obj$works_count),
    cited_by_count     = n(obj$cited_by_count),
    h_index            = n(ss$h_index),
    i10_index          = n(ss$i10_index),
    two_yr_mean_cited  = d(ss$`2yr_mean_citedness`)
  )
}

# Turn an entity's counts_by_year list into a data frame
# (year, works_count, cited_by_count), newest year first.
#
# `year_cap` defaults to the run-wide cap, which is what the fetch wants. A
# VALIDATOR must pass the year of the snapshot it is checking instead: reading
# the process clock there means the same committed snapshot reconstructs
# differently depending on when validation runs. The 2026 archives already carry
# 2027-dated rows, so in 2027 the default would have admitted them and then
# failed against the correctly capped 2026 CSV.
openalex_counts_by_year <- function(obj, year_cap = openalex_year_cap()) {
  cby <- obj$counts_by_year
  if (is.null(cby) || length(cby) == 0) {
    return(data.frame(year = integer(), works_count = integer(),
                      cited_by_count = integer(), stringsAsFactors = FALSE))
  }
  df <- data.frame(
    year           = vapply(cby, function(x) as.integer(x$year %||% NA), integer(1)),
    works_count    = vapply(cby, function(x) as.integer(x$works_count %||% NA), integer(1)),
    cited_by_count = vapply(cby, function(x) as.integer(x$cited_by_count %||% NA), integer(1)),
    stringsAsFactors = FALSE
  )
  # OpenAlex occasionally carries a stray future year (a work with a wrong
  # publication date). Drop anything past the current year so the yearly view
  # stays clean; the raw JSON snapshot still preserves the original values.
  df[!is.na(df$year) & df$year <= year_cap, , drop = FALSE]
}

# Small null-coalescing helper (base R has none).
`%||%` <- function(a, b) if (is.null(a)) b else a

# The latest publication year any view may contain. OpenAlex carries mis-dated
# records, so every yearly view drops anything past it.
#
# This is deliberately ONE value for a whole run rather than an argument on each
# helper: fetch.R fixes the snapshot date once and validate.R checks the year
# bound against it, so a second clock is a source of disagreement, not of
# flexibility. Reading Sys.Date() per query meant a run crossing midnight could
# fetch a year its own validation then rejected. Callers set it once via
# openalex_set_year_cap(); unset, it is the current year, which is what every
# call site did before.
.openalex_year_cap <- new.env(parent = emptyenv())
openalex_set_year_cap <- function(year) {
  assign("year", as.integer(year), envir = .openalex_year_cap)
}
openalex_year_cap <- function() {
  if (exists("year", envir = .openalex_year_cap))
    get("year", envir = .openalex_year_cap)
  else
    as.integer(format(Sys.Date(), "%Y"))
}
