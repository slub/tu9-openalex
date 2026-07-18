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
})

# Contact e-mail for the OpenAlex "polite pool" (faster, more reliable). Set the
# OPENALEX_MAILTO environment variable to override; falls back to a repo contact.
openalex_mailto <- function() {
  m <- Sys.getenv("OPENALEX_MAILTO")
  if (nzchar(m)) m else "open-access@slub-dresden.de"
}

# Normalise an OpenAlex institution id to its bare short form, e.g.
# "https://openalex.org/I78650965" -> "I78650965".
openalex_bare <- function(x) {
  x <- trimws(x)
  sub("^https?://openalex\\.org/", "", x)
}

# Full API URL for a single institution entity (looked up by its short id).
# Adds the polite-pool `mailto` and, if OPENALEX_API_KEY is set, the premium
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
  url <- openalex_institution_url(id)
  tryCatch(
    {
      old_timeout <- getOption("timeout")
      on.exit(options(timeout = old_timeout), add = TRUE)
      options(timeout = max(60, old_timeout))
      txt <- paste(readLines(url, warn = FALSE), collapse = "\n")
      obj <- fromJSON(txt, simplifyVector = FALSE)
      if (is.null(obj$id)) stop("no id in response")
      obj
    },
    error = function(e) {
      message("  could not read institution ", openalex_bare(id), ": ",
              conditionMessage(e))
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
# if set, the premium api_key).
openalex_works_group_url <- function(filter, group_by) {
  url <- paste0(
    "https://api.openalex.org/works",
    "?filter=", utils::URLencode(filter, reserved = TRUE),
    "&group_by=", utils::URLencode(group_by, reserved = TRUE),
    "&mailto=", utils::URLencode(openalex_mailto(), reserved = TRUE))
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

openalex_group_reader <- function(filter, group_by) {
  url <- openalex_works_group_url(filter, group_by)
  tryCatch(
    {
      old_timeout <- getOption("timeout")
      on.exit(options(timeout = old_timeout), add = TRUE)
      options(timeout = max(60, old_timeout))
      txt <- paste(readLines(url, warn = FALSE), collapse = "\n")
      obj <- fromJSON(txt, simplifyVector = FALSE)
      g <- obj$group_by
      if (is.null(g)) stop("no group_by in response")
      df <- data.frame(
        key   = vapply(g, function(x) as.character(x$key %||% NA), character(1)),
        count = vapply(g, function(x) as.integer(x$count %||% NA), integer(1)),
        stringsAsFactors = FALSE
      )
      attr(df, "total") <- as.integer(obj$meta$count %||% NA)
      df
    },
    error = function(e) {
      message("    works query failed (", group_by, "): ", conditionMessage(e))
      NULL
    }
  )
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
  keep <- !is.na(yr) & yr <= as.integer(format(Sys.Date(), "%Y"))
  list(
    total = attr(g, "total"),
    by_year = data.frame(year = yr[keep], works_count = g$count[keep],
                         stringsAsFactors = FALSE)
  )
}

# Corresponding-author (CA) OA share by publication year, from `start_year`
# onward. `inst_ids` may be one id or several: several are OR-ed in the
# corresponding_institution_ids filter, so a work counts once if its
# corresponding author sits at ANY of them (used for the Leiden-consolidated
# view: university + its component affiliates). All figures are on the
# corresponding-author lens, hence the `ca_` prefix. Returns
# (year, ca_works, ca_oa_works, ca_oa_share) or NULL if either query fails.
openalex_ca_oa_by_year <- function(inst_ids, start_year) {
  ids <- paste(openalex_bare(inst_ids), collapse = "|")
  denom <- openalex_group_reader(
    paste0("corresponding_institution_ids:", ids, ",", XPAC_EXCLUDE),
    "publication_year")
  numer <- openalex_group_reader(
    paste0("corresponding_institution_ids:", ids, ",is_oa:true,", XPAC_EXCLUDE),
    "publication_year")
  if (is.null(denom) || is.null(numer)) return(NULL)

  denom$year <- suppressWarnings(as.integer(denom$key))
  numer$year <- suppressWarnings(as.integer(numer$key))
  denom <- denom[!is.na(denom$year) & denom$year >= start_year, ]
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

# Read the institution configuration (name, openalex_id, ror_id, slug, type)
# and add a bare OpenAlex id column.
read_institutions <- function(path = "data-raw/institutions.csv") {
  inst <- read_csv(path, col_types = cols(.default = col_character()))
  inst$openalex_bare <- openalex_bare(inst$openalex_id)
  inst
}

# Read the Leiden consolidation mapping (built by scripts/leiden_affiliations.py)
# and keep only the weight-1 `component` affiliates that carry an OpenAlex id --
# the members that make up a "university incl. its components" consolidated view.
# Returns NULL if the mapping is absent (the pipeline then skips consolidation).
read_leiden_components <- function(path = "data-raw/leiden_affiliations.csv") {
  if (!file.exists(path)) return(NULL)
  la <- read_csv(path, col_types = cols(.default = col_character()))
  la[la$relation_type == "component" &
     !is.na(la$affiliated_openalex_id) & nzchar(la$affiliated_openalex_id), ]
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
openalex_counts_by_year <- function(obj) {
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
  df[!is.na(df$year) & df$year <= as.integer(format(Sys.Date(), "%Y")), , drop = FALSE]
}

# Small null-coalescing helper (base R has none).
`%||%` <- function(a, b) if (is.null(a)) b else a
