# Helper functions for the OpenAlex Institutions API.
# Docs: https://docs.openalex.org/api-entities/institutions
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

# Read the institution configuration (name, openalex_id, ror_id, slug, type)
# and add a bare OpenAlex id column.
read_institutions <- function(path = "data-raw/institutions.csv") {
  inst <- read_csv(path, col_types = cols(.default = col_character()))
  inst$openalex_bare <- openalex_bare(inst$openalex_id)
  inst
}

# Pull the flat set of aggregated metrics we track over time out of a parsed
# institution entity. Missing fields become NA so the row shape stays stable
# even if OpenAlex drops a field for some institution.
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
