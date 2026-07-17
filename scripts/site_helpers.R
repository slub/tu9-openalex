# Shared helpers for the Quarto website. All chunks run from the project root
# (see `execute-dir: project` in _quarto.yml), so paths are given relative to it.

suppressPackageStartupMessages({
  library(readr)
  library(reactable)
  library(htmltools)
})

read_data <- function(...) {
  read_csv(file.path("data", ...), col_types = cols(.default = col_character()))
}

read_meta <- function() {
  jsonlite::read_json("data/meta.json", simplifyVector = FALSE)
}

# Format an integer count for display, with a thousands separator (e.g. 12874 ->
# "12,874"). Used wherever a large count is shown in prose.
fmt_int <- function(x) {
  x <- suppressWarnings(as.integer(x))
  ifelse(is.na(x), "", formatC(x, format = "d", big.mark = ","))
}

# Link an OpenAlex short id (e.g. "I78650965") to its entity page.
openalex_cell <- function(value) {
  if (is.na(value) || value == "") return("")
  url <- paste0("https://openalex.org/", value)
  as.character(tags$a(href = url, target = "_blank", value))
}

# Link a bare ROR id to its record.
ror_cell <- function(value) {
  if (is.na(value) || value == "") return("")
  url <- paste0("https://ror.org/", value)
  as.character(tags$a(href = url, target = "_blank", value))
}

# A horizontal bar sized to `value` relative to `max`, with the number beside
# it. Used to give the yearly counts a quick visual scale inside a table cell.
bar_cell <- function(max_value, colour = "#4c78a8") {
  function(value) {
    v <- suppressWarnings(as.numeric(value))
    if (is.na(v)) return("")
    width <- if (max_value > 0) sprintf("%.1f%%", 100 * v / max_value) else "0%"
    bar <- tags$div(style = paste0(
      "background:", colour, ";height:0.8em;width:", width,
      ";border-radius:2px;display:inline-block;vertical-align:middle;"))
    label <- tags$span(style = "margin-left:0.4em;", fmt_int(v))
    as.character(tags$div(style = "display:flex;align-items:center;", bar, label))
  }
}

# Overview table for the landing page, built from meta.json's institution list.
institutions_table <- function(inst, path_prefix = "institutions/") {
  df <- data.frame(
    Institution = vapply(inst, function(x) x$name, character(1)),
    Type        = vapply(inst, function(x) x$type, character(1)),
    Works       = vapply(inst, function(x) as.integer(x$works_count), integer(1)),
    Citations   = vapply(inst, function(x) as.integer(x$cited_by_count), integer(1)),
    h_index     = vapply(inst, function(x) as.integer(x$h_index), integer(1)),
    slug        = vapply(inst, function(x) x$slug, character(1)),
    openalex    = vapply(inst, function(x) x$openalex_id, character(1)),
    stringsAsFactors = FALSE
  )
  reactable(
    df,
    searchable = TRUE, sortable = TRUE, defaultPageSize = 13, highlight = TRUE,
    defaultSorted = list(Works = "desc"),
    columns = list(
      Institution = colDef(minWidth = 240, cell = function(value, index) {
        tags$a(href = sprintf("%s%s.html", path_prefix, df$slug[index]), value)
      }),
      Type      = colDef(maxWidth = 110),
      Works     = colDef(format = colFormat(separators = TRUE, locales = "en-US")),
      Citations = colDef(format = colFormat(separators = TRUE, locales = "en-US")),
      h_index   = colDef(name = "h-index"),
      slug      = colDef(show = FALSE),
      openalex  = colDef(name = "OpenAlex", cell = openalex_cell, html = TRUE,
                         minWidth = 110)
    )
  )
}

# Yearly works / citations table for one institution.
counts_by_year_table <- function(cby) {
  cby <- cby[order(-as.integer(cby$year)), , drop = FALSE]
  works <- suppressWarnings(as.integer(cby$works_count))
  max_w <- if (length(works)) max(works, na.rm = TRUE) else 0
  reactable(
    cby[, c("year", "works_count", "cited_by_count")],
    sortable = TRUE, defaultPageSize = 12, highlight = TRUE,
    columns = list(
      year           = colDef(name = "Year", maxWidth = 90),
      works_count    = colDef(name = "Works", cell = bar_cell(max_w), html = TRUE),
      cited_by_count = colDef(name = "Citations received",
                              format = colFormat(separators = TRUE, locales = "en-US"))
    )
  )
}

# Compose an inline paragraph from text and tag pieces as a single HTML node
# (avoids stray spaces htmltools introduces by pretty-printing children).
inline_p <- function(...) {
  parts <- vapply(list(...), as.character, character(1))
  tags$p(HTML(paste0(parts, collapse = "")))
}

# A file name rendered as an inline `<code>` link.
code_link <- function(href, file) {
  tags$a(href = href, target = "_blank", HTML(as.character(tags$code(file))))
}

# Full body of a per-institution page. Reads the institution's metric time
# series and yearly counts, and links to its OpenAlex entity and raw snapshots.
inst_page <- function(slug) {
  m   <- read_data(slug, "metrics.csv")
  cby <- read_data(slug, "counts_by_year.csv")
  latest <- m[nrow(m), ]

  tagList(
    inline_p(
      "OpenAlex records ", tags$strong(fmt_int(latest$works_count)),
      " works and ", tags$strong(fmt_int(latest$cited_by_count)),
      " citations for this institution",
      ", with an h-index of ", tags$strong(latest$h_index),
      " (snapshot ", latest$snapshot_date, ")."),
    inline_p(
      "OpenAlex entity: ",
      tags$a(href = paste0("https://openalex.org/", latest$openalex_id),
             target = "_blank", latest$openalex_id),
      " · ROR: ",
      tags$a(href = paste0("https://ror.org/", latest$ror_id),
             target = "_blank", latest$ror_id), "."),
    inline_p(
      "Download this institution's data as CSV: ",
      code_link(paste0(slug, "/metrics.csv"), "metrics.csv"),
      " · ",
      code_link(paste0(slug, "/counts_by_year.csv"), "counts_by_year.csv"),
      "."),
    tags$h2(id = "metrics", "Metric history"),
    inline_p(
      "One row per snapshot. It grows as the pipeline runs; ",
      "the OpenAlex entity is re-read on each refresh."),
    metric_history_table(m),
    tags$h2(id = "by-year", "Works and citations by year"),
    counts_by_year_table(cby)
  )
}

# Time series of the tracked metrics across snapshots (one row per snapshot).
metric_history_table <- function(m) {
  m <- m[order(m$snapshot_date, decreasing = TRUE), , drop = FALSE]
  reactable(
    m[, c("snapshot_date", "works_count", "cited_by_count",
          "h_index", "i10_index", "two_yr_mean_citedness")],
    sortable = TRUE, defaultPageSize = 12, highlight = TRUE,
    columns = list(
      snapshot_date         = colDef(name = "Snapshot", maxWidth = 120),
      works_count           = colDef(name = "Works",
                                     format = colFormat(separators = TRUE, locales = "en-US")),
      cited_by_count        = colDef(name = "Citations",
                                     format = colFormat(separators = TRUE, locales = "en-US")),
      h_index               = colDef(name = "h-index"),
      i10_index             = colDef(name = "i10-index"),
      two_yr_mean_citedness = colDef(name = "2yr mean citedness")
    )
  )
}
