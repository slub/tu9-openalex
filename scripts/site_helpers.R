# Shared helpers for the Quarto website. All chunks run from the project root
# (see `execute-dir: project` in _quarto.yml), so paths are given relative to it.

suppressPackageStartupMessages({
  library(readr)
  library(reactable)
  library(htmltools)
})

# Null-coalescing helper (base R has none); also used to guard missing OA fields.
`%||%` <- function(a, b) if (is.null(a)) b else a

read_data <- function(...) {
  read_csv(file.path("data", ...), col_types = cols(.default = col_character()))
}

# Read a per-institution product only if it belongs to the CURRENT snapshot.
# A file left behind by an earlier run must never be rendered underneath a newer
# headline, so a missing file, a missing snapshot_date column, or any row from a
# different snapshot is treated as "product absent".
read_current <- function(slug, file, snapshot_date) {
  path <- file.path("data", slug, file)
  if (!file.exists(path)) return(NULL)
  d <- read_data(slug, file)
  if (nrow(d) == 0 || !"snapshot_date" %in% names(d)) return(NULL)
  if (!all(d$snapshot_date == snapshot_date)) return(NULL)
  d
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

# Format a 0..1 share as a percentage string (e.g. 0.7637 -> "76.4%").
fmt_pct <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", sprintf("%.1f%%", 100 * x))
}

# A 0..1 share rendered as a filled bar with the percentage beside it.
share_bar_cell <- function(colour = "#2a9d4a") {
  function(value) {
    v <- suppressWarnings(as.numeric(value))
    if (is.na(v)) return("")
    width <- sprintf("%.1f%%", 100 * max(0, min(1, v)))
    track <- tags$div(style = paste0(
      "background:#e9ecef;border-radius:2px;flex:1;height:0.8em;",
      "position:relative;overflow:hidden;"),
      tags$div(style = paste0("background:", colour, ";height:100%;width:", width, ";")))
    label <- tags$span(style = "margin-left:0.5em;min-width:3.2em;text-align:right;",
                       fmt_pct(v))
    as.character(tags$div(style = "display:flex;align-items:center;", track, label))
  }
}

# Overview table for the landing page, built from meta.json's institution list.
institutions_table <- function(inst, path_prefix = "institutions/") {
  ca_num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  # Two side-by-side readings, each with its own CA-works denominator and OA
  # share: the single OpenAlex institution (ROR), and the same university
  # grouped with its Leiden `component` affiliates.
  df <- data.frame(
    Institution  = vapply(inst, function(x) x$name, character(1)),
    CA_works     = vapply(inst, function(x) as.integer(x$ca_works_period %||% NA), integer(1)),
    CA_OA_ror    = vapply(inst, function(x) ca_num(x$ca_oa_share_period), numeric(1)),
    CA_works_leiden = vapply(inst, function(x) as.integer(x$cons_ca_works_period %||% NA), integer(1)),
    CA_OA_leiden = vapply(inst, function(x) ca_num(x$cons_ca_oa_share_period), numeric(1)),
    slug         = vapply(inst, function(x) x$slug, character(1)),
    openalex     = vapply(inst, function(x) x$openalex_id, character(1)),
    stringsAsFactors = FALSE
  )
  reactable(
    df,
    searchable = FALSE, sortable = TRUE, defaultPageSize = 13, highlight = TRUE,
    defaultSorted = list(CA_OA_ror = "desc"),
    columnGroups = list(
      colGroup(name = "Single institution (ROR/OpenAlex)",
               columns = c("CA_works", "CA_OA_ror")),
      colGroup(name = "Consolidated (OpenAlex/Leiden)",
               columns = c("CA_works_leiden", "CA_OA_leiden"))
    ),
    columns = list(
      Institution = colDef(minWidth = 210, cell = function(value, index) {
        tags$a(href = sprintf("%s%s.html", path_prefix, df$slug[index]), value)
      }),
      CA_works  = colDef(name = "CA works", minWidth = 100,
                         format = colFormat(separators = TRUE, locales = "en-US")),
      CA_OA_ror = colDef(name = "CA OA share", minWidth = 150,
                         cell = share_bar_cell(), html = TRUE),
      CA_works_leiden = colDef(name = "CA works", minWidth = 100,
                               format = colFormat(separators = TRUE, locales = "en-US")),
      CA_OA_leiden = colDef(name = "CA OA share", minWidth = 150,
                            cell = share_bar_cell("#7059b8"), html = TRUE),
      slug      = colDef(show = FALSE),
      openalex  = colDef(name = "OpenAlex", cell = openalex_cell, html = TRUE,
                         minWidth = 100)
    )
  )
}

# Yearly works / citations table for one institution.
counts_by_year_table <- function(cby) {
  cby <- cby[order(-as.integer(cby$year)), , drop = FALSE]
  works <- suppressWarnings(as.integer(cby$works_count))
  max_w <- if (length(works)) max(works, na.rm = TRUE) else 0
  reactable(
    cby[, c("year", "works_count", "works_count_incl_xpac", "cited_by_count")],
    sortable = TRUE, defaultPageSize = 12, highlight = TRUE,
    columns = list(
      year           = colDef(name = "Year", maxWidth = 80),
      works_count    = colDef(name = "Works", cell = bar_cell(max_w), html = TRUE),
      works_count_incl_xpac = colDef(name = "Works (incl. XPAC)", maxWidth = 150,
                              format = colFormat(separators = TRUE, locales = "en-US")),
      cited_by_count = colDef(name = "Citations received",
                              format = colFormat(separators = TRUE, locales = "en-US"))
    )
  )
}

# Names of the weight-1 component affiliates Leiden merges into a university,
# read from the build-input mapping (data-raw/leiden_affiliations.csv).
leiden_component_names <- function(slug,
                                   path = "data-raw/leiden_affiliations.csv") {
  if (!file.exists(path)) return(character(0))
  la <- read_csv(path, col_types = cols(.default = col_character()))
  la <- la[la$tu9_slug == slug & la$relation_type == "component" &
           !is.na(la$affiliated_openalex_id) & nzchar(la$affiliated_openalex_id), ]
  la$affiliated_name
}

# Compose an inline paragraph from text and tag pieces as a single HTML node
# (avoids stray spaces htmltools introduces by pretty-printing children).
inline_p <- function(...) {
  # Drop NULL / empty pieces so callers can inline conditional fragments.
  parts <- Filter(function(x) !is.null(x) && length(x) > 0, list(...))
  parts <- unlist(lapply(parts, as.character), use.names = FALSE)
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

  # OA views are best-effort in the pipeline, so a page must render even if they
  # are absent for this institution.
  oa     <- read_current(slug, "ca_oa_by_year.csv", latest$snapshot_date)
  status <- read_current(slug, "ca_oa_status.csv", latest$snapshot_date)

  # Leiden-consolidated view (university + component affiliates), universities only.
  cons <- read_current(slug, "consolidated_ca_oa_by_year.csv", latest$snapshot_date)
  cons_section <- NULL
  if (!is.null(cons) && nrow(cons) > 0) {
    cref <- cons[cons$year == latest$ref_year, ]
    members <- leiden_component_names(slug)
    leiden_link <- tags$a(href = "https://open.leidenranking.com/",
                          target = "_blank", "CWTS Leiden Ranking Open Edition")
    cons_intro <- if (nrow(cref) > 0) inline_p(
      "The ", leiden_link, " counts a university together with its ",
      tags$strong("component"), " organisations. Adding those raises ",
      "corresponding-author works in ", tags$strong(latest$ref_year), " from ",
      tags$strong(fmt_int(latest$ca_works_ref)), " (entity) to ",
      tags$strong(fmt_int(cref$ca_works)), " (consolidated), OA share ",
      tags$strong(fmt_pct(cref$ca_oa_share)), ".")
    else inline_p("The ", leiden_link,
                  " counts a university together with its ",
                  tags$strong("component"), " organisations.")
    cons_section <- tagList(
      tags$h2(id = "consolidated", "Leiden-consolidated (incl. component affiliates)"),
      cons_intro,
      if (length(members))
        inline_p("Component members added: ", tags$em(paste(members, collapse = "; ")), "."),
      ca_oa_by_year_table(cons))
  }

  oa_intro <- NULL
  oa_section <- NULL
  if (!is.null(latest$ca_works_ref) && !is.na(latest$ca_works_ref) &&
      nzchar(latest$ca_works_ref)) {
    oa_intro <- inline_p(
      "In ", tags$strong(latest$ref_year), ", ",
      tags$strong(fmt_int(latest$ca_works_ref)),
      " works had a corresponding author at this institution, of which ",
      tags$strong(fmt_pct(latest$ca_oa_share_ref)),
      " were open access.")
    oa_section <- tagList(
      tags$h2(id = "oa", "Open access (corresponding author)"),
      inline_p(
        "Share of open-access works among those whose ",
        tags$strong("corresponding author"),
        " is affiliated with this institution — the lens used for OpenAPC and ",
        "transformative agreements. Denominator and numerator both come from ",
        "OpenAlex ", tags$code("corresponding_institution_ids"), "."),
      if (!is.null(oa)) ca_oa_by_year_table(oa),
      if (!is.null(status)) tagList(
        tags$h3(sprintf("OA-status composition (%s)", latest$ref_year)),
        ca_oa_status_table(status)))
  }
  # `cons_section` (built above) is placed after the OA section in the page body.

  tagList(
    inline_p(
      "This university's own OpenAlex record (the single ",
      tags$strong("ROR/OpenAlex"), " institution, all authors) holds ",
      tags$strong(fmt_int(latest$works_count)), " works — ",
      tags$strong(fmt_int(latest$works_count_incl_xpac)),
      " including XPAC, the basis on which the OpenAlex entity reports ",
      tags$strong(fmt_int(latest$cited_by_count)), " citations and an h-index of ",
      tags$strong(latest$h_index), " (snapshot ", latest$snapshot_date,
      "). These are broader than the corresponding-author figures below."),
    oa_intro,
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
      if (!is.null(oa)) HTML(paste0(" · ",
        as.character(code_link(paste0(slug, "/ca_oa_by_year.csv"), "ca_oa_by_year.csv")))),
      if (!is.null(cons)) HTML(paste0(" · ",
        as.character(code_link(paste0(slug, "/consolidated_ca_oa_by_year.csv"),
                               "consolidated_ca_oa_by_year.csv")))),
      "."),
    oa_section,
    cons_section,
    tags$h2(id = "metrics", "Metric history"),
    inline_p(
      "Figures for this single institution (", tags$strong("ROR/OpenAlex"),
      ", all authors), one row per snapshot. ", tags$strong("Works"),
      " exclude XPAC (works API); ", tags$strong("Works (incl. XPAC)"),
      " and the citation columns — citations, h-index, i10-index, 2-year mean ",
      "citedness — come from the OpenAlex entity, so they share the same ",
      "XPAC-inclusive basis (the entity works count is the matching denominator ",
      "for the citation figures)."),
    metric_history_table(m),
    tags$h2(id = "by-year", "Works and citations by year"),
    inline_p(
      "By publication year, for this single institution (",
      tags$strong("ROR/OpenAlex"), ", all authors) — not the corresponding-author ",
      "subset. ", tags$strong("Works"), " exclude XPAC; ",
      tags$strong("Works (incl. XPAC)"), " and the citation counts come from the ",
      "OpenAlex entity, sharing the same XPAC-inclusive basis."),
    counts_by_year_table(cby)
  )
}

# Corresponding-author OA share by publication year, with a bar on the share.
ca_oa_by_year_table <- function(oa) {
  oa <- oa[order(-as.integer(oa$year)), , drop = FALSE]
  reactable(
    oa[, c("year", "ca_works", "ca_oa_works", "ca_oa_share")],
    sortable = TRUE, defaultPageSize = 14, highlight = TRUE,
    columns = list(
      year        = colDef(name = "Year", maxWidth = 90),
      ca_works    = colDef(name = "CA works",
                           format = colFormat(separators = TRUE, locales = "en-US")),
      ca_oa_works = colDef(name = "CA OA works",
                           format = colFormat(separators = TRUE, locales = "en-US")),
      ca_oa_share = colDef(name = "CA OA share", minWidth = 150,
                           cell = share_bar_cell(), html = TRUE)
    )
  )
}

# OA-status composition (gold/hybrid/green/bronze/diamond/closed) for one year.
ca_oa_status_table <- function(status) {
  works <- suppressWarnings(as.integer(status$ca_works))
  status <- status[order(-works), , drop = FALSE]
  max_w  <- if (length(works)) max(works, na.rm = TRUE) else 0
  reactable(
    status[, c("oa_status", "ca_works")],
    sortable = TRUE, defaultPageSize = 8, highlight = TRUE,
    columns = list(
      oa_status = colDef(name = "OA status", maxWidth = 140),
      ca_works  = colDef(name = "CA works", cell = bar_cell(max_w), html = TRUE)
    )
  )
}

# Time series of the tracked metrics across snapshots (one row per snapshot).
metric_history_table <- function(m) {
  m <- m[order(m$snapshot_date, decreasing = TRUE), , drop = FALSE]
  reactable(
    m[, c("snapshot_date", "works_count", "works_count_incl_xpac", "cited_by_count",
          "h_index", "i10_index", "two_yr_mean_citedness")],
    sortable = TRUE, defaultPageSize = 12, highlight = TRUE,
    columns = list(
      snapshot_date         = colDef(name = "Snapshot", maxWidth = 110),
      works_count           = colDef(name = "Works",
                                     format = colFormat(separators = TRUE, locales = "en-US")),
      works_count_incl_xpac = colDef(name = "Works (incl. XPAC)",
                                     format = colFormat(separators = TRUE, locales = "en-US")),
      cited_by_count        = colDef(name = "Citations",
                                     format = colFormat(separators = TRUE, locales = "en-US")),
      h_index               = colDef(name = "h-index"),
      i10_index             = colDef(name = "i10-index"),
      two_yr_mean_citedness = colDef(name = "2yr mean citedness")
    )
  )
}
