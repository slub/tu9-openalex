# Shared helpers for the Quarto website. All chunks run from the project root
# (see `execute-dir: project` in _quarto.yml), so paths are given relative to it.

suppressPackageStartupMessages({
  library(readr)
  library(reactable)
  library(htmltools)
})

# The configuration and mapping readers, so the site reads its build inputs
# through exactly the same validated code as the fetch and the validators.
# Sourcing is side-effect-free: openalex.R only defines functions.
if (!exists("read_leiden_components")) source("scripts/openalex.R")

# Null-coalescing helper (base R has none); also used to guard missing OA fields.
`%||%` <- function(a, b) if (is.null(a)) b else a

read_data <- function(...) {
  read_csv(file.path("data", ...), col_types = cols(.default = col_character()))
}

# Read a per-institution product and require it to belong to the CURRENT
# snapshot. These products were best-effort once, and a missing or stale one
# simply dropped its section from the page. They are all mandatory now -- the
# alliance table sums the four OA views over the same nine universities -- so
# silently omitting one produces a wrong page rather than a thinner one. Fail the
# build instead; scripts/validate_products.R checks the same contract up front.
read_current <- function(slug, file, snapshot_date) {
  path <- file.path("data", slug, file)
  if (!file.exists(path))
    stop("required product missing: ", path, call. = FALSE)
  d <- read_data(slug, file)
  if (nrow(d) == 0)
    stop("required product is empty: ", path, call. = FALSE)
  if (!"snapshot_date" %in% names(d))
    stop("required product has no snapshot_date column: ", path, call. = FALSE)
  stale <- setdiff(unique(as.character(d$snapshot_date)), as.character(snapshot_date))
  if (length(stale) > 0)
    stop(sprintf("stale product %s: carries snapshot_date %s, published is %s",
                 path, paste(stale, collapse = ", "), snapshot_date), call. = FALSE)
  d
}

read_meta <- function() {
  jsonlite::read_json("data/meta.json", simplifyVector = FALSE)
}

# The published snapshot date, taken from meta.json. This is the one anchor for
# "current" on the site: reading it from each institution's own metrics.csv
# instead would let a house left behind at an older date render as if it were
# up to date, because every staleness check would then compare it with itself.
published_snapshot <- function() {
  u <- read_meta()$updated
  if (is.null(u) || !nzchar(as.character(u)))
    stop("data/meta.json has no `updated` date; refusing to render", call. = FALSE)
  as.character(u)
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
# Takes the whole meta object so the headers can state the period the figures
# cover: the table is embedded on more than one page, and each embedding would
# otherwise have to name the window in its own prose.
institutions_table <- function(meta, path_prefix = "institutions/") {
  inst   <- meta$institutions
  period <- sprintf("%s–%s", meta$oa_period_start, meta$oa_period_end)
  ca_num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  # Four side-by-side readings, each with its own CA-works denominator and OA
  # share: the single OpenAlex institution (ROR), the same university grouped
  # with its Leiden `component` affiliates, and that consolidated member set
  # restricted to CWTS Core sources under two readings -- any recorded location
  # a Core source, and the narrower primary-venue-only reading.
  df <- data.frame(
    Institution     = vapply(inst, function(x) x$name, character(1)),
    CA_works        = vapply(inst, function(x) as.integer(x$ca_works_period %||% NA), integer(1)),
    CA_OA_ror       = vapply(inst, function(x) ca_num(x$ca_oa_share_period), numeric(1)),
    CA_works_hier   = vapply(inst, function(x) as.integer(x$hier_ca_works_period %||% NA), integer(1)),
    CA_OA_hier      = vapply(inst, function(x) ca_num(x$hier_ca_oa_share_period), numeric(1)),
    CA_works_leiden = vapply(inst, function(x) as.integer(x$cons_ca_works_period %||% NA), integer(1)),
    CA_OA_leiden    = vapply(inst, function(x) ca_num(x$cons_ca_oa_share_period), numeric(1)),
    CA_works_core_any = vapply(inst, function(x) as.integer(x$core_any_ca_works_period %||% NA), integer(1)),
    CA_OA_core_any    = vapply(inst, function(x) ca_num(x$core_any_ca_oa_share_period), numeric(1)),
    CA_works_core   = vapply(inst, function(x) as.integer(x$core_ca_works_period %||% NA), integer(1)),
    CA_OA_core      = vapply(inst, function(x) ca_num(x$core_ca_oa_share_period), numeric(1)),
    slug            = vapply(inst, function(x) x$slug, character(1)),
    openalex        = vapply(inst, function(x) x$openalex_id, character(1)),
    stringsAsFactors = FALSE
  )
  reactable(
    df,
    searchable = FALSE, sortable = TRUE, defaultPageSize = 13, highlight = TRUE,
    defaultSorted = list(CA_OA_ror = "desc"),
    columnGroups = list(
      colGroup(name = sprintf("Single institution %s (ROR/OpenAlex)", period),
               columns = c("CA_works", "CA_OA_ror")),
      colGroup(name = sprintf("Multi-institutional hierarchy %s (OpenAlex/ROR)", period),
               columns = c("CA_works_hier", "CA_OA_hier")),
      colGroup(name = sprintf("Consolidated %s (OpenAlex/Leiden)", period),
               columns = c("CA_works_leiden", "CA_OA_leiden")),
      colGroup(name = sprintf("Core sources (any location) %s", period),
               columns = c("CA_works_core_any", "CA_OA_core_any")),
      colGroup(name = sprintf("Core sources (primary venue) %s", period),
               columns = c("CA_works_core", "CA_OA_core"))
    ),
    columns = list(
      Institution = colDef(minWidth = 200, cell = function(value, index) {
        tags$a(href = sprintf("%s%s.html", path_prefix, df$slug[index]), value)
      }),
      CA_works  = colDef(name = "CA works", minWidth = 90,
                         format = colFormat(separators = TRUE, locales = "en-US")),
      CA_OA_ror = colDef(name = "CA OA share", minWidth = 130,
                         cell = share_bar_cell(), html = TRUE),
      CA_works_hier = colDef(name = "CA works", minWidth = 90,
                             format = colFormat(separators = TRUE, locales = "en-US")),
      CA_OA_hier = colDef(name = "CA OA share", minWidth = 130,
                          cell = share_bar_cell("#3182bd"), html = TRUE),
      CA_works_leiden = colDef(name = "CA works", minWidth = 90,
                               format = colFormat(separators = TRUE, locales = "en-US")),
      CA_OA_leiden = colDef(name = "CA OA share", minWidth = 130,
                            cell = share_bar_cell("#7059b8"), html = TRUE),
      CA_works_core_any = colDef(name = "CA works", minWidth = 90,
                             format = colFormat(separators = TRUE, locales = "en-US")),
      CA_OA_core_any = colDef(name = "CA OA share", minWidth = 130,
                          cell = share_bar_cell("#e6ab02"), html = TRUE),
      CA_works_core = colDef(name = "CA works", minWidth = 90,
                             format = colFormat(separators = TRUE, locales = "en-US")),
      CA_OA_core = colDef(name = "CA OA share", minWidth = 130,
                          cell = share_bar_cell("#d95f02"), html = TRUE),
      slug      = colDef(show = FALSE),
      openalex  = colDef(name = "OpenAlex", cell = openalex_cell, html = TRUE,
                         minWidth = 95)
    )
  )
}

# Stable alliance view identifiers (ALLIANCE_VIEWS in scripts/openalex.R) to
# their display labels, in table row order.
ALLIANCE_VIEW_LABELS <- c(
  single       = "Single institution (ROR/OpenAlex)",
  hierarchy    = "Multi-institutional hierarchy (OpenAlex/ROR)",
  consolidated = "Consolidated (OpenAlex/Leiden)",
  core_any     = "Core sources (any location)",
  core_primary = "Core sources (primary venue)"
)

# Alliance-level summary across the five corresponding-author OA views, built
# from meta$alliance (scripts/fetch.R) rather than re-derived from the
# per-institution figures: each view's figures come from ONE deduplicated
# OpenAlex OR-query across all nine universities, not from summing or
# averaging the nine per-institution rows shown on the Institutions page.
#
#   Distinct CA works -- U, the deduplicated works count itself
#   CA OA share        -- U_OA / U, a direct ratio over that same union
#
# meta.json's alliance entries also carry sum_ca_works, sum_ca_oa_works and
# overlap_share (S, S_OA and (S - U) / S) -- kept as a validated diagnostic
# field (see scripts/validate.R and CONTRACT.md) and available via meta.json
# and data/alliance_ca_oa_by_year.csv, but deliberately NOT surfaced as a
# headline column here; see background.qmd for why it reads as a data detail
# rather than a site metric.
#
# meta.json is regenerated fresh on every run and carries no history, so a
# missing or incomplete alliance block means the site has nothing current to
# show -- fail loudly rather than silently render a thinner table.
alliance_summary_table <- function(meta) {
  period <- sprintf("%s–%s", meta$oa_period_start, meta$oa_period_end)
  al <- meta$alliance
  if (is.null(al) || length(al) == 0)
    stop("data/meta.json has no alliance summary; refusing to render", call. = FALSE)
  views <- names(ALLIANCE_VIEW_LABELS)
  missing_v <- setdiff(views, names(al))
  if (length(missing_v) > 0)
    stop("data/meta.json alliance summary is missing view(s): ",
         paste(missing_v, collapse = ", "), call. = FALSE)

  num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  df <- data.frame(
    View           = unname(ALLIANCE_VIEW_LABELS[views]),
    Distinct_works = vapply(views, function(v) as.integer(num(al[[v]]$ca_works)), integer(1)),
    OA_share       = vapply(views, function(v) num(al[[v]]$ca_oa_share), numeric(1)),
    stringsAsFactors = FALSE
  )

  reactable(
    df,
    sortable = FALSE, highlight = TRUE,
    columns = list(
      View           = colDef(minWidth = 230),
      Distinct_works = colDef(name = sprintf("Distinct CA works %s", period), minWidth = 140,
                              format = colFormat(separators = TRUE, locales = "en-US")),
      OA_share       = colDef(name = sprintf("CA OA share %s", period), minWidth = 150,
                              cell = share_bar_cell("#2a9d4a"), html = TRUE)
    )
  )
}

# Compact directory of institution pages, one linked card per university.
institution_directory <- function(inst, path_prefix = "") {
  df <- data.frame(
    Institution = vapply(inst, function(x) x$name, character(1)),
    slug        = vapply(inst, function(x) x$slug, character(1)),
    stringsAsFactors = FALSE
  )
  tags$div(
    style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(260px,1fr));gap:0.75rem;margin-top:1rem;",
    lapply(seq_len(nrow(df)), function(i) {
      tags$a(
        href = sprintf("%s%s.html", path_prefix, df$slug[i]),
        class = "btn btn-outline-primary",
        style = "text-align:left;white-space:normal;",
        df$Institution[i]
      )
    })
  )
}

# Names of the weight-1 component affiliates Leiden merges into a university.
#
# This used to re-implement the component filter inline, which meant the SITE
# parsed the mapping under weaker rules than the fetch and the validators: a row
# with a malformed weight, an unconfigured slug or an identity that had drifted
# from the configuration would be listed on the page while every validator
# rejected it. Go through the one validated reader instead, so the names shown
# are exactly the members the consolidated figures were computed from.
leiden_component_names <- function(slug,
                                   path = "data-raw/leiden_affiliations.csv") {
  la <- read_leiden_components(path)
  if (is.null(la)) return(character(0))
  la$affiliated_name[la$tu9_slug == slug]
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
  # Anchor everything on the published snapshot rather than on this institution's
  # own newest row: a house left behind at an older date would otherwise pass
  # every staleness check, because it would be compared against itself.
  published <- published_snapshot()
  m <- read_data(slug, "metrics.csv")
  if (nrow(m) == 0)
    stop("required product is empty: data/", slug, "/metrics.csv", call. = FALSE)
  latest <- m[nrow(m), ]
  if (!identical(as.character(latest$snapshot_date), published))
    stop(sprintf("stale metrics for %s: newest row is %s, published is %s",
                 slug, latest$snapshot_date, published), call. = FALSE)

  # Every product below is mandatory; read_current() fails the build if one is
  # missing, empty or stale.
  oa     <- read_current(slug, "ca_oa_by_year.csv", published)
  status <- read_current(slug, "ca_oa_status.csv", published)

  # Leiden-consolidated view (university + component affiliates), universities only.
  cons <- read_current(slug, "consolidated_ca_oa_by_year.csv", published)
  cons_section <- NULL
  if (!is.null(cons) && nrow(cons) > 0) {
    cref <- cons[cons$year == latest$ref_year, ]
    members <- leiden_component_names(slug)
    leiden_link <- tags$a(href = "https://open.leidenranking.com/",
                          target = "_blank", "CWTS Leiden Ranking Open Edition")
    # "Adding those raises X to Y" only holds where the components actually
    # contribute in the reference year. Two cases do not: a university with no
    # components at all, and one whose components published nothing with a
    # corresponding author that year. Both would otherwise render as "from
    # 1,804 to 1,804", so each gets its own sentence.
    cons_gained <- nrow(cref) > 0 &&
      !is.na(suppressWarnings(as.numeric(cref$ca_works))) &&
      !is.na(suppressWarnings(as.numeric(latest$ca_works_ref))) &&
      as.numeric(cref$ca_works) > as.numeric(latest$ca_works_ref)
    cons_intro <- if (length(members) == 0) inline_p(
      "The ", leiden_link, " counts a university together with its ",
      tags$strong("component"), " organisations. This university has none, so it ",
      "consolidates to itself: the figures below repeat the entity view. It is ",
      "reported so that all nine universities can be read on the same basis.")
    else if (nrow(cref) > 0 && !cons_gained) inline_p(
      "The ", leiden_link, " counts a university together with its ",
      tags$strong("component"), " organisations. Its components added no further ",
      "corresponding-author works in ", tags$strong(latest$ref_year), ", so the ",
      "consolidated count stays at ", tags$strong(fmt_int(cref$ca_works)),
      ", OA share ", tags$strong(fmt_pct(cref$ca_oa_share)),
      ". Earlier years may still differ.")
    else if (nrow(cref) > 0) inline_p(
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

  # Multi-institutional hierarchy (OpenAlex/ROR): the university OR-ed with
  # every institution in ITS OWN OpenAlex `lineage`, independently of the
  # Leiden mapping above. A mandatory product like consolidated, so
  # read_current() fails the build if it is missing, empty or stale.
  hier <- read_current(slug, "hierarchy_ca_oa_by_year.csv", published)
  hier_section <- NULL
  if (!is.null(hier) && nrow(hier) > 0) {
    href <- hier[hier$year == latest$ref_year, ]
    leiden_link <- tags$a(href = "https://open.leidenranking.com/",
                          target = "_blank", "CWTS Leiden Ranking Open Edition")
    hier_section <- tagList(
      tags$h2(id = "hierarchy", "Multi-institutional hierarchy (OpenAlex/ROR)"),
      inline_p(
        "OpenAlex's own institution hierarchy, via ROR, independently of the ",
        leiden_link, " mapping above: the university OR-ed with every ",
        "institution in its OpenAlex ", tags$code("lineage"), " -- its full ",
        "descendant tree, at any depth. This is a different, automatically-",
        "derived hierarchy than Leiden's curated ", tags$strong("component"),
        " list, so the two member sets can diverge in either direction; ",
        "neither is a subset of the other. This view is guaranteed to be at ",
        "least as large as the single-institution view above, but ",
        tags$strong("not"), " guaranteed to fall between it and the Leiden-",
        "consolidated view below -- it can exceed consolidated just as ",
        "easily as fall short of it.",
        if (nrow(href)) paste0(" In ", latest$ref_year, " this covers ",
                               fmt_int(href$n_members), " member institution(s).")
        else ""),
      ca_oa_by_year_table(hier))
  }

  # CWTS Core-source views: same member set as the consolidated view, restricted
  # to the CWTS Core sources allow-list under two readings. Any-location keeps a
  # work when at least one recorded location is a Core source; primary-venue
  # keeps it only when its primary venue is. Both are mandatory products, so
  # read_current() fails the build if either is missing, empty or stale.
  core     <- read_current(slug, "leiden_core_ca_oa_by_year.csv", published)
  core_any <- read_current(slug, "leiden_core_any_location_ca_oa_by_year.csv", published)
  core_section <- NULL
  if (!is.null(core) && nrow(core) > 0) {
    core_members <- leiden_component_names(slug)
    leiden_link <- tags$a(href = "https://open.leidenranking.com/",
                          target = "_blank", "CWTS Leiden Ranking Open Edition")
    core_sources_link <- tags$a(
      href = "https://doi.org/10.5281/zenodo.17200868",
      target = "_blank", "CWTS Core sources allow-list")
    cons_ref_works <- {
      r <- if (!is.null(cons) && nrow(cons) > 0) cons[cons$year == latest$ref_year, ] else NULL
      if (!is.null(r) && nrow(r) > 0) suppressWarnings(as.numeric(r$ca_works)) else NA_real_
    }
    # One reference-year sentence per reading. "down from N across all sources"
    # holds only where the consolidated reference row is available to compare to.
    core_reading <- function(d, keep_phrase) {
      dref <- d[d$year == latest$ref_year, ]
      if (nrow(dref) > 0 && !is.na(cons_ref_works)) inline_p(
        "Keeping only works ", keep_phrase, " leaves ",
        tags$strong(fmt_int(dref$ca_works)), " corresponding-author works in ",
        tags$strong(latest$ref_year), " (down from ",
        tags$strong(fmt_int(cons_ref_works)), " across all sources), OA share ",
        tags$strong(fmt_pct(dref$ca_oa_share)), ".")
      else if (nrow(dref) > 0) inline_p(
        "Keeping only works ", keep_phrase, " leaves ",
        tags$strong(fmt_int(dref$ca_works)), " corresponding-author works in ",
        tags$strong(latest$ref_year), ", OA share ",
        tags$strong(fmt_pct(dref$ca_oa_share)), ".")
      else inline_p("Keeping only works ", keep_phrase, ".")
    }
    core_section <- tagList(
      tags$h2(id = "core", "Core sources (CWTS)"),
      inline_p(
        "These two views use the same ", leiden_link, " member set as the ",
        "consolidated view, but keep only works on the ", core_sources_link,
        ". They are two readings of the same allow-list: ",
        tags$strong("any location"), " keeps a work when at least one of its ",
        "recorded locations is a Core source, while ", tags$strong("primary venue"),
        " keeps it only when its primary venue is a Core source — so primary venue ",
        "is never larger than any location."),
      inline_p(
        "CWTS Core is a curated allow-list of international scientific sources ",
        "in fields suitable for citation analysis; a source not on the list is not ",
        "necessarily predatory or low quality. XPAC records remain excluded. Both ",
        "are source-only filters, not the complete official Leiden ",
        tags$em("core publication"), " definition, which adds publication-level ",
        "criteria such as work type, language and references."),
      if (length(core_members))
        inline_p("Component members included: ", tags$em(paste(core_members, collapse = "; ")), "."),
      tags$h3("Any location"),
      core_reading(core_any, "with at least one location a Core source"),
      ca_oa_by_year_table(core_any),
      tags$h3("Primary venue"),
      core_reading(core, "whose primary venue is a Core source"),
      ca_oa_by_year_table(core))
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
        "transformative agreements. Both the denominator and the numerator are ",
        "counted on this corresponding-author basis. The ",
        tags$strong("CA DOAJ"), " columns show the subset of corresponding-author ",
        "works whose primary source is indexed in ",
        tags$a(href = "https://doaj.org/", target = "_blank", "DOAJ"),
        ", and their share of all CA works for that year. DOAJ indexing is a ",
        "journal attribute, not an additional OA-status category. DOAJ-listed ",
        "works overlap with the mutually exclusive OA-status categories shown ",
        "below, so the DOAJ figures are presented separately rather than added ",
        "to the OA-status composition."),
      if (!is.null(oa)) ca_oa_by_year_table(oa),
      if (!is.null(status)) tagList(
        tags$h3(sprintf("OA-status composition (%s)", latest$ref_year)),
        ca_oa_status_table(status)))
  }
  # `cons_section` (built above) is placed after the OA section in the page body.

  tagList(
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
      if (!is.null(hier)) HTML(paste0(" · ",
        as.character(code_link(paste0(slug, "/hierarchy_ca_oa_by_year.csv"),
                               "hierarchy_ca_oa_by_year.csv")))),
      if (!is.null(cons)) HTML(paste0(" · ",
        as.character(code_link(paste0(slug, "/consolidated_ca_oa_by_year.csv"),
                               "consolidated_ca_oa_by_year.csv")))),
      if (!is.null(core)) HTML(paste0(" · ",
        as.character(code_link(paste0(slug, "/leiden_core_ca_oa_by_year.csv"),
                               "leiden_core_ca_oa_by_year.csv")))),
      if (!is.null(core_any)) HTML(paste0(" · ",
        as.character(code_link(paste0(slug, "/leiden_core_any_location_ca_oa_by_year.csv"),
                               "leiden_core_any_location_ca_oa_by_year.csv")))),
      ". Entity-level indicators (works, citations, h-index, i10-index, 2-year mean ",
      "citedness) are archived per snapshot in ", tags$code("metrics.csv"),
      " above and the raw snapshot JSON, though not shown on this page — see ",
      tags$a(href = "../background.html#context-metrics", "Background"), "."),
    oa_section,
    hier_section,
    cons_section,
    core_section
  )
}

# Corresponding-author OA share by publication year, with a bar on the share.
ca_oa_by_year_table <- function(oa) {
  oa <- oa[order(-as.integer(oa$year)), , drop = FALSE]
  # DOAJ is only computed for the single-institution view; the consolidated and
  # Core products share this renderer but have no DOAJ columns.
  has_doaj <- all(c("ca_doaj_works", "ca_doaj_share") %in% names(oa))
  cols <- c("year", "ca_works", "ca_oa_works", "ca_oa_share")
  defs <- list(
    year        = colDef(name = "Year", maxWidth = 90),
    ca_works    = colDef(name = "CA works",
                         format = colFormat(separators = TRUE, locales = "en-US")),
    ca_oa_works = colDef(name = "CA OA works",
                         format = colFormat(separators = TRUE, locales = "en-US")),
    ca_oa_share = colDef(name = "CA OA share", minWidth = 150,
                         cell = share_bar_cell(), html = TRUE)
  )
  if (has_doaj) {
    cols <- c(cols, "ca_doaj_works", "ca_doaj_share")
    defs$ca_doaj_works <- colDef(name = "CA DOAJ works",
                         format = colFormat(separators = TRUE, locales = "en-US"))
    defs$ca_doaj_share <- colDef(name = "CA DOAJ share", minWidth = 150,
                         cell = share_bar_cell("#0072b2"), html = TRUE)
  }
  reactable(oa[, cols], sortable = TRUE, defaultPageSize = 14, highlight = TRUE,
            columns = defs)
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
