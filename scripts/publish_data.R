#!/usr/bin/env Rscript
# Quarto post-render step: copy the data files into the rendered site at clean,
# prefix-free URLs (no /data/ or /data-raw/). Runs after `quarto render`, which
# sets QUARTO_PROJECT_OUTPUT_DIR to the output directory (_site).
#
#   data/metrics.csv               -> /metrics.csv
#   data/counts_by_year.csv        -> /counts_by_year.csv
#   data/ca_oa_by_year.csv         -> /ca_oa_by_year.csv
#   data/ca_oa_status.csv          -> /ca_oa_status.csv
#   data/alliance_ca_oa_by_year.csv -> /alliance_ca_oa_by_year.csv  (global only, no per-slug copy)
#   data/meta.json                 -> /meta.json
#   data/LICENSE                   -> /DATA-LICENSE
#   data-raw/institutions.csv      -> /institutions.csv
#   data/<slug>/*.csv              -> /institutions/<slug>/*.csv
#   data/snapshots/<slug>/*.json   -> /institutions/<slug>/snapshots/*.json

out <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR", "_site")

# Every file below is linked from the Downloads page, so skipping a missing one
# publishes a site with dead links. These products are all mandatory now, so
# absence is a failure rather than a reason to publish less.
publish <- function(from, to) {
  if (!file.exists(from))
    stop("publish_data.R: required file missing: ", from, call. = FALSE)
  dest <- file.path(out, to)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(from, dest, overwrite = TRUE))
    stop("publish_data.R: failed to copy ", from, " -> ", dest, call. = FALSE)
}

PRODUCTS <- c("metrics.csv", "counts_by_year.csv",
              "ca_oa_by_year.csv", "ca_oa_status.csv",
              "consolidated_ca_oa_by_year.csv", "hierarchy_ca_oa_by_year.csv",
              "leiden_core_ca_oa_by_year.csv",
              "leiden_core_any_location_ca_oa_by_year.csv")

# Alliance-level deduplicated union product: one OR-query per view over the
# UNION of member ids across all nine universities, so it has no meaningful
# per-institution slice. Published at the top level only -- NOT part of
# PRODUCTS, which also drives the per-institution loop below.
ALLIANCE_PRODUCT <- "alliance_ca_oa_by_year.csv"

# Top-level data products, metadata and the data licence.
for (f in c(PRODUCTS, ALLIANCE_PRODUCT, "meta.json")) publish(file.path("data", f), f)
publish("data/LICENSE", "DATA-LICENSE")

# Build inputs. The Leiden mapping is required: fetch.R refuses to publish a
# snapshot without it, so a site without it would not match its own data.
publish("data-raw/institutions.csv", "institutions.csv")
publish("data-raw/leiden_affiliations.csv", "leiden_affiliations.csv")

# Per-institution views + raw snapshots, under /institutions/<slug>/ to mirror
# the page paths. Driven by the institution configuration rather than by whatever
# directories happen to exist: listing directories would silently publish a house
# that is no longer configured, and silently omit one whose directory is missing.
source("scripts/openalex.R")
slugs <- sort(read_institutions()$slug)
for (slug in slugs) {
  for (f in PRODUCTS) {
    publish(file.path("data", slug, f), file.path("institutions", slug, f))
  }
  snaps <- list.files(file.path("data", "snapshots", slug), pattern = "\\.json$",
                      full.names = TRUE)
  if (length(snaps) == 0)
    stop("publish_data.R: no raw snapshots for ", slug, call. = FALSE)
  for (s in snaps) {
    publish(s, file.path("institutions", slug, "snapshots", basename(s)))
  }
}

message("publish_data.R: published data files into ", out, "/")
