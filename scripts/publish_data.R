#!/usr/bin/env Rscript
# Quarto post-render step: copy the data files into the rendered site at clean,
# prefix-free URLs (no /data/ or /data-raw/). Runs after `quarto render`, which
# sets QUARTO_PROJECT_OUTPUT_DIR to the output directory (_site).
#
#   data/metrics.csv               -> /metrics.csv
#   data/counts_by_year.csv        -> /counts_by_year.csv
#   data/meta.json                 -> /meta.json
#   data/LICENSE                   -> /DATA-LICENSE
#   data-raw/institutions.csv      -> /institutions.csv
#   data/<slug>/metrics.csv        -> /institutions/<slug>/metrics.csv
#   data/<slug>/counts_by_year.csv -> /institutions/<slug>/counts_by_year.csv
#   data/snapshots/<slug>/*.json   -> /institutions/<slug>/snapshots/*.json

out <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR", "_site")

publish <- function(from, to) {
  dest <- file.path(out, to)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(from, dest, overwrite = TRUE))
    stop("publish_data.R: failed to copy ", from, " -> ", dest)
}

# Top-level data products, metadata and the data licence.
publish("data/metrics.csv",        "metrics.csv")
publish("data/counts_by_year.csv", "counts_by_year.csv")
publish("data/meta.json",          "meta.json")
if (file.exists("data/LICENSE")) publish("data/LICENSE", "DATA-LICENSE")

# Build input.
publish("data-raw/institutions.csv", "institutions.csv")

# Per-institution views + raw snapshots, under /institutions/<slug>/ to mirror
# the page paths.
slugs <- list.dirs("data", recursive = FALSE, full.names = FALSE)
slugs <- setdiff(slugs[nzchar(slugs)], "snapshots")
for (slug in slugs) {
  for (f in c("metrics.csv", "counts_by_year.csv")) {
    src <- file.path("data", slug, f)
    if (file.exists(src)) publish(src, file.path("institutions", slug, f))
  }
  snaps <- list.files(file.path("data", "snapshots", slug), pattern = "\\.json$",
                      full.names = TRUE)
  for (s in snaps) {
    publish(s, file.path("institutions", slug, "snapshots", basename(s)))
  }
}

message("publish_data.R: published data files into ", out, "/")
