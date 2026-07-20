#!/usr/bin/env Rscript
# Quarto pre-render step: expose the data's last-updated date (from
# data/meta.json) as a project variable so the site footer can show it on every
# page without repeating it in each page body. Referenced as {{< var updated >}}.
# Runs before `quarto render`; writes the generated _variables.yml at the root.

meta <- jsonlite::read_json("data/meta.json", simplifyVector = FALSE)
# Without this, a meta.json lacking `updated` gives character(0) from sprintf(),
# writeLines() writes an empty file, and every page renders a blank "Last
# updated" in its footer -- a build that succeeds while publishing an undated
# site.
if (is.null(meta$updated) || !nzchar(as.character(meta$updated)))
  stop("gen_variables.R: data/meta.json has no `updated` date", call. = FALSE)
writeLines(sprintf('updated: "%s"', meta$updated), "_variables.yml")
message("gen_variables.R: wrote _variables.yml (updated: ", meta$updated, ")")
