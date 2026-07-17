#!/usr/bin/env Rscript
# Quarto pre-render step: expose the data's last-updated date (from
# data/meta.json) as a project variable so the site footer can show it on every
# page without repeating it in each page body. Referenced as {{< var updated >}}.
# Runs before `quarto render`; writes the generated _variables.yml at the root.

meta <- jsonlite::read_json("data/meta.json", simplifyVector = FALSE)
writeLines(sprintf('updated: "%s"', meta$updated), "_variables.yml")
message("gen_variables.R: wrote _variables.yml (updated: ", meta$updated, ")")
