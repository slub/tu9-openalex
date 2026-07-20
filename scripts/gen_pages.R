#!/usr/bin/env Rscript
# Generate one Quarto page per institution (institutions/<slug>.qmd) from the
# template below. Run this after fetch.R and before `quarto render`.

source("scripts/openalex.R")
inst <- read_institutions()
inst <- inst[!duplicated(inst$slug), ]   # one page per institution

dir.create("institutions", showWarnings = FALSE)

template <- '---
title: "%s"
---

```{r}
source("scripts/site_helpers.R")
inst_page("%s")
```
'

for (i in seq_len(nrow(inst))) {
  writeLines(
    sprintf(template, inst$name[i], inst$slug[i]),
    file.path("institutions", paste0(inst$slug[i], ".qmd"))
  )
}
# Remove pages for institutions that are no longer configured. Quarto renders
# every institutions/*.qmd, so a leftover page would keep being built and would
# read data that fetch.R has already deleted -- the build fails loudly, but the
# removal could never complete on its own.
keep <- paste0(inst$slug, ".qmd")
existing <- setdiff(list.files("institutions", pattern = "[.]qmd$"), "index.qmd")
obsolete <- setdiff(existing, keep)
for (f in obsolete) {
  unlink(file.path("institutions", f))
  message("Removed page of unconfigured institution: ", f)
}
message("Generated ", nrow(inst), " institution pages.")
