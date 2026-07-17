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
message("Generated ", nrow(inst), " institution pages.")
