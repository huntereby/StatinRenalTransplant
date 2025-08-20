#!/usr/bin/env Rscript
# Generate a Venn diagram for SwissDock targets using R

if (!requireNamespace('VennDiagram', quietly = TRUE)) {
  install.packages('VennDiagram', repos = 'https://cloud.r-project.org')
}
library(VennDiagram)

base <- file.path('Docking Results', 'Swiss')
files <- list(
  Atorvastatin = file.path(base, 'AtorvastatinSwiss.csv'),
  Rosuvastatin = file.path(base, 'RosuvastatinSwiss.csv'),
  Simvastatin = file.path(base, 'SimvastatinSwiss.csv')
)

read_targets <- function(path) {
  df <- read.csv(path, check.names = TRUE)
  unique(df$Common.name)
}

targets <- lapply(files, read_targets)
venn_plot <- venn.diagram(
  x = targets,
  filename = NULL,
  fill = c('#66c2a5', '#fc8d62', '#8da0cb'),
  alpha = 0.5,
  cex = 1.2,
  cat.cex = 1.2,
  cat.col = c('#66c2a5', '#fc8d62', '#8da0cb'),
  category.names = names(files)
)

output_file <- file.path(base, 'SwissDock_Targets_Venn.svg')
svg(output_file, width = 6, height = 5)
grid::grid.draw(venn_plot)
dev.off()
cat('Venn diagram saved to', output_file, '\n')
