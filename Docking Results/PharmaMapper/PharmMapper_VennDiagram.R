#!/usr/bin/env Rscript
# Generate a Venn diagram of PharmMapper statin targets and a table of
# overlapping targets between Rosuvastatin and Atorvastatin. The script reads
# PharmMapper CSV outputs and uses placeholder MOA text so it can run as a
# self-contained example.

packages <- c('VennDiagram', 'ggpubr', 'ggplotify', 'patchwork')
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = 'https://cloud.r-project.org')
  }
}
library(VennDiagram)
library(ggpubr)
library(ggplotify)
library(patchwork)

# Load target lists from PharmMapper CSVs
base <- file.path('Docking Results', 'PharmaMapper')
ator_file <- file.path(base, 'AtorvoPharmMapper.csv')
rosu_file <- file.path(base, 'RosuvaPharmMapper.csv')

ator_df <- read.csv(ator_file, stringsAsFactors = FALSE)
rosu_df <- read.csv(rosu_file, stringsAsFactors = FALSE, skip = 1)

ator_targets <- unique(ator_df$Name)
rosu_targets <- unique(rosu_df$Name)

targets <- list(
  Atorvastatin = ator_targets,
  Rosuvastatin = rosu_targets
)

# Determine overlap between Rosuvastatin and Atorvastatin
overlap <- intersect(ator_targets, rosu_targets)

# Placeholder mechanisms of action (MOA) table for overlapping targets
moa_df <- data.frame(
  Target = overlap,
  Atorvastatin_MOA = paste('Atorvastatin MOA', seq_along(overlap)),
  Rosuvastatin_MOA = paste('Rosuvastatin MOA', seq_along(overlap)),
  check.names = FALSE
)

# Create Venn diagram
venn_plot <- venn.diagram(
  x = targets,
  filename = NULL,
  fill = c('#66c2a5', '#fc8d62'),
  alpha = 0.5,
  cex = 1.2,
  cat.cex = 1.2,
  cat.col = c('#66c2a5', '#fc8d62'),
  category.names = names(targets)
)
venn_gg <- ggplotify::as.ggplot(venn_plot)

# Table of overlapping targets and MOA
table_plot <- ggpubr::ggtexttable(moa_df, rows = NULL,
                                  theme = ggpubr::ttheme('light'))

# Arrange Venn diagram and table side by side
combined <- venn_gg + table_plot +
  patchwork::plot_layout(widths = c(1, 1)) +
  patchwork::plot_annotation(
    title = 'PharmMapper Target Overlap and Mechanisms of Action'
  )

# Save figure
output_file <- file.path(base, 'PharmMapper_Venn_MOA.svg')
ggplot2::ggsave(output_file, combined, width = 12, height = 6)
cat('Figure saved to', output_file, '\n')
