#!/usr/bin/env Rscript
# Generate a Venn diagram of statin SwissDock targets and a table of
# overlapping targets between Rosuvastatin and Atorvastatin. The script uses
# placeholder data so it can run as a self-contained example.

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

# Placeholder target sets
ator_targets <- paste0('Target', 1:40)
rosu_targets <- paste0('Target', 13:40)  # 28 overlapping targets
simva_targets <- paste0('Target', 25:60)

targets <- list(
  Atorvastatin = ator_targets,
  Rosuvastatin = rosu_targets,
  Simvastatin = simva_targets
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
  fill = c('#66c2a5', '#fc8d62', '#8da0cb'),
  alpha = 0.5,
  cex = 1.2,
  cat.cex = 1.2,
  cat.col = c('#66c2a5', '#fc8d62', '#8da0cb'),
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
    title = 'SwissDock Target Overlap and Mechanisms of Action'
  )

# Save figure
base <- file.path('Docking Results', 'Swiss')
output_file <- file.path(base, 'SwissDock_Venn_MOA.svg')
ggplot2::ggsave(output_file, combined, width = 12, height = 6)
cat('Figure saved to', output_file, '\n')
