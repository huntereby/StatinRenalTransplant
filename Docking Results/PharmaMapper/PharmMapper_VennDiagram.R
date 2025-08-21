#!/usr/bin/env Rscript
# Generate a Venn diagram of PharmMapper statin targets and a table of
# overlapping targets between Rosuvastatin and Atorvastatin. The script scans
# the PharmMapper directory for statin CSV outputs so it can adapt to renamed or
# additional files and uses placeholder MOA text for a self-contained example.

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

read_pharmmapper <- function(file) {
  lines <- readLines(file)
  header_line <- grep('^Pharma Model', lines)[1]
  read.csv(file, skip = header_line - 1, stringsAsFactors = FALSE)
}

# Load all target lists from PharmMapper CSVs
base <- file.path('Docking Results', 'PharmaMapper')
files <- list.files(base, pattern = '\\.(csv|CSV)$', full.names = TRUE)

dfs <- lapply(files, read_pharmmapper)
labels <- tools::file_path_sans_ext(basename(files))
labels <- gsub('PharmMapper', '', labels, ignore.case = TRUE)

targets <- lapply(dfs, function(df) unique(df$Name))
names(targets) <- labels

# Determine overlap between Rosuvastatin and Atorvastatin
drug_names <- tolower(names(targets))
ator_idx <- grep('ator', drug_names)[1]
rosu_idx <- grep('rosu', drug_names)[1]
if (length(ator_idx) && length(rosu_idx)) {
  overlap <- intersect(targets[[ator_idx]], targets[[rosu_idx]])
} else {
  overlap <- character(0)
}

# Placeholder mechanisms of action (MOA) table for overlapping targets
moa_df <- data.frame(
  Target = overlap,
  Atorvastatin_MOA = paste('Atorvastatin MOA', seq_along(overlap)),
  Rosuvastatin_MOA = paste('Rosuvastatin MOA', seq_along(overlap)),
  check.names = FALSE
)

# Create Venn diagram
cols <- c('#66c2a5', '#fc8d62', '#8da0cb', '#e78ac3', '#a6d854')
fills <- cols[seq_along(targets)]
venn_plot <- venn.diagram(
  x = targets,
  filename = NULL,
  fill = fills,
  alpha = 0.5,
  cex = 1.2,
  cat.cex = 1.2,
  cat.col = fills,
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
