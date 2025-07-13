#' Generate rosuvastatin (RA) differential gene expression signature from GSE250020
#'
#' This script downloads the GSE250020 GEO dataset and extracts the differential
#' expression profile for samples treated with rosuvastatin (labelled "RA")
#' against control samples. The resulting table is compared to the
#' `extdata/AllBlood.csv` signature and the Spearman correlation between the two
#' logFC vectors is reported.
#'
#' The resulting RA signature (Gene.symbol, logFC, P.Value) is written to
#' `extdata/RA_GSE250020_DGE.csv`.
#'
#' Required packages: GEOquery, limma, and tidyverse.

library(GEOquery)
library(limma)
library(tidyverse)

# ----------------------------------------------------------------------
# Download and prepare GSE250020
# ----------------------------------------------------------------------

message("Downloading GEO dataset GSE250020 ...")
gse <- getGEO("GSE250020", GSEMatrix = TRUE)
if (length(gse) > 1) gse <- gse[[1]]

pd <- pData(gse)
# Attempt to identify RA vs control samples.  This assumes that a phenotype
# column contains the treatment information with the keyword 'rosuvastatin' or
# 'RA'.  Modify the regular expression if the column naming differs.
ra_flag <- apply(pd, 1, function(x) any(grepl("rosuvastatin|\bRA\b", x, ignore.case = TRUE)))

if (all(!ra_flag)) {
  stop("No rosuvastatin samples detected in the phenotype data")
}

group <- ifelse(ra_flag, "RA", "CTL")
expr <- exprs(gse)

design <- model.matrix(~ 0 + group)
colnames(design) <- c("CTL", "RA")
fit <- lmFit(expr, design)
contrast <- makeContrasts(RA - CTL, levels = design)
fit2 <- eBayes(contrasts.fit(fit, contrast))

dge <- topTable(fit2, number = nrow(expr))

# Retrieve gene symbols from feature data (common GEO column names attempted)
feat <- fData(gse)
symbol_col <- intersect(colnames(feat), c("Gene.symbol", "Gene Symbol", "Symbol", "GENE_SYMBOL"))[1]
if (is.na(symbol_col)) {
  stop("Gene symbol column not found in feature data")
}

dge <- dge %>% mutate(Gene.symbol = feat[[symbol_col]]) %>%
  select(Gene.symbol, logFC, P.Value)

# ----------------------------------------------------------------------
# Load comparator signature
# ----------------------------------------------------------------------

all_blood <- read_csv("extdata/AllBlood.csv", show_col_types = FALSE)

# ----------------------------------------------------------------------
# Compare signatures
# ----------------------------------------------------------------------

merged <- inner_join(dge, all_blood, by = "Gene.symbol",
                     suffix = c("_RA", "_AllBlood"))

if (nrow(merged) == 0) {
  stop("No overlapping genes between signatures")
}

spearman_cor <- cor(merged$logFC_RA, merged$logFC_AllBlood,
                    use = "pairwise.complete.obs", method = "spearman")

message(sprintf("Spearman correlation between RA and AllBlood signatures: %.3f",
                spearman_cor))

# ----------------------------------------------------------------------
# Write output tables
# ----------------------------------------------------------------------

write_csv(dge, "extdata/RA_GSE250020_DGE.csv")

