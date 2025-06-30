#' Generate rosuvastatin (RA) differential gene expression signature from GSE250020
#'
#' This script downloads the GSE250020 GEO dataset and extracts the differential
#' expression profile for samples treated with rosuvastatin (labelled "RA")
#' against control samples. The resulting table is filtered to L1000 genes and
#' compared to the `extdata/AllBlood.csv` signature. A Spearman correlation is
#' reported to quantify discordance between the two signatures.
#'
#' Output tables are written to `extdata/RA_GSE250020_DGE.csv` and
#' `extdata/AllBlood_L1000.csv`.
#'
#' Required packages: GEOquery, limma, tidyverse and drugfindR.

library(GEOquery)
library(limma)
library(tidyverse)

# L1000 gene set from drugfindR. The function name may differ depending on
# the installed version; adjust if necessary.
if ("package:drugfindR" %in% search() || requireNamespace("drugfindR", quietly = TRUE)) {
  l1000_genes <- tryCatch({
    drugfindR::load_l1000_genes()
  }, error = function(e) {
    # Fall back to built in dataset if available
    if (exists("l1000_genes", where = asNamespace("drugfindR"))) {
      get("l1000_genes", asNamespace("drugfindR"))$gene
    } else {
      stop("Unable to load L1000 gene set from drugfindR")
    }
  })
} else {
  stop("drugfindR package is required to access L1000 genes")
}

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

dge_l1000 <- dge %>% filter(Gene.symbol %in% l1000_genes)

# ----------------------------------------------------------------------
# Load comparator signature and filter to L1000 genes
# ----------------------------------------------------------------------

all_blood <- read_csv("extdata/AllBlood.csv", show_col_types = FALSE)
all_blood_l1000 <- all_blood %>% filter(Gene.symbol %in% l1000_genes)

# ----------------------------------------------------------------------
# Compare signatures
# ----------------------------------------------------------------------

merged <- inner_join(dge_l1000, all_blood_l1000, by = "Gene.symbol",
                     suffix = c("_RA", "_AllBlood"))

if (nrow(merged) == 0) {
  stop("No overlapping L1000 genes between signatures")
}

spearman_cor <- cor(merged$logFC_RA, merged$logFC_AllBlood,
                    use = "pairwise.complete.obs", method = "spearman")

message(sprintf("Spearman correlation between RA and AllBlood signatures: %.3f",
                spearman_cor))

# ----------------------------------------------------------------------
# Write output tables
# ----------------------------------------------------------------------

write_csv(dge_l1000, "extdata/RA_GSE250020_DGE.csv")
write_csv(all_blood_l1000, "extdata/AllBlood_L1000.csv")

