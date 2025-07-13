# Load necessary libraries
suppressPackageStartupMessages({
  library(GEOquery)
  library(DESeq2)
  library(dplyr)
  library(tibble)
  library(readr)
})

#' Download GSE86216 and compare rosuvastatin to disease signature
#'
#' This script downloads GSE86216 using GEOquery, performs a simple
#' differential expression analysis to obtain a rosuvastatin treatment
#' signature and then compares this signature to the disease gene
#' expression profile stored in `extdata/AllBlood.csv`. Genes showing
#' opposite regulation between the rosuvastatin and disease signatures
#' are written to `rosuvastatin_vs_disease_discordant.csv`.
#'
#' The code assumes sample titles contain "rosuvastatin" and "control" to
#' denote treatment groups. Modify the pattern extraction below if the
#' metadata differs.

# download expression set
gse <- getGEO("GSE86216", GSEMatrix = TRUE)[[1]]
expr <- exprs(gse)
pdata <- pData(gse)

# create condition variable from the title
pdata$Condition <- ifelse(grepl("rosuvastatin", pdata$title, ignore.case = TRUE),
                          "rosuvastatin", "control")
rownames(pdata) <- colnames(expr)

# build DESeq2 object and compute differential expression
dds <- DESeqDataSetFromMatrix(countData = expr,
                              colData = pdata,
                              design = ~ Condition)
dds <- DESeq(dds)
res <- results(dds, contrast = c("Condition", "rosuvastatin", "control"))

# format results
res_df <- as.data.frame(res) %>%
  rownames_to_column("Symbol") %>%
  select(Symbol, drug_logFC = log2FoldChange, drug_pval = pvalue)

# read disease signature
disease_df <- read_csv(file.path("extdata", "AllBlood.csv"), show_col_types = FALSE) %>%
  rename(Symbol = 1, disease_logFC = 2, disease_pval = 3) %>%
  filter(Symbol != "" & !is.na(Symbol))

# join and keep genes with opposite regulation
discordant <- inner_join(disease_df, res_df, by = "Symbol") %>%
  filter(sign(disease_logFC) != sign(drug_logFC))

write_csv(discordant, "rosuvastatin_vs_disease_discordant.csv")
