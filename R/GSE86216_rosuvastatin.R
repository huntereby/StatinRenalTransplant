# Load necessary libraries
suppressPackageStartupMessages({
  library(GEOquery)
  library(DESeq2)
  library(dplyr)
  library(tibble)
  library(drugfindR)
})

#' Download GSE86216 and compute rosuvastatin discordant signature
#'
#' This script downloads GSE86216 using GEOquery, performs a simple
#' differential expression analysis and compares the resulting
#' signature to rosuvastatin profiles in iLINCS. The discordant
#' rosuvastatin results are saved as a CSV file.
#'
#' The code assumes samples labelled with "control" and "disease" in
#' the GEO title field. Modify the pattern extraction below if the
#' metadata differs.

# download expression set
gse <- getGEO("GSE86216", GSEMatrix = TRUE)[[1]]
expr <- exprs(gse)
pdata <- pData(gse)

# create condition variable from the title
pdata$Condition <- ifelse(grepl("control", pdata$title, ignore.case = TRUE),
                          "control", "disease")
rownames(pdata) <- colnames(expr)

# build DESeq2 object and compute differential expression
dds <- DESeqDataSetFromMatrix(countData = expr,
                              colData = pdata,
                              design = ~ Condition)
dds <- DESeq(dds)
res <- results(dds, contrast = c("Condition", "disease", "control"))

# prepare data frame for drugfindR
res_df <- as.data.frame(res) %>%
  rownames_to_column("Symbol") %>%
  select(Symbol, log2FoldChange, pvalue)

# query iLINCS for rosuvastatin connectivity
lincs_res <- investigateSignature(res_df,
                                  outputLib = "CP",
                                  filterThreshold = 0,
                                  geneColumn = "Symbol",
                                  logfcColumn = "log2FoldChange",
                                  pvalColumn = "pvalue")

# extract rosuvastatin entries and keep discordant only
rosuvastatin_discordant <- lincs_res %>%
  filter(Target == "rosuvastatin", Similarity < 0)

write.csv(rosuvastatin_discordant,
          file = "GSE86216_rosuvastatin_discordant.csv",
          row.names = FALSE)
