# Load libraries
suppressPackageStartupMessages({
  library(GEOquery)
  library(limma)
  library(dplyr)
  library(tibble)
  library(readr)
  library(illuminaHumanv4.db)
  library(AnnotationDbi)
})

# 1. Download and process GSE86216
gse <- getGEO("GSE86216", GSEMatrix = TRUE)[[1]]
expr <- exprs(gse)
pdata <- pData(gse)

# 2. Assign condition labels (FollowUp = rosuvastatin, BaseLine = control)
pdata$Condition <- ifelse(grepl("FollowUp", pdata$title, ignore.case = TRUE),
                          "rosuvastatin", "control")
pdata$Condition <- factor(pdata$Condition)
rownames(pdata) <- colnames(expr)

# 3. Differential expression with limma
design <- model.matrix(~ 0 + pdata$Condition)
colnames(design) <- levels(pdata$Condition)
fit <- lmFit(expr, design)
contrast <- makeContrasts(rosuvastatin - control, levels = design)
fit2 <- contrasts.fit(fit, contrast)
fit2 <- eBayes(fit2)

# 4. Extract probe-level DE results
probe_res <- topTable(fit2, number = Inf, sort.by = "none") %>%
  rownames_to_column("ProbeID")

# 5. Map probe IDs to HGNC gene symbols
probe_res$Gene.symbol <- mapIds(
  illuminaHumanv4.db,
  keys = probe_res$ProbeID,
  column = "SYMBOL",
  keytype = "PROBEID",
  multiVals = "first"
)

# 6. Filter for valid gene symbols
rosu_df <- probe_res %>%
  filter(!is.na(Gene.symbol)) %>%
  dplyr::select(Gene.symbol, drug_logFC = logFC, drug_pval = P.Value)

# 7. Load disease signature from CSV
disease_df <- read_csv("extdata/AllBlood.csv", show_col_types = FALSE) %>%
  rename_with(~c("Gene.symbol", "disease_logFC", "disease_pval")) %>%
  filter(Gene.symbol != "" & !is.na(Gene.symbol))

# 8. Join and identify discordant genes
all_common <- inner_join(rosu_df, disease_df, by = "Gene.symbol")
discordant <- all_common %>%
  filter(sign(drug_logFC) != sign(disease_logFC))

# 9. Save discordant genes to CSV
write_csv(discordant, "rosuvastatin_vs_disease_discordant.csv")

# 10. Calculate and print summary
num_discordant <- nrow(discordant)
num_total <- nrow(all_common)
percent_discordant <- round(100 * num_discordant / num_total, 2)

cat("Discordant gene count:", num_discordant, "/", num_total,
    sprintf("(%.2f%% negative connectivity)\n", percent_discordant))
