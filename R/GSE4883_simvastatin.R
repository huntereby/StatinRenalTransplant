# Load libraries
suppressPackageStartupMessages({
  library(GEOquery)
  library(limma)
  library(dplyr)
  library(tibble)
  library(readr)
  library(AnnotationDbi)
})

# 1. Download and process GSE4883
gse <- getGEO("GSE4883", GSEMatrix = TRUE)[[1]]
expr <- exprs(gse)
pdata <- pData(gse)

# 2. Assign condition labels (simvastatin vs control)
pdata$Condition <- ifelse(grepl("simvastatin", pdata$title, ignore.case = TRUE),
                          "simvastatin", "control")
pdata$Condition <- factor(pdata$Condition)
rownames(pdata) <- colnames(expr)

# 3. Differential expression with limma
design <- model.matrix(~ 0 + pdata$Condition)
colnames(design) <- levels(pdata$Condition)
fit <- lmFit(expr, design)
contrast <- makeContrasts(simvastatin - control, levels = design)
fit2 <- contrasts.fit(fit, contrast)
fit2 <- eBayes(fit2)

# 4. Extract probe-level DE results
probe_res <- topTable(fit2, number = Inf, sort.by = "none") %>%
  rownames_to_column("ProbeID")

# 5. Map probe IDs to HGNC gene symbols using platform annotation
db_package <- paste0(annotation(gse), ".db")
if (!requireNamespace(db_package, quietly = TRUE)) {
  stop("Annotation package ", db_package, " is not installed")
}
probe_res$Gene.symbol <- mapIds(
  get(db_package),
  keys = probe_res$ProbeID,
  column = "SYMBOL",
  keytype = "PROBEID",
  multiVals = "first"
)

# 6. Filter for valid gene symbols
simva_df <- probe_res %>%
  filter(!is.na(Gene.symbol)) %>%
  dplyr::select(Gene.symbol, drug_logFC = logFC, drug_pval = P.Value)

# 7. Load disease signature from CSV
disease_df <- read_csv("extdata/AllBlood.csv", show_col_types = FALSE) %>%
  rename_with(~c("Gene.symbol", "disease_logFC", "disease_pval")) %>%
  filter(Gene.symbol != "" & !is.na(Gene.symbol))

# 8. Join and identify discordant genes
all_common <- inner_join(simva_df, disease_df, by = "Gene.symbol")
discordant <- all_common %>%
  filter(sign(drug_logFC) != sign(disease_logFC))

# 9. Save discordant genes to CSV
write_csv(discordant, "simvastatin_vs_disease_discordant.csv")

# 10. Calculate and print summary
num_discordant <- nrow(discordant)
num_total <- nrow(all_common)
percent_discordant <- round(100 * num_discordant / num_total, 2)

cat("Discordant gene count:", num_discordant, "/", num_total,
    sprintf("(%.2f%% negative connectivity)\n", percent_discordant))

# 11. Create bar plot comparing with atorvastatin
bar_df <- data.frame(
  Drug = c("Simvastatin", "Atorvastatin"),
  Percent = c(percent_discordant, 57)
)

library(ggplot2)
g <- ggplot(bar_df, aes(x = Drug, y = Percent, fill = Drug)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = -0.5) +
  ylim(0, 100) +
  labs(y = "Discordant Genes (%)", x = NULL,
       title = "Negative Connectivity to Disease Signature") +
  theme_minimal()

ggsave("discordant_barplot.png", g, width = 5, height = 4, dpi = 300)
