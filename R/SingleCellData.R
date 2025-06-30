# Load libraries
library(GEOquery)
library(DESeq2)
library(dplyr)
library(tibble)

# Load data
gse <- getGEO("GSE196735", GSEMatrix = TRUE)[[1]]
expr <- exprs(gse)
pdata <- pData(gse)

# Clean metadata
pdata$Sex <- ifelse(grepl("female", pdata$title, ignore.case=TRUE), "Female", "Male")
pdata$Condition <- ifelse(grepl("UUO", pdata$title, ignore.case=TRUE), "Treatment", "Control")

# Set rownames to match expression data
rownames(pdata) <- colnames(expr)

# Subset: Male samples
pdata_male <- pdata[pdata$Sex == "Male", ]
expr_male <- expr[, rownames(pdata_male)]
dds_male <- DESeqDataSetFromMatrix(countData = expr_male, colData = pdata_male, design = ~ Condition)
dds_male <- DESeq(dds_male)
res_male <- results(dds_male)
res_male_df <- as.data.frame(res_male) %>%
  rownames_to_column("Gene") %>%
  select(Gene, log2FoldChange, padj) %>%
  filter(!is.na(padj)) %>%
  arrange(padj)
write.csv(res_male_df, "GSE196735_Male_for_iLINCS.csv", row.names = FALSE)

# Subset: Female samples
pdata_female <- pdata[pdata$Sex == "Female", ]
expr_female <- expr[, rownames(pdata_female)]
dds_female <- DESeqDataSetFromMatrix(countData = expr_female, colData = pdata_female, design = ~ Condition)
dds_female <- DESeq(dds_female)
res_female <- results(dds_female)
res_female_df <- as.data.frame(res_female) %>%
  rownames_to_column("Gene") %>%
  select(Gene, log2FoldChange, padj) %>%
  filter(!is.na(padj)) %>%
  arrange(padj)
write.csv(res_female_df, "GSE196735_Female_for_iLINCS.csv", row.names = FALSE)
