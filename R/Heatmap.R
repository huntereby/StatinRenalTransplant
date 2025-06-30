#Heatmap code
#data
RvsAll <-global_state$data$`RAll vs CTL`$data
RvsMale <-global_state$data$`RM vs CTLM`$data
RvsFemale <- global_state$data$`RF vs CTLF`$data
RvsDSAAll <-global_state$data$`RAll vs DSA+All`$data
RMvsDSAM <-global_state$data$`RM vs DSA+M`$data
RFvsDSAF <- global_state$data$`RF vs DSA+F`$data
DSAvsCTL <- global_state$data$`DSA+ALL vs CTL`$data
DSAMvsCTLM <- global_state$data$`DSA+Male vs CTL Male`$data
DSAFvsCTLF <- global_state$data$`DSA+Female vs CTL Female`$data


datasets <- list(RvsAll,RvsMale,RvsFemale,RvsDSAAll,RMvsDSAM,RFvsDSAF,
                 DSAvsCTL,DSAMvsCTLM,DSAFvsCTLF)

datasets_list_clean <- lapply(datasets, function(df) df[, !grepl("pvalue", colnames(df))])



library(purrr)
merged_df <- reduce(datasets_list_clean, full_join, by = "Symbol")
rownames(merged_df) <- merged_df$Symbol
merged_df<- merged_df[,-1]
library(pheatmap)
library(RColorBrewer)
library(viridis)

pheatmap(mat, color = viridis(100))


pheatmap(merged_df,
         cluster_cols=FALSE,
         color = colorRampPalette(c("blue", "white", "red"))(100),
         gaps_col = c(3,6))



