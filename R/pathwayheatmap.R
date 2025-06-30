#This is the upregulated EnrichR for comps versus DSA-
#Files
AMRall <- global_state[["data"]][["RAll vs CTL"]][["bpn"]]@enrichr@up_enrichr
AMRMale<- global_state[["data"]][["RM vs CTLM"]][["bpn"]]@enrichr@up_enrichr
AMRF<- global_state[["data"]][["RF vs CTLF"]][["bpn"]]@enrichr@up_enrichr
DSAall <- global_state[["data"]][["RAll vs DSA+All"]][["bpn"]]@enrichr@up_enrichr
DSAM <- global_state[["data"]][["RM vs DSA+M"]][["bpn"]]@enrichr@up_enrichr
DSAF <- global_state[["data"]][["RF vs DSA+F"]][["bpn"]]@enrichr@up_enrichr
Ctlall<- global_state[["data"]][["DSA+ALL vs CTL"]][["bpn"]]@enrichr@up_enrichr
ctlm<- global_state[["data"]][["DSA+Male vs CTL Male"]][["bpn"]]@enrichr@up_enrichr
ctlf<- global_state[["data"]][["DSA+Female vs CTL Female"]][["bpn"]]@enrichr@up_enrichr


#Rejection Versus Control All iP Odds Ratio
AMRall2 <- subset(AMRall, select = c(Term, Odds.Ratio))
AMRM2 <- subset(AMRMale, select = c(Term, Odds.Ratio))
AMRF2 <- subset(AMRF, select = c(Term, Odds.Ratio))
DSAall2 <- subset(DSAall, select = c(Term, Odds.Ratio))
DSAM2 <- subset(DSAM, select = c(Term, Odds.Ratio))
DSAF2 <- subset(DSAF, select = c(Term, Odds.Ratio))
Ctlall <- subset(Ctlall, select = c(Term, Odds.Ratio))
ctlm <- subset(ctlm, select = c(Term, Odds.Ratio))
ctlf <- subset(ctlf, select = c(Term, Odds.Ratio))


AMRall2 <- subset(AMRall, select = c(Term, Combined.Score))
AMRM2 <- subset(AMRMale, select = c(Term, Combined.Score))
AMRF2 <- subset(AMRF, select = c(Term, Combined.Score))
DSAall2 <- subset(DSAall, select = c(Term, Combined.Score))
DSAM2 <- subset(DSAM, select = c(Term, Combined.Score))
DSAF2 <- subset(DSAF, select = c(Term, Combined.Score))
Ctlall <- subset(Ctlall, select = c(Term, Combined.Score))
ctlm <- subset(ctlm, select = c(Term, Combined.Score))
ctlf <- subset(ctlf, select = c(Term, Combined.Score))


#FullJoin using dplyr
merged_df <- AMRall2 %>%
  merge(AMRM2, by = "Term") %>%
  merge(AMRF2, by = "Term")%>%
  merge(DSAall2, by = "Term")%>%
  merge(DSAM2, by = "Term")%>%
  merge(DSAF2, by = "Term")%>%
  merge(Ctlall, by = "Term")%>%
  merge(ctlm, by = "Term")%>%
  merge(ctlf, by = "Term")




merged_df <- AMRall2 %>%
  full_join(AMRM2, by = "Term") %>%
  full_join(AMRF2, by = "Term")

merged_df <- merged_df %>%
  mutate(across(everything(), ~ replace_na(., 0)))
numeric_df <- merged_df %>%
  column_to_rownames(var = "Term")

install.packages("pheatmap")
library(pheatmap)
custom_colors <- colorRampPalette(c("white","blue", "red"))(100)
pheatmap(numeric_df, color = custom_colors )

#This is the Downregulated EnrichR for comps versus DSA-
#Files
AMRall <- global_state[["data"]][["RAll vs CTL"]][["bpn"]]@enrichr@sig_down_enrichr
AMRMale<- global_state[["data"]][["RM vs CTLM"]][["bpn"]]@enrichr@sig_down_enrichr
AMRF<- global_state[["data"]][["RF vs CTLF"]][["bpn"]]@enrichr@sig_down_enrichr

#Rejection Versus Control All iP
AMRall2 <- subset(AMRall, select = c(Term, Combined.Score))
AMRM2 <- subset(AMRMale, select = c(Term, Combined.Score))
AMRF2 <- subset(AMRF, select = c(Term, Combined.Score))

EnrichedDown <- merge(AMRall2,AMRM2, by="Term")
EnrichedDown2 <- merge(EnrichedUp,AMRF2, by="Term")






if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")
remotes::install_github("willgryan/PAVER")
