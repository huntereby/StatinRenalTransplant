volcano_plot2 <- function(X, showFDRLine=TRUE, alpha = 0.05) {
  
  #adjusted p values
  if (showFDRLine) {
    X$padj = p.adjust(X$pvalue, method = "fdr")
    closest_row <- X[which.min(abs(X$padj - alpha)), ]
    closest_pvalue <- closest_row$pvalue
    
    top10 <- X %>%
      filter(padj <= alpha) %>%
      arrange(desc(abs(log2FoldChange))) %>%
      slice_head(n = 10) %>%
      pull(Symbol)
    
    #label
    ns <- paste0("NS (", sum(X$padj > alpha), ")")
    up <-
      paste0("Up (", sum(X$padj <= alpha &
                           X$log2FoldChange > 0), ")")
    down <-
      paste0("Down (", sum(X$padj <= alpha &
                             X$log2FoldChange < 0), ")")
    
    X <- X %>%
      mutate(
        Significant = case_when(
          padj > alpha ~ "NS",
          padj <= alpha & log2FoldChange >= 0 ~ "Up",
          padj <= alpha & log2FoldChange < 0 ~ "Down"
        ) %>% as.factor()
      )
    
  } else {
    top10 <- X %>%
      filter(pvalue <= alpha) %>%
      arrange(desc(abs(log2FoldChange))) %>%
      slice_head(n = 10) %>%
      pull(Symbol)
    
    #label
    ns <- paste0("NS (", sum(X$pvalue > alpha), ")")
    up <-
      paste0("Up (", sum(X$pvalue <= alpha &
                           X$log2FoldChange > 0), ")")
    down <-
      paste0("Down (", sum(X$pvalue <= alpha &
                             X$log2FoldChange < 0), ")")
    
    X <- X %>%
      mutate(
        Significant = case_when(
          pvalue > alpha ~ "NS",
          pvalue <= alpha & log2FoldChange >= 0 ~ "Up",
          pvalue <= alpha & log2FoldChange < 0 ~ "Down"
        ) %>% as.factor()
      )
  }
  
  plot <- X %>%
    mutate(
      top10label = if_else(Symbol %in% top10, Symbol, NA_character_)
    ) %>%
    ggplot(aes(
      x = log2FoldChange,
      y = -log10(pvalue),
      col = Significant,
      label = top10label
    )) +
    geom_point(size = (.25 * .75)) +
    scale_color_manual(labels = c(down, ns, up),
                       values = c("blue", "black", "red")) +
    geom_text_repel(size = 9 / ggplot2:::.pt, fontface= "bold") +
    geom_hline(linetype='dotted', yintercept = -log10(alpha), col = "black") +
    {if(showFDRLine) geom_hline(linetype='dotted', yintercept = -log10(closest_pvalue), col = "black")} +
    theme_pubr() +
    ggprism::theme_prism(base_size=9) +
    theme(legend.position = "top", legend.direction = "horizontal") +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0
      )),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0
      )),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0
      )),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(
        t = 0,
        r = 1,
        b = 0,
        l = 0
      )),
      legend.spacing.y = grid::unit(1, "mm"),
      legend.spacing.x = grid::unit(-1, "mm"),
      legend.box.spacing = grid::unit(-1, "mm"),
      legend.key.spacing.x = grid::unit(1, "mm"),
      legend.key.spacing.y = grid::unit(0, "mm"),
      legend.box.margin = ggplot2::margin(0,0,0,0),
      legend.box = "vertical",
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      panel.spacing = grid::unit(c(0, 0, 0, 0), "mm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.text = ggplot2::element_text(
        face = "bold",
        size = 9,
        margin = ggplot2::margin(0, 0, 0,0)
      )
    ) 
  return(plot)
}



RvsAll <-global_state$data$`RAll vs CTL`$data
RvsMale <-global_state$data$`RM vs CTLM`$data
RvsFemale <- global_state$data$`RF vs CTLF`$data
RvsDSAAll <-global_state$data$`RAll vs DSA+All`$data
RMvsDSAM <-global_state$data$`RM vs DSA+M`$data
RFvsDSAF <- global_state$data$`RF vs DSA+F`$data






plot1 <- volcano_plot2(RvsAll)
plot2 <- volcano_plot2(RvsMale)
plot3 <- volcano_plot2(RvsFemale)
plot4 <- volcano_plot2(RvsDSAAll)
plot5 <- volcano_plot2(RMvsDSAM)
plot6 <- volcano_plot2(RFvsDSAF)
ggsave("plot11.pdf", plot1, dpi = 600, width = 4, height = 4, units = "in")
ggsave("plot22.pdf", plot2, dpi = 600, width = 4, height = 4, units = "in")
ggsave("plot33.pdf", plot3, dpi = 600, width = 4, height = 4, units = "in")
ggsave("plot44.pdf", plot4, dpi = 600, width = 4, height = 4, units = "in")
ggsave("plot55.pdf", plot5, dpi = 600, width = 4, height = 4, units = "in")
ggsave("plot66.pdf", plot6, dpi = 600, width = 4, height = 4, units = "in")

RvsAll <-global_state$data$`RAll vs CTL`$data
#Heatmap
combined_deg_heatmap2 <- function(X, num_genes = 50) {
  combined_degs <- X %>%
    map(
      ~ filter(., pvalue <= 0.05) %>%
        arrange(desc(abs(log2FoldChange))) %>%
        slice_head(n = num_genes) %>%
        pull(Symbol)
    ) %>%
    flatten_chr() %>%
    unique()
  
  mat <- X %>%
    map( ~ filter(., Symbol %in% combined_degs) %>%
           select(Symbol, log2FoldChange)) %>%
    bind_rows(.id = "Group") %>%
    pivot_wider(names_from = Group, values_from = log2FoldChange) %>%
    column_to_rownames(var = "Symbol") %>%
    drop_na() %>%
    as.matrix()
  
  min = min(mat, na.rm = T)
  max = max(mat, na.rm = T)
  
  lgd = ComplexHeatmap::Legend(
    title = "log2FC",
    col_fun = circlize::colorRamp2(c(min, 0, max), c("blue", "white", "red")),
    at = c(min, 0, max),
    labels = c(round(min, 2), 0, round(max, 2)),
    direction = "vertical",
    labels_gp = grid::gpar(fontsize = 9, fontface = "bold"),
    title_gp = grid::gpar(fontsize = 9, fontface = "bold")
  )
  
  ht = ComplexHeatmap::Heatmap(
    mat,
    column_names_rot = 0,
    column_names_gp = grid::gpar(fontsize = 9, fontface = "bold"),
    column_names_centered = TRUE,
    show_row_names = FALSE,
    show_row_dend = FALSE,
    show_column_dend = FALSE,
    show_heatmap_legend = FALSE
  )
  
  ComplexHeatmap::draw(
    ht,
    padding = unit(c(1, 1, 1, 1), "mm"),
    heatmap_legend_list = lgd,
    heatmap_legend_side = "right"
  )
  
}


plot <- x$data %>%
  map("data") %>%
  combined_deg_heatmap2()

plot <- grid::grid.grabExpr(ComplexHeatmap::draw(plot)) %>%
  ggplotify::as.ggplot()

plot

ggsave("deg_heatmap.png", plot, dpi = 600, width = 2, height = 2.5, units = "in")

plot <- PAVER_hunter_plot(PAVER_result)

plot

ggsave("paver_heatmap.png", plot, dpi = 600, width = 5, height = 3.5, units = "in")

plot <- PAVER_dot_plot(PAVER_result)

plot

ggsave("paver_dots.png", plot, dpi = 600, width = 5, height = 1.5, units = "in")