#Surv Curve Stati project 
#Function

plot_trinetx_surv <- function(csv_path, 
                              cohort_labels = c("Cohort 1", "Cohort 2"), 
                              plot_title = "Kaplan-Meier Survival Curve",
                              flip_survival = FALSE,
                              time_cutoff = 6000,
                              custom_colors = NULL) {
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Load CSV, skipping TriNetX headers
  df <- read_csv(csv_path, skip = 9, show_col_types = FALSE)
  
  # Rename columns
  colnames(df) <- c("time", 
                    "cohort1_surv", "cohort1_lower", "cohort1_upper", 
                    "cohort2_surv", "cohort2_lower", "cohort2_upper")
  
  # Fill missing data
  df <- df %>% fill(everything(), .direction = "down")
  
  # Filter to desired time range
  df <- df %>% filter(time <= time_cutoff)
  
  # Reshape
  df_long <- df %>%
    pivot_longer(cols = -time,
                 names_to = c("cohort", ".value"),
                 names_pattern = "cohort(\\d+)_(.*)")
  
  # Recode cohort labels
  df_long <- df_long %>%
    mutate(cohort = recode(cohort,
                           `1` = cohort_labels[1],
                           `2` = cohort_labels[2]))
  
  # Flip survival to cumulative incidence if desired
  if (flip_survival) {
    df_long <- df_long %>%
      mutate(surv = 1 - surv,
             lower = 1 - upper,
             upper = 1 - lower)
    y_label <- "Cumulative Incidence"
  } else {
    y_label <- "Survival Probability"
  }
  
  # Base plot
  p <- ggplot(df_long, aes(x = time, y = surv, color = cohort, fill = cohort)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    labs(title = plot_title,
         x = "Time (Days)",
         y = y_label) +
    theme_minimal()
  
  # Add custom colors if provided
  if (!is.null(custom_colors)) {
    p <- p + scale_color_manual(values = custom_colors) +
      scale_fill_manual(values = custom_colors)
  }
  
  return(p)
}







p1 <- plot_trinetx_surv("Surv curv Data/RosuSurv.csv", c("Rosuvastatin", "Control"), "",
                        custom_colors = c("Rosuvastatin" = "#a6cee3", "Control" = "#999999"))
p2 <- plot_trinetx_surv("Surv curv Data/SimSurv.csv", c("Simvastatin", "Control"), "",
                        custom_colors = c("Simvastatin" = "#fdbf6f", "Control" = "#999999"))
p3 <- plot_trinetx_surv("Surv curv Data/AtorvaSurv.csv", c("Atorvastatin", "Control"), "",
                        custom_colors = c("Atorvastatin" = "#e31a1c", "Control" = "#999999"))
p4 <- plot_trinetx_surv("Surv curv Data/PravaSurv.csv", c("Pravastatin", "Control"), "",
                        custom_colors = c("Pravastatin" = "#228B22", "Control" = "#999999"))


library(patchwork)
library(ggplot2)

library(ggplot2)
library(patchwork)

# Annotate each plot
p1 <- p1 + annotate("label", x = 300, y = .85, 
                    label = "RR = 0.681
                    \nRR-P < .0001
                    \nHR = .721
                    \nHR-P < 0.0001", 
                    hjust = 0, vjust = 0, size = 3.5)

p2 <- p2 + annotate("label", x = 300, y = .9, 
                    label = "RR = 1.176
                    \nRR-P 0.012
                    \nHR = 1.253
                    \nHR-P 0.510", 
                    hjust = 0, vjust = 0, size = 4)

p3 <- p3 + annotate("label",  x = 300, y = .9,
                    label = "RR = 0.799
                    \nRR-P < 0.0001
                    \nHR = .772
                    \nHR-P  0.002",
                    hjust = 0, vjust = 0, size = 4)

p4 <- p4 + annotate("label",  x = 300, y = .9,
                    label = "RR = 1.053  
                    \nRR-P 0.417
                    \nHR  = 1.085
                    \nHR-P 0.181", 
                    hjust = 0, vjust = 0, size = 4)

# Combine into 2x2 layout with shared legend
p1 <- p1  # keep legend here
p2 <- p2 + theme(legend.position = "none")
p3 <- p3 + theme(legend.position = "none")
p4 <- p4 + theme(legend.position = "none")

# Combine and collect legend
p1 <- p1  # keep the legend here
p2 <- p2 + theme(legend.position = "none")
p3 <- p3 + theme(legend.position = "none")
p4 <- p4 + theme(legend.position = "none")

# Combine and collect the shared legend on the side
final_plot <- (p1 | p2) /
  (p3 | p4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")  # <- move to the side
  theme(legend.position = "bottom")

print(final_plot)



f(p1 / p2 / p3/p4) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

final_plot <- (p1 | p2) /
  (p3|p4 )+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
print(final_plot)
