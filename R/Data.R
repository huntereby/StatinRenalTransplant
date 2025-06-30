# Data: Rejected and Total for each drug
data <- data.frame(
  Drug = c("Atorvastatin", "Pravastatin", "Simvastatin", "Rosuvastatin"),
  Rejected = c(976, 244, 262, 141),
  Total = c(17345, 4656, 4853, 2834)
)

# Create a new column to group Atorvastatin vs Other Statins
# Group "Atorvastatin" separately, and all others as "Other Statins"
data$Group <- ifelse(data$Drug == "Atorvastatin", "Atorvastatin", "Other Statins")

# Aggregate the data for Atorvastatin vs Other Statins
contingency_table <- data.frame(
  Group = c("Atorvastatin", "Other Statins"),
  Rejected = c(976, sum(data$Rejected[data$Group == "Other Statins"])),
  Not_Rejected = c(17345 - 976, sum(data$Total[data$Group == "Other Statins"]) - sum(data$Rejected[data$Group == "Other Statins"]))
)

# Print the contingency table
print(contingency_table)

# Perform Fisher's Exact Test (since Chi-squared may be unreliable due to small numbers)
fisher_test <- fisher.test(contingency_table[, -1])
fisher_test






# Prepare data for the plot
plot_data <- data.frame(
  Statin = c("Atorvastatin", "Atorvastatin", "Other Statins", "Other Statins"),
  Rejection = c("Rejected", "Not Rejected", "Rejected", "Not Rejected"),
  Count = c(50, 100, sum(data$Rejected[data$Group == "Other Statins"]), 
            sum(data$Total_Patients[data$Group == "Other Statins"]) - sum(data$Rejected[data$Group == "Other Statins"]))
)

# Plot the stacked bar chart
library(ggplot2)

ggplot(plot_data, aes(x = Statin, y = Count, fill = Rejection)) +
  geom_bar(stat = "identity") +
  labs(title = "Rejection Rates for Kidney Transplants on Different Statins", 
       x = "Statin", 
       y = "Number of Patients") +
  scale_fill_manual(values = c("Rejected" = "red", "Not Rejected" = "green")) +
  theme_minimal() +
  # Add significance line and annotation
  geom_segment(aes(x = 1, xend = 2, y = 220, yend = 220), color = "black", size = 1) +
  geom_text(aes(x = 1.5, y = 225, label = ifelse(chisq_test$p.value < 0.05, "*", "")), 
            size = 6, color = "black")























# Create a contingency table for rejection status for Atorvastatin vs. Other Drugs
# Note: This will group all drugs into a "Other Drugs" category for comparison

# Create a contingency table
data$Group <- ifelse(data$Drug == "Atorvastatin", "Atorvastatin", "Other Drugs")

# Prepare the data for the chi-squared test (Rejected vs Not Rejected)
contingency_table <- data.frame(
  Group = c("Atorvastatin", "Other Drugs"),
  Rejected = c(976, sum(data$Rejected[data$Group == "Other Drugs"])),
  Not_Rejected = c(17345 - 976, sum(data$Total[data$Group == "Other Drugs"]) - sum(data$Rejected[data$Group == "Other Drugs"]))
)

# Perform a Chi-squared test
chisq_test <- chisq.test(contingency_table[, -1])
chisq_test








table <- matrix(c(559, 10129, 169, 2903,140,2579), nrow = 3, byrow = TRUE)
colnames(table) <- c("Rejection (Yes)", "Rejection (No)")
rownames(table) <- c("Atorvastatin", "Simvastatin","Pravastatin")

print(table)
# Perform the Chi-square test
test_result <- chisq.test(table)

# View the result
test_result

prop_test <- prop.test(table)
prop_test


# Example data in a data frame format
# Example data in a data frame format
data <- data.frame(
  Drug = c("Atorvastatin", "Atorvastatin", 
           "Pravastatin", "Pravastatin",
           "Simvastatin","Simvastatin",
           "Rosuvastatin", "Rosuvastatin"),
  Rejection = c("Rejected", "Total", 
                "Rejected", "Total",
                "Rejected", "Total",
                "Rejected", "Total"),
  Count = c(976, 17345, 
            244, 4656,
            262, 4853,
            141, 2834,)
)

# p-value from your statistical test (just for demonstration)
p_value <- 0.03  # Example p-value showing significance


# Load ggplot2 package
install.packages("ggplot2")
library(ggplot2)

# Load ggplot2 package
library(ggplot2)

# Create the stacked bar plot
ggplot(data, aes(x = Drug, y = Count, fill = Rejection)) +
  geom_bar(stat = "identity") +
  labs(title = "Kidney Transplant Patients on Different Drugs", 
       x = "Drug", 
       y = "Number of Patients") +
  scale_fill_manual(values = c("Rejected" = "red", "Total" = "blue")) +
  theme_minimal() +
  # Add significance line and annotation
  geom_segment(aes(x = 1, xend = 2, y = 260, yend = 260), 
               color = "black", size = 1) +  # Line between bars
  geom_text(aes(x = 1.5, y = 265, label = ifelse(p_value < 0.05, "*", "")), 
            size = 6, color = "black")
