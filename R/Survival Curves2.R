#Survival Curve
install.packages("ggplot2")
install.packages("reshape2")  # or tidyr
install.packages("survival")
library(dplyr)
library(tidyr)

x <- read.csv("Atovastatin2.0.csv")
y <- read.csv("Rosuvastatin2.0.csv")
  z <- read.csv("Simvastatin2.0.csv")
zz <- read.csv("Prava2.0.csv")

xx <- full_join(x,y, by="Time..Days." )
xxx <- full_join(xx,z)
xxxx <- full_join(xxx,zz)

df <- xxxx %>% select(-X, -X.1)

df <- df %>%
  arrange(Time..Days.)

df1 <- df %>% 
  fill(atorvastatin,
       Rosuvastatin,
       PravaStatin,
       simvastatin,
       NoStain..Prava.,
       No.Statin..Simvastatin.,
       No.Statin..Rosuvastatin.,
       No.Statin..Atorvastatin.,
       .direction = "down")


df1.1 <- df %>% 
  fill(.direction = "down")


library(ggplot2)
library(reshape2)
library(survival)
install.packages("ggprism")
library(ggprism)

df1 <- df1()
data <- df1
  
  
  
   df1 <- df1 %>% rename(No.Stain..Prava. = NoStain..Prava.)

data_long <- melt(data, id.vars = "Time..Days.", variable.name = "Drug", value.name = "Survival")

ggplot(data_long, aes(x = Time..Days., y = Survival, color = Drug)) +
  geom_line() +
  labs(title = "Percentage of Patients Not Experienced Rejection Event",
       x = "Days",
       y = "Percentage") +
  ggprism::theme_prism(base_size = 9)

surv_obj <- Surv(data$Days, data$Simvastatin)
fit <- survfit(surv_obj ~ 1)
plot(fit)





library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)

# Example: Convert to long format
df_long <- df1 %>%
  pivot_longer(cols = -Time..Days., names_to = "group", values_to = "survival") %>%
  mutate(group = factor(group))

# Define linetypes based on "No."
linetypes <- ifelse(grepl("^No\\.", levels(df_long$group)), "dotted", "solid")

library(ggplot2)
library(dplyr)
library(tidyr)

# Example: Convert to long format
df_long <- df1 %>%
  pivot_longer(cols = -time..days, names_to = "group", values_to = "survival") %>%
  mutate(group = factor(group))

# Define linetypes based on "No."
linetypes <- ifelse(grepl("^No\\", levels(df_long$group)), "dotted", "solid")

# Plot manually with ggplot
ggplot(df_long, aes(x = Time..Days., y = survival, color = group, linetype = group)) +
  geom_line(size =1) +
  scale_linetype_manual(values = linetypes) +  # Apply linetype mapping
  theme_minimal() +
  labs(title = "Survival Curve by Statin Use", 
       x = "Time (Days)", 
       y = "Survival Probability") + 
  theme_prism(base_size = 15)+
  theme(legend.title = element_blank())


ggplot(df_long, aes(x = Time..Days., y = survival, color = group, linetype = group)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = linetypes) +
  theme_minimal() +
  labs(title = "Survival Curve by Statin Use", 
       x = "Time (Days)", 
       y = "Survival Probability") +
  theme_prism(base_size = 15) +
  theme(legend.title = element_blank()) +
  coord_fixed(ratio = 1000) 

