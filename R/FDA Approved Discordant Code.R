FDAApproved <- global_state$lincs_fda
FDAApproved <- FDAApproved$max_fda_phase

library(dplyr)

# Filter rows where Column1 > 4
filtered_df <- FDAApproved %>% filter(max_fda_phase >= 4)

# Print result
print(filtered_df)

x <- read.csv("RejectionVSCtlDiscordant.csv")
x <- RejectionVSCtlDiscordant


FDADisc2 <- x %>% filter(Target %in% filtered_df$sm_name)

filtered_drugs <- x %>%
  dplyr::filter(tolower(Target) %in% tolower(filtered_df$sm_name))
         