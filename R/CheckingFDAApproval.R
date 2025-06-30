RvsAll <-global_state$data$`RAll vs CTL`$results$lincs$discordant
RvsDSA <-global_state$data$`RAll vs DSA+All`$results$lincs$discordant
DSAvsCtl <-global_state$data$`DSA+ALL vs CTL`$results$lincs$discordant

# Install necessary packages
install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)

library(httr)
library(jsonlite)

library(httr)
library(jsonlite)

library(httr)
library(jsonlite)

library(httr)
library(jsonlite)

library(httr)
library(jsonlite)

# Enhanced function to check FDA approval status
check_fda_approval <- function(drug) {
  # URL encode the drug name
  encoded_drug <- URLencode(drug)
  url <- paste0("https://rxnav.nlm.nih.gov/REST/rxcui.json?name=", encoded_drug)
  
  # Perform the GET request and check status
  response <- httr::GET(url)
  status <- status_code(response)
  message(paste("Status for", drug, ":", status))
  
  if (status != 200) {
    message(paste("Failed to retrieve data for:", drug))
    return(FALSE)
  }
  
  # Extract content and handle errors gracefully
  content <- httr::content(response, "text", encoding = "UTF-8")
  
  # Log the API response
  message(paste("Response for", drug, ":", content))
  
  # Check for "Path or Query Parameter error"
  if (grepl("Path or Query Parameter error", content)) {
    message(paste("Invalid parameter for:", drug))
    return(FALSE)
  }
  
  # Try to parse JSON, if it fails, return FALSE
  json_response <- tryCatch({
    fromJSON(content)
  }, error = function(e) {
    message(paste("JSON parsing error for:", drug))
    return(NULL)
  })
  
  if (is.null(json_response)) {
    message(paste("Failed JSON parse for:", drug))
    return(FALSE)
  }
  
  # Check if rxnormId is present
  if (!is.null(json_response$idGroup$rxnormId)) {
    rxcui <- json_response$idGroup$rxnormId[1]
    message(paste("rxcui found for", drug, ":", rxcui))
    
    # Delay to avoid rate limit
    Sys.sleep(1)
    
    # Corrected endpoint:
    fda_url <- paste0("https://rxnav.nlm.nih.gov/REST/rxcui/", rxcui, "/related.json?tty=BN")
    fda_response <- httr::GET(fda_url)
    
    # Log the brand response
    brand_status <- status_code(fda_response)
    message(paste("Brand response status for", drug, ":", brand_status))
    
    brand_content <- httr::content(fda_response, "text", encoding = "UTF-8")
    message(paste("Brand content for", drug, ":", brand_content))
    
    # If the status is not 200, we know it failed
    if (brand_status != 200) {
      message(paste("Failed to retrieve brand info for:", drug))
      return(FALSE)
    }
    
    fda_data <- tryCatch({
      fromJSON(brand_content)
    }, error = function(e) {
      message(paste("JSON parsing error for brand info:", drug))
      return(NULL)
    })
    
    if (is.null(fda_data)) return(FALSE)
    
    # Check if any brand names are present
    if (length(fda_data$relatedGroup$conceptGroup) > 0) {
      message(paste(drug, "is FDA approved."))
      return(TRUE)
    } else {
      message(paste(drug, "is not FDA approved."))
      return(FALSE)
    }
  } else {
    message(paste(drug, "not found in RxNorm database."))
    return(FALSE)
  }
}

# Apply to list
drug_list <- RvsAll$Target
approved_drugsDSAvsCtl <- drug_list[sapply(drug_list, check_fda_approval)]

view# Output the approved drugs
print(approved_drugsRvsAll)
print(approved_drugsRvsDSA)
print(approved_drugsDSAvsCtl)




RvALLFiltered <- RvsAll[RvsAll$Target %in% approved_drugsRvsAll, ]
df1 <- RvALLFiltered[, c(2,3,7)]

RvDSAFiltered<- RvsDSA[RvsDSA$Target %in% approved_drugsRvsDSA, ]
df2 <- RvDSAFiltered[,c(2,3,7)]

DSAvCtlFiltered<- DSAvsCtl[DSAvsCtl$Target %in% approved_drugsDSAvsCtl, ]
df3<- DSAvCtlFiltered[, c(2,3,7)]

library(dplyr)
merged_df <- df1 %>%
  full_join(df2, by = "TargetSignature") %>%
  full_join(df3, by = "TargetSignature") 


