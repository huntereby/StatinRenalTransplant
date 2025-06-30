StructureSimilarity <- read.csv("Targets/table_download-5.csv")

# Install necessary packages
install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)
library(ggplot2)


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

drug_list <- StructureSimilarity$LINCS.Analog.Name
approved_drugsDSAvsCtl <- drug_list[sapply(drug_list, check_fda_approval)]






StructureFiltered <- StructureSimilarity[StructureSimilarity$LINCS.Analog.Name %in% approved_drugsDSAvsCtl, ]
df1 <- StructureFiltered[, c(3,4)] 
df1 <- df1[-c(1:5,8:9,14,18), ]
df1 <- df1[c(1:10), ]
df1 <- rbind(df1, new_rows)
new_rows<- data.frame(
  LINCS.Analog.Name = c("Simvastatin", "Atorvastatin", 'Rosuvastatin', 'Fluvastatin','Pitavastatin'),
  Similarity = c(.8720,.3036,.2901,.3045,.2852)
)

XXXXX <- merge(df1,similarity_data, by = 'LINCS.Analog.Name', all.x = T)

df1 <- rbind(df1, new_rows)


# Reorder factors for descending bar order
df1$LINCS.Analog.Name <- factor(df1$LINCS.Analog.Name, 
                                levels = unique(df1$LINCS.Analog.Name[order(df1$Similarity)]))
df1 <- df1[-c(3,6,11:24), ]

df1$LINCS.Analog.Name <- factor(df1$LINCS.Analog.Name, levels = df1$LINCS.Analog.Name[order(df1$Similarity)])
df1 <- df1[-c(1, 10,11:24), ]
df1 <- df1[c(1:10), ]
df1 <- df1[-c(1:2), ]

# Plot
ggplot(df1, aes(x = LINCS.Analog.Name, y = Similarity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 20) +  
  labs(title = "Chemical Similarity to Pravastatin",
       y = "Tanimoto Similarity (%)",
       x = "") +
  geom_text(aes(label = round(Similarity, 1)), hjust = -0.1, size = 4) +
  ylim(0, max(df1$Similarity) + 1)

