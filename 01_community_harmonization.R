## ---------------------------------------------------- ##
# CFD - Step 1: Harmonization for community data
## ---------------------------------------------------- ##
# Purpose:
## Standarize and combine (i.e., "harmonize") input datasets

# Load libraries
librarian::shelf(tidyverse, ltertools)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Read in Key ----
## --------------------------- ##

# Read in the data key
key <- read.csv(file = file.path("data", "-keys", "community_datakey.csv"))

# Check structure
dplyr::glimpse(key)

# # Make sure it's ready for use with `ltertools::harmonize()`
# key_v2 <- ltertools::check_key(key = key)

# # Re-check structure
# dplyr::glimpse(key_v2)

## --------------------------- ##
# Harmonize Data ----
## --------------------------- ##
# Read in all data (as a list)
list_raw <- ltertools::read(raw_folder = file.path("data", "community_raw-data"), 
                            data_format = "csv")

# # Check structure of one element
# dplyr::glimpse(list_raw)

# Make a list to store standardized outputs
list_std <- list()

# list the name of all datasets
namelist <- sort(intersect(x = key$source, y = names(list_raw)))

# Now, let's loop across datasets in the key
for (focal_src in sort(intersect(x = key$source, y = names(list_raw)))){
  
  # Progress message
  message("Standarizing file: '", focal_src, "'")
  
  
  # Standardize this file
  focal_v1 <- ltertools::standardize(focal_file = focal_src, 
                                     key = key, 
                                     df_list = list_raw)
  
  
  #  Identify valid (non-blank) column names
  nms  <- names(focal_v1)
  good <- !is.na(nms) & nzchar(str_trim(nms))
  
  # Keep only those columns (avoids passing "" anywhere)
  focal_v1_clean <- focal_v1 %>%
    select(all_of(nms[good]))
  
  # Do some bonus processing if taxa are in wide format
  if(any(stringr::str_detect(string = names(focal_v1_clean), pattern = "orig_taxa_"))){
    
    # Flip it to long format & tidy up taxa names
    focal_v2 <- focal_v1_clean %>% 
      tidyr::pivot_longer(cols = dplyr::starts_with("orig_taxa_"),
                          names_to = "species",
                          values_to = "density") %>% 
      dplyr::mutate(species = gsub(pattern = "orig_taxa_", replacement = "", x = species))
    
  } else { focal_v2 <- focal_v1_clean }
 
  # Make a final object
  focal_std <- focal_v2
  
  # Add to output list
  list_std[[focal_src]] <- focal_std
  
} # Close loop


# Unlist outputs 
combo_v1 <- purrr::list_rbind(x = list_std)

# Check structure
dplyr::glimpse(combo_v1)


## --------------------------- ##
# Add on Key Metadata ----
## --------------------------- ##

# Grab some important metadata stored in the key
key_meta <- key %>% 
  dplyr::select(project, data_type, habitat, source) %>% 
  dplyr::distinct()

# Check that out
key_meta

# Attach that to the combined data using the 'source' column
combo_v2 <- combo_v1 %>% 
  dplyr::left_join(y = key_meta, by = "source") %>% 
  dplyr::relocate(project:habitat, .before = source)

# Check structure
dplyr::glimpse(combo_v2)

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make a final object
combo_v99 <- combo_v2

# Double check its structure
dplyr::glimpse(combo_v99)

# Make a filename for it
combo_file <- "01_community_harmonized.csv"

# Export
write.csv(x = combo_v99, row.names = F, na = '',
          file = file.path("data", "community_tidy-data", combo_file))

# End ----
