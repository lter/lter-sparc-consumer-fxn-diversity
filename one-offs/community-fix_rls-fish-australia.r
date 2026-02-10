## ------------------------------------------------------- ##
# Community Data Fix - RLS Fish Data (Australia)
## ------------------------------------------------------- ##
# Purpose:
## Extract sites from two RLS files and create one file that has all unique sites

# Get set up
# Load libraries
librarian::shelf(tidyverse, googledrive)

# Create needed folders
source("00_setup.R")
dir.create(path = file.path("Data", "community_raw-data", "unprocessed"),
  showWarnings = F)

# Download necessary data
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1jGP-q7yun5z2rVHpgGKU-Jrwy4S9bx8r")) %>% 
  dplyr::filter(stringr::str_detect(string = name, pattern = ".csv")) %>% 
  purrr::walk2(.x = .$id, .y = .$name,
    .f = ~ googledrive::drive_download(file = .x, overwrite = T,
      path = file.path("Data", "community_raw-data", "unprocessed", .y)))

# Clear environment & collect garbage
rm(list = ls()); gc()

## ------------------------------- ##
# Load Data ----
## ------------------------------- ##

# Load in the first of the two data files
rls.varA_v01 <- read.csv(file.path("Data", "community_raw-data", "unprocessed", "RLS_fish_data_Australia_sites.csv"))

# Check structure
dplyr::glimpse(rls.varA_v01)

# Load in the other
rls.varB_v01 <- read.csv(file.path("Data", "community_raw-data", "unprocessed", "RLS_fish_data_Austrialia_sites_v2.csv"))

# Check structure
dplyr::glimpse(rls.varB_v01)

# What sites are in each?
sort(unique(rls.varA_v01$location))
sort(unique(rls.varB_v01$location))

## ------------------------------- ##
# Filter Data ----
## ------------------------------- ##

# Filter out redundant locations
rls.varA_v02 <- rls.varA_v01 %>% 
  ## Remove locations found in the other data file
  dplyr::filter(!location %in% unique(rls.varB_v01$location))

# Re-check locations
sort(unique(rls.varA_v02$location))
sort(unique(rls.varB_v01$location))

# Do the same operation for the other data
rls.varB_v02 <- rls.varB_v01 %>% 
  ## Remove locations found in the other data file
  dplyr::filter(!location %in% unique(rls.varA_v02$location))

# Re-check locations
sort(unique(rls.varA_v02$location))
sort(unique(rls.varB_v02$location))
  
## ------------------------------- ##
# Combine ----
## ------------------------------- ##

# Combine the data
rls_v01 <- dplyr::bind_rows(rls.varA_v02, rls.varB_v02)

# Check locations
sort(unique(rls_v01$location))

# Check data structure
dplyr::glimpse(rls_v01)

## ------------------------------- ##
# Export ----
## ------------------------------- ##

# Make a final object
rls_v99 <- rls_v01

# Final location/structure check
dplyr::glimpse(rls_v99)
sort(unique(rls_v99$location))

# Export locally
write.csv(x = rls_v99, row.names = F, na = '',
  file = file.path("Data", "community_raw-data", "RLS_fish_Australia_all-sites.csv"))

# Upload to raw community data folder in Drive
## This step takes ~1min because of the size of the file
googledrive::drive_upload(media = file.path("Data", "community_raw-data", "RLS_fish_Australia_all-sites.csv"),
  overwrite = T,
  path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1n6iqs3aK2xWkROI8nPSVfk6ZkYPNpF7F"))

# End ----