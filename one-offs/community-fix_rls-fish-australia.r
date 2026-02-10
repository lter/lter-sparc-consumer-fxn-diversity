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

## ------------------------------- ##


# End ----