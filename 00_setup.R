## ---------------------------------------------------- ##
# CFD - Generally-Useful Setup Steps
## ---------------------------------------------------- ##
# Purpose:
## Create necessary local folder(s) for following scripts

# Load libraries
librarian::shelf(tidyverse)

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Make Folders ----
## --------------------------- ##

# Create folders
dir.create(path = file.path("Data"), showWarnings = F)

# Create all subfolders of the 'data' folder
purrr::walk(.x = c("00_keys",
                   "01_community_raw_data",
                   "02_community_processed_data",
                   "11_traits_raw_data",
                   "12_traits_processed_data",
                   "21_environmental_raw_data"),
            .f = ~ dir.create(path = file.path("data", .x),
                              showWarnings = F))

# End ----
