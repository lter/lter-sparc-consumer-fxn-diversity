## ---------------------------------------------------- ##
# CFD - Group Member Shortcut
## ---------------------------------------------------- ##
# Purpose:
## Download inputs required by the 'actual' workflow scripts
## Requires access to this group's Shared Drive so this script is only run-able by current group members

# Drive authentication note:
## To run this code, you need to tell the `googledrive` R package who you are on Google
## Work through this tutorial (est. 5 min) before trying to run this script:
### https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Create needed folders
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

# a function to download the csv files from individual folder; 
# could change the pattern if it is not only the csv files
download_drive_folder <- function(folder_url, local_subfolder, pattern_in = NULL) {
  # Convert URL to Drive ID
  drive_id <- googledrive::as_id(folder_url)
  
  # List all files in the folder
  files <- googledrive::drive_ls(path = drive_id, pattern = pattern_in)
  
  # Download each file to the specified local folder
  purrr::walk2(.x = files$id, .y = files$name,
    .f = ~ googledrive::drive_download(file = .x, overwrite = TRUE, type = "csv",
                                       path = file.path(local_subfolder, .y)))
}

## --------------------------- ##
# Download Keys ----
## --------------------------- ##

# Grab relevant Drive folder URL
download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/1of2tKXcU_KnNDZrYLt3b3smObNrG_aXD",
  local_subfolder = file.path("Data", "-keys"))

## --------------------------- ##
# Download Raw Community Data ----
## --------------------------- ##

# Download the contents of the relevant Drive folder to the respective local folder
download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/1n6iqs3aK2xWkROI8nPSVfk6ZkYPNpF7F",
  local_subfolder = file.path("Data", "community_raw-data"))

## -------------------------- ##
# Download Raw Terrestrial Species Data ----
## -------------------------- ##

# Download the contents of the relevant Drive folder to the respective local folder
download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/14H0hFBHxiqfubRBZ7DUUOSxMh5NOeiJN",
  local_subfolder = file.path("Data", "species_raw-data"))
                                                
## --------------------------- ##
# Download Raw Trait Data ----
## --------------------------- ##

# Download the contents of the relevant Drive folder to the respective local folder
download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/1UAq72kFD8Hh9uV1_He1ijJQ1m4lVg7eK",
  local_subfolder = file.path("Data", "traits_raw-data"))

## --------------------------- ##
# Download Environmental Data ----
## --------------------------- ##

# Download the contents of the relevant Drive folder to the respective local folder
download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/1yUg4tYF7F-fuamODUOy55JUg8tmaUyYD",
  local_subfolder = file.path("Data", "environmental_raw-data"))

#################################################
##################################################
# codes below download the tidy data
#### --------------------------- ##
# Download community_tidy-data ----
## --------------------------- ##

# Grab relevant Drive folder URL
download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/1LE1Rr1Hfa1uZPvZoUIr1t18khnsnbeFV",
  local_subfolder = "community_tidy-data"
)

#### --------------------------- ##
# Download traits_tidy-data ----
## --------------------------- ##

# Grab relevant Drive folder URL
download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/1KPv27jTBIwGwuHNU3-EyWlubN9xXjIDt",
  local_subfolder = "traits_tidy-data"
)

### ---------------------------- ##
# Download species_tidy-data ----
## ----------------------------- ##

# Grab relevant Drive folder URL
download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/1VOJpEarHAs1csIzAT7pobWXWXjLt6TdN",
  local_subfolder = "species_tidy-data"
)

# End ----

