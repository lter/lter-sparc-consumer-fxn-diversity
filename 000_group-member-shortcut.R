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
download_drive_folder <- function(folder_url, local_subfolder,pattern_in=NULL) {
  # Convert URL to Drive ID
  drive_id <- googledrive::as_id(folder_url)
  
  # List all files in the folder
  files <- googledrive::drive_ls(path = drive_id,pattern=pattern_in)
  
  # Download each file to the specified local folder
  purrr::walk2(
    .x = files$id, 
    .y = files$name,
    .f = ~ googledrive::drive_download(
      file = .x, 
      overwrite = TRUE, 
      type = "csv",
      path = file.path("Data", local_subfolder, .y)
    )
  )
  
  # Clear environment and run garbage collection
  rm(list = ls()); gc()
}
## --------------------------- ##
# Download Keys ----
## --------------------------- ##

# Grab relevant Drive folder URL
download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/1of2tKXcU_KnNDZrYLt3b3smObNrG_aXD",
  local_subfolder = "-keys"
)
## --------------------------- ##
# Download Raw Community Data ----
## --------------------------- ##

# Grab relevant Drive folder URL
download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/1n6iqs3aK2xWkROI8nPSVfk6ZkYPNpF7F",
  local_subfolder = "community_raw-data"
)

                                                
## --------------------------- ##
# Download Raw Trait Data ----
## --------------------------- ##

download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/1UAq72kFD8Hh9uV1_He1ijJQ1m4lVg7eK",
  local_subfolder = "traits_raw-data"
)
## --------------------------- ##
# Download Environmental Data ----
## --------------------------- ##

download_drive_folder(
  folder_url = "https://drive.google.com/drive/folders/1yUg4tYF7F-fuamODUOy55JUg8tmaUyYD",
  local_subfolder = "environmental_raw-data"
)

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

# End ----
