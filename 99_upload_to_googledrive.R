## ---------------------------------------------------- ##
# CFD - Upload Script Outputs to Shared Google Drive
## ---------------------------------------------------- ##
# Purpose:
## Upload outputs to relevant Drive folders

# Drive authentication note:
## To run this code, you need to tell the `googledrive` R package who you are on Google
## Work through this tutorial (est. 5 min) before trying to run this script:
### https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Clear environment & collect garbage
rm(list = ls()); gc()

# Create needed folders
source("00_setup.R")

## --------------------------- ##
# Upload Community Data ----
## --------------------------- ##

# List local file path to relevant folder
comm_path <- file.path("Data", "02_community_processed_data")

# List local files in that folder
(comm_files <- dir(path = comm_path, pattern = "*.csv"))

# Identify the destination Drive folder
comm_drive <- googledrive::as_id("https://drive.google.com/drive/folders/1LE1Rr1Hfa1uZPvZoUIr1t18khnsnbeFV")

# Upload all the files to the Drive (overwriting what's there if there is one already)
purrr::walk(.x = comm_files,
            .f = ~ googledrive::drive_upload(media = file.path(comm_path, .x),
                                             path = comm_drive, overwrite = T))

## --------------------------- ##
# Upload Trait Data ----
## --------------------------- ##

# List local file path to relevant folder
trt_path <- file.path("Data", "12_traits_processed_data")

# List local files in that folder
(trt_files <- dir(path = trt_path, pattern = "*.csv"))

# Identify the destination Drive folder
trt_drive <- googledrive::as_id("https://drive.google.com/drive/folders/1KPv27jTBIwGwuHNU3-EyWlubN9xXjIDt")

# Upload all the files to the Drive (overwriting what's there if there is one already)
purrr::walk(.x = trt_files,
            .f = ~ googledrive::drive_upload(media = file.path(trt_path, .x),
                                             path = trt_drive, overwrite = T))

# End ----
