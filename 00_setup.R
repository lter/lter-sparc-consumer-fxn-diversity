## ---------------------------------------------------- ##
# CFD - Step 1: Harmonization
## ---------------------------------------------------- ##
# Purpose:
## Standarize and combine (i.e., "harmonize") input datasets

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Make Folders ----
## --------------------------- ##

# Make the folders used by later scripts
dir.create(path = file.path("data", "preprocess"), showWarnings = F, recursive = T)
dir.create(path = file.path("data", "traits"), showWarnings = F)

## --------------------------- ##
# Download Data Key ----
## --------------------------- ##

# Identify the folder
drive_folder <- googledrive::as_id("https://drive.google.com/drive/folders/1AxdFQ0EjNqaLUTzms4prF52cqbVFec0F")

# Identify the relevant file(s) in that folder
drive_key <- googledrive::drive_ls(path = drive_folder) %>% 
  dplyr::filter(name == "CFD_Datakey")

# Check that worked
drive_key

# Download it
purrr::walk2(.x = drive_key$id, .y = drive_key$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, type = "csv",
                                                path = file.path("data", .y)))

# Clear environment
rm(list = ls()); gc()

## --------------------------- ##
# Download 'Raw' Data ----
## --------------------------- ##

# Identify the folder
drive_folder <- googledrive::as_id("https://drive.google.com/drive/folders/1LE1Rr1Hfa1uZPvZoUIr1t18khnsnbeFV")

# Read in the data key
key_df <- read.csv(file = file.path("data", "CFD_Datakey.csv"))

# Check structure
dplyr::glimpse(key_df)

# What files are referenced in the key?
sort(unique(key_df$source))

# Identify the relevant file(s) in the Drive folder
drive_raw <- googledrive::drive_ls(path = drive_folder) %>% 
  dplyr::filter(name %in% key_df$source)

# Check that worked
drive_raw

# Download it
purrr::walk2(.x = drive_raw$id, .y = drive_raw$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, 
                                                path = file.path("data", "preprocess", .y)))

# Clear environment
rm(list = ls()); gc()

## --------------------------- ##
# Download Zooplankton Traits ----
## --------------------------- ##

# Identify the folder
drive_folder <- googledrive::as_id("https://drive.google.com/drive/folders/1UAq72kFD8Hh9uV1_He1ijJQ1m4lVg7eK")

# Identify the relevant file(s) in that folder
drive_zootrt <- googledrive::drive_ls(path = drive_folder) %>% 
  dplyr::filter(name == "trait_dataset_level2-2023-09-14.csv")

# Check that worked
drive_zootrt

# Download it
purrr::walk2(.x = drive_zootrt$id, .y = drive_zootrt$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, 
                                                path = file.path("data", "traits", .y)))

# Clear environment
rm(list = ls()); gc()

# End ----
