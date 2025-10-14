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

# Clear environment & collect garbage
rm(list = ls()); gc()

# Create needed folders
source("00_setup.R")

## --------------------------- ##
# Download Keys ----
## --------------------------- ##

# Grab relevant Drive folder URL
key_drive <- googledrive::as_id("https://drive.google.com/drive/folders/1of2tKXcU_KnNDZrYLt3b3smObNrG_aXD")

# List items in that folder
(key_files <- googledrive::drive_ls(path = key_drive))

# Download them to the relevant local folder
purrr::walk2(.x = key_files$id, .y = key_files$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, type = "csv",
                                                path = file.path("data", "00_keys", .y)))

## --------------------------- ##
# Download Raw Community Data ----
## --------------------------- ##

# Grab relevant Drive folder URL
comm_drive <- googledrive::as_id("https://drive.google.com/drive/folders/1n6iqs3aK2xWkROI8nPSVfk6ZkYPNpF7F")

# List items in that folder
(comm_files <- googledrive::drive_ls(path = comm_drive))

# Download them to the relevant local folder
purrr::walk2(.x = comm_files$id, .y = comm_files$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, type = "csv",
                                                path = file.path("data", "01_community_raw_data", .y)))

## --------------------------- ##
# Download Raw Trait Data ----
## --------------------------- ##

# Grab relevant Drive folder URL
trt_drive <- googledrive::as_id("https://drive.google.com/drive/folders/1UAq72kFD8Hh9uV1_He1ijJQ1m4lVg7eK")

# List items in that folder
(trt_files <- googledrive::drive_ls(path = trt_drive, pattern = ".csv"))

# Download them to the relevant local folder
purrr::walk2(.x = trt_files$id, .y = trt_files$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, type = "csv",
                                                path = file.path("data", "11_traits_raw_data", .y)))

## --------------------------- ##
# Download Environmental Data ----
## --------------------------- ##

# Grab relevant Drive folder URL
env_drive <- googledrive::as_id("https://drive.google.com/drive/folders/1yUg4tYF7F-fuamODUOy55JUg8tmaUyYD")

# List items in that folder
(env_files <- googledrive::drive_ls(path = env_drive))

# Download them to the relevant local folder
purrr::walk2(.x = env_files$id, .y = env_files$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, type = "csv",
                                                path = file.path("data", "21_environmental_raw_data", .y)))

# End ----
