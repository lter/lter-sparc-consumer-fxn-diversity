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

# Create top-level folders
dir.create(path = file.path("Data"), showWarnings = F)

# Make some useful, one-off "Data/" subfolders
dir.create(path = file.path("Data", "-keys"), showWarnings = F)
dir.create(path = file.path("Data", "checks"), showWarnings = F)

# Create 'raw' and 'tidy' subfolders of "Data/" for each data type
## Identify all data types
data_types <- c("community", "traits", "environmental", "species")
## Actually make folders
purrr::walk(.x = paste0(data_types, sort(rep(c("_raw-data", "_tidy-data"), 
                                               times = length(data_types)))),
            .f = ~ dir.create(path = file.path("Data", .x),
                              showWarnings = F, recursive = T))


# Clear environment & collect garbage
rm(list = ls()); gc()

# End ----
