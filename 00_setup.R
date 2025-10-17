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

# Identify all data types
data_types <- c("community", "traits", "environmental", "species")

# Create 'raw' and 'tidy' subfolders of "Data/" for each data type
purrr::walk(.x = paste0(data_types, sort(rep(c("_raw-data", "_tidy-data"), 
                                               times = length(data_types)))),
            .f = ~ dir.create(path = file.path("Data", .x),
                              showWarnings = F, recursive = T))

# Make a folder for the data keys as well
dir.create(path = file.path("Data", "-keys"), showWarnings = F)

# Clear environment & collect garbage
rm(list = ls()); gc()

# End ----
