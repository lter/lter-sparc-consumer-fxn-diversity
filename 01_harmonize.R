## ---------------------------------------------------- ##
# CFD - Step 1: Harmonization
## ---------------------------------------------------- ##
# Purpose:
## Standarize and combine (i.e., "harmonize") input datasets

# Load libraries
librarian::shelf(tidyverse, ltertools)

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Read in Key
## --------------------------- ##

# Read in the data key
key_v1 <- read.csv(file = file.path("data", "CFD_Datakey.csv"))

# Check structure
dplyr::glimpse(key_v1)

# Make sure it's ready for use with `ltertools::harmonize()`
key_v2 <- ltertools::check_key(key = key_v1)

# Re-check structure
dplyr::glimpse(key_v2)

## --------------------------- ##
# Harmonize Data ----
## --------------------------- ##

# Harmonize the data!
combo_v1 <- ltertools::harmonize(key = key_v2, quiet = F,
                                 raw_folder = file.path("data", "raw"))

# Check structure
dplyr::glimpse(combo_v1)

## --------------------------- ##
# Add on Key Metadata ----
## --------------------------- ##

# Grab some important metadata stored in the key
key_meta <- key_v1 %>% 
  dplyr::select(project, data_type, habitat, source) %>% 
  dplyr::distinct()

# Check that out
key_meta

# Attach that to the combined data using the 'source' column
combo_v2 <- combo_v1 %>% 
  dplyr::left_join(y = key_meta, by = "source") %>% 
  dplyr::relocate(project:habitat, .before = source)

# Check structure
dplyr::glimpse(combo_v2)

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make a final object
combo_v99 <- combo_v2

# Double check its structure
dplyr::glimpse(combo_v99)

# Make a filename for it
combo_file <- "01_cfd-harmonize.csv"

# Export
write.csv(x = combo_v99, row.names = F, na = '',
          file = file.path("data", combo_file))

# End ----
