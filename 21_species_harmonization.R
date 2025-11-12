## ---------------------------------------------------- ##
# CFD - Species Harmonization
## ---------------------------------------------------- ##
# Purpose:
## Harmonize terrestrial species lists

# Load libraries
librarian::shelf(tidyverse, ltertools, supportR)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Load & Check Key ----
## --------------------------- ##

# Read in species key
species_key_v0 <- read.csv(file = file.path("Data", "-keys", "species_datakey.csv"))

# Check that all projects called in 
unique(species_key_v0$project); length(unique(species_key_v0$project))

# Check key to make sure desired columns are intact 
species_key <- ltertools::check_key(key = species_key_v0)

# Check structure
dplyr::glimpse(species_key)
## View(species_key)

## --------------------------- ##
# Harmonize Species ----
## --------------------------- ##

#Harmonize terrestrial species list 
species_v1 <- ltertools::harmonize(key = species_key, data_format = "csv",
                                   raw_folder = file.path("Data", "species_raw-data"))

# Check structure
dplyr::glimpse(species_v1)
## View(species_v1)

## ----------------------------- ##
# Attach Metadata from Key ----
## ----------------------------- ##

# Grab desired metadata stored in data key 
species_meta <- species_key_v0 %>%
  dplyr::select(project, data_type, habitat, source) %>%
  dplyr::distinct()

# Re-check that all projects called in 
unique(species_meta$project); length(unique(species_meta$project))

# Check structure
dplyr::glimpse(species_meta)

# Attach species meta data using 'source' column
species_v2 <- species_v1 %>%
  dplyr::left_join(y = species_meta, by = "source") %>%
  dplyr::relocate(project:habitat, .before = source)

# Check that all projects called in 
unique(species_v2$project); length(unique(species_v2$project))

# Check structure
dplyr::glimpse(species_v2)

## ---------------------------- ##
# Export ----
## ---------------------------- ##

# Make a final object 
species_v99 <- species_v2 

# Check structure
dplyr::glimpse(species_v99)

# Identify file name & path
species_file <-"21_species_harmonized.csv"
species_path <- file.path("Data", "species_tidy-data", species_file)

# Export locally
write.csv(x = species_v99, na = '', row.names = F, file = species_path)

# End ----
