## ---------------------------------------------------- ##
# CFD - Species Harmonization
## ---------------------------------------------------- ##
# Purpose:
##  "harmonized" terrestrial species names 


# Load libraries
librarian::shelf(tidyverse, ltertools, supportR)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()


# Read in species key
species_key_v0 <- read.csv(file = file.path("Data", "-keys", "species_datakey.csv"))

# Check key to make sure desired columns are intact 
species_key <- ltertools::check_key(key = species_key_v0)

#View(species_key)

# Harmonize terrestrial species list 

species_v1 <- ltertools::harmonize(key= species_key, data_format = "csv",
                                   raw_folder = file.path("Data", "species_raw-data"))

#View(species_v1)

## ----------------------------- ##
#  Add on Key Metadata ----
## ----------------------------- ##


#Grab desired metadata stored in data key 

species_meta <- species_key_v0 %>%
  dplyr::select(project, data_type, habitat, source) %>%
  dplyr::distinct()

#View(species_meta)

#Attachthe species meta data using 'source'column

species_v2 <- species_v1 %>%
  dplyr::left_join(y = species_meta, by = "source") %>%
  dplyr::relocate(project:habitat, .before = source)

########### end #############

## ---------------------------- ##
# Export ----
## ---------------------------- ##

# Make a final object 

species_v99 <- species_v2 


# Identify file name & path
species_file <-"01_species_harmonized.csv"
species_path <- file.path("Data", "species_tidy-data", species_file)

# Export locally
write.csv(x = species_v99, na = '', row.names = F, file = species_path)
