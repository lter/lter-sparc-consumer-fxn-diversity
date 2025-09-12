#----------------------------------##
# SPARC - Consumer Functional Diversity (CFD) 
###--------------------------------##

# Script author(s): Shalanda Grier, Li Kui, Nick Lyons
# Sites: Palmer LTER, Arctic LTER, North Lakes LTER

## ---------------------------------------------------- ##
# CFD - Step 2A: Subset Zookplankton Species List
## ---------------------------------------------------- ##
# Purpose:
## Attach zooplankton trait information to the harmonized data

# Load libraries
librarian::shelf(tidyverse, supportR)

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Load Data ----
## --------------------------- ##

# Read in the harmonized data
sub_v1 <- read.csv(file = file.path("data", "01_cfd-harmonize.csv"))

# Check structure
dplyr::glimpse(sub_v1)

# Also get the zooplankton trait table (by taxon)
zootrt_v1 <- read.csv(file = file.path("data", "traits", "trait_dataset_level2-2023-09-14.csv"))

# Check its structure
dplyr::glimpse(zootrt_v1)

## --------------------------- ##
# Prep the Zooplankton Trait Info ----
## --------------------------- ##

# To join the Zooplankton trait data, we need it to have one row per species
zootrt_v2 <- zootrt_v1 %>% 
  # Pare down to only desired columns
  dplyr::select(scientificName, traitName:traitUnit, valueType) %>% 
  dplyr::distinct() %>% 
  # And only desired traits
  dplyr::filter(valueType == "numeric") %>% 
  dplyr::filter(traitName %in% c("dryWeight")) %>% 
  # Combine trait category with units
  dplyr::mutate(trait_actual = paste0(traitName, "__", traitUnit)) %>% 
  # Ditch superseded columns and pivot wider
  dplyr::select(-valueType, -traitName, -traitUnit) %>% 
  tidyr::pivot_wider(names_from = trait_actual, values_from = traitValue)

# Check structure
dplyr::glimpse(zootrt_v2)

## --------------------------- ##
# Compare Taxa ----
## --------------------------- ##

# Before we can combine by species, we need to make sure the species are entered the same
setdiff(x = unique(sub_v1$species), y = unique(zootrt_v1$scientificName))

# Using the above values, we can edit them to synonymize as needed
sub_v2 <- sub_v1 %>% 
  dplyr::mutate(species = dplyr::case_when(
    
    ## If not edited above, just keep the original species name in the data
    T ~ species))


# Re-check overlap / difference between data and trait table
setdiff(x = unique(sub_v2$species), y = unique(zootrt_v1$scientificName))

## --------------------------- ##
# Join Data ----
## --------------------------- ##

# Join on the trait information!
sub_v3 <- sub_v2 %>% 
  dplyr::left_join(y = zootrt_v2, by = c("species" = "scientificName"))

# Re-check structure
dplyr::glimpse(sub_v3)

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make a final object
sub_v99 <- sub_v3

# Double check its structure
dplyr::glimpse(sub_v99)

# Make a filename for it
sub_file <- "02a_traits_harmonized.csv"

# Export
write.csv(x = sub_v99, row.names = F, na = '',
          file = file.path("data", sub_file))

# End ----
