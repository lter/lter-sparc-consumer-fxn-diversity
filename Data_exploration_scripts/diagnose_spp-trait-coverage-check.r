## ---------------------------------------------------- ##
# CFD - Species' Trait Coverage Checks
## ---------------------------------------------------- ##
# Purpose:
## Compare 'master species list' against the species included in the trait data

# Load libraries
librarian::shelf(tidyverse)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Load & Prepare Species List ----
## --------------------------- ##

# Read in the master species list data
spp_v1 <- read.csv(file.path("Data", "species_tidy-data", "23_species_master-spp-list.csv"))

# Check structure
dplyr::glimpse(spp_v1)

## --------------------------- ##
# Load & Prepare Traits ----
## --------------------------- ##

# Read the trait data in too
trt_v1 <- read.csv(file.path("Data", "traits_tidy-data", "12_traits_wrangled.csv"))

# Check structure
dplyr::glimpse(trt_v1)

## --------------------------- ##
# Join Them ----
## --------------------------- ##

# Do the joining
cvg_v1 <- dplyr::full_join(x = spp_v1, y = trt_v1, by = "scientific_name")

# Check structure
dplyr::glimpse(cvg_v1)

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make a final object
cvg_v99 <- cvg_v1

# Check structure
dplyr::glimpse(cvg_v99)

# Identify the file name & path
cvg_file <- "diagnose_species-trait-coverage-check.csv"
cvg_path <- file.path("Data", "mixed_tidy-data", cvg_file)

# Export locally
write.csv(x = cvg_v99, na = '', row.names = F, file = cvg_path)






# End ----