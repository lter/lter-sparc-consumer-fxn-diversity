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
# Load & Prepare Traits ----
## --------------------------- ##

# Read the trait data in too
trt_v1 <- read.csv(file.path("Data", "traits_tidy-data", "12_traits_wrangled.csv"))

# Check structure
dplyr::glimpse(trt_v1)

# Pare down to just needed information
trt_v2 <- trt_v1 |> 
  dplyr::select(-source, -genus, -taxonomic.resolution, -taxon) |> 
  dplyr::distinct() |> 
  # And remove any traits lacking a scientific name
  dplyr::filter(!is.na(scientific_name) & nchar(scientific_name) != 1)

# Check structure
dplyr::glimpse(trt_v2)

## --------------------------- ##
# Load & Prepare Species List ----
## --------------------------- ##

# Read in the master species list data
spp_v1 <- read.csv(file.path("Data", "species_tidy-data", "23_species_master-spp-list.csv"))

# Check structure
dplyr::glimpse(spp_v1)

# Pare down to just needed information
spp_v2 <- spp_v1 |> 
  dplyr::select(scientific_name) |> 
  dplyr::distinct()

# Check structure
dplyr::glimpse(spp_v2)

# Identify any species for which we have no trait information
spp_no.trait <- spp_v2 |> 
  dplyr::filter(scientific_name %in% unique(trt_v2$scientific_name))

# Any in this category? (hopefully not)
nrow(spp_no.trait)

## --------------------------- ##
# Assess Coverage ----
## --------------------------- ##

# Add species found in the species list but not the trait data
cvg_v1 <- dplyr::bind_rows(trt_v2, spp_no.trait)

# Check structure
dplyr::glimpse(cvg_v1)

# Identify which species are found in the trait data
cvg_v2 <- cvg_v1 |> 
  dplyr::mutate(in_spp_list = ifelse(scientific_name %in% unique(spp_v2$scientific_name),
    yes = "IN DATA", no = NA), 
    .before = sex)

# Check structure
dplyr::glimpse(cvg_v2)

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make a final object
cvg_v99 <- cvg_v2

# Check structure
dplyr::glimpse(cvg_v99)

# Identify the file name & path
cvg_file <- "diagnose_species-trait-coverage-check.csv"
cvg_path <- file.path("Data", "mixed_tidy-data", cvg_file)

# Export locally
write.csv(x = cvg_v99, na = '', row.names = F, file = cvg_path)

# End ----