## ---------------------------------------------------- ##
# CFD - Species Master List Composition 
## ---------------------------------------------------- ##
# Purpose:
## Join species name from both aquatic and terrestrial sites 

# Load libraries
librarian::shelf(tidyverse)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Load Data ----
## --------------------------- ##

# Read in excretion data
aqu_v1 <- read.csv(file.path("Data", "community_tidy-data", "04_harmonized_consumer_excretion_sparc_cnd_site.csv"))

# Streamline this to just what we need here
aqu_v2 <- aqu_v1 |> 
  dplyr::select(project, habitat, raw_filename, scientific_name, 
                kingdom, phylum, class, order, family, genus) |> 
  dplyr::distinct()

# Check structure
dplyr::glimpse(aqu_v2)

# Terrestrial species names data 
ter_v1 <- read.csv(file.path("Data", "species_tidy-data", "22_species_wrangled.csv"))

# Streamline this to just what we need here
ter_v2 <- ter_v1 |> 
  dplyr::rename(raw_filename = source) |> 
  dplyr::select(project, habitat, raw_filename, scientific_name, 
                kingdom, phylum, class, order, family, genus) |> 
  dplyr::distinct()

# Check structure
dplyr::glimpse(ter_v2)

## --------------------------- ##
# Combine Data ----
## --------------------------- ##

# Combine the two species lists to get a single master list
master_v1 <- dplyr::bind_rows(aqu_v2, ter_v2)

# Check structure
dplyr::glimpse(master_v1)


# check for duplicates
unique_spe <- master_v1 %>%
  distinct(project, scientific_name, kingdom, phylum, class, order, family, genus) 

duplicate <- unique_spe %>%
  distinct(scientific_name, kingdom, phylum, class, order, family, genus) %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::filter(count > 1)%>%
  #merge back to get the project names
  left_join(unique_spe, by = "scientific_name") 

#write.csv(x = duplicate, na = '', row.names = F, file = file.path("Data", "species_tidy-data", "23_species_master-spp-list_duplicates.csv"))

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make one last version 
master_v99 <- master_v1

# Check structure
dplyr::glimpse(master_v99)

# Identify the file name & path
master_file <- "23_species_master-spp-list.csv"
master_path <- file.path("Data", "species_tidy-data", master_file)

# Export locally
write.csv(x = master_v99, na = '', row.names = F, file = master_path)

# End ----
