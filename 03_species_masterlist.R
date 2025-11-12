## ---------------------------------------------------- ##
# CFD - Species Master List Composition 
## ---------------------------------------------------- ##
# Purpose:
## Join species name from both aquatic and terrestrial sites 

# Script author(s): Shalanda Grier, Li Kui, Nick Lyons


#Load libraries
librarian::shelf(tidyverse)


# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()


#Read in data 

#excretion data 
aqu_v1 <- read.csv(file.path("Data", "community_tidy-data", "04_harmonized_consumer_excretion_sparc_cnd_site.csv"))

#glimpse(aqu_v1)

aqu_v2 <- aqu_v1 %>%
  dplyr::select(project, habitat, raw_filename, scientific_name, 
                kingdom, phylum, class, order, family, genus)

aqu_ready <- aqu_v2 %>% 
  group_by(project) %>%
  distinct(scientific_name, .keep_all = TRUE) #keep all distinct names for each project #check this again!


#Terrestrial species names data 

ter_v1 <- read.csv(file.path("Data", "species_tidy-data", "02_species_wrangled.csv"))

ter_v2 <- ter_v1 %>%
  dplyr::rename(raw_filename = source) %>%
  dplyr::select(project, habitat, raw_filename, scientific_name, 
                kingdom, phylum, class, order, family, genus)
  

ter_ready <- ter_v2


#combine aquatic and terrestrial consumer scientific_names to create a master list for all sites 

master_list <- rbind(aqu_ready, ter_ready)

# Make one last version 

master_list_v99 <- master_list


# Identify the file name & path
master_list_file <- "03_species_masterlist.csv"
master_list_path <- file.path("Data", "species_tidy-data", master_list_file)

# Export locally
write.csv(x = master_list_v99, na = '', row.names = F, file = master_list_file)


# End ----
