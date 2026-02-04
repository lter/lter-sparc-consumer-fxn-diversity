
#######################
#CFD - Trait Coverage
#######################

#Purpose: Visulize trait coverage of all consumer groups  

# Load libraries
librarian::shelf(tidyverse, dplyr, funbiogeo, ggplot2)
                 
# Get set up
source("00_setup.R")
                 
# Clear environment & collect garbage
rm(list = ls()); gc()                 
                 
                 
########################################
##### Load Imputed Trait Database 
########################################

impu_traits <- read.csv(file.path("Data", "traits_tidy-data","consumer-trait-species-imputed-taxonmic-database.csv"))



########################################
##### Master Species List 
########################################


master_sp_list <-  read.csv(file.path("Data", "species_tidy-data", "23_species_master-spp-list.csv"))
#program_sp_list <- read.csv("/Users/shalandagrier/Documents/program-distinct-sci-names.csv")


#############filter for all consumers and/or consumer groups ############
con_sp_list <-master_sp_list %>%
  dplyr::select(project, scientific_name) #%>%
  #dplyr::filter(project %in% c("CoastalCA", "FCE","SBC", "MCR", "VCR", "RLS", "FISHGLOB")) %>%
  #dplyr::filter(habitat %in% c("ocean")) #fish
  #dplyr::filter(project %in% c("CCE", "Arctic", "Palmer", "NGA", "North Lakes")) %>% #zooplankton 
  #dplyr::filter(project %in% c("KBS_INS", "KONZA")) %>% #insects 
  #dplyr::filter(project %in% c("MOHONK" ,"KBS_AMP")) %>%#amphibians
  #dplyr::filter(project %in% c("KBS_MAM", "SEV")) %>%#mammals 
  #dplyr::filter(project %in% c("HARVARD", "SBC_BEACH", "KBS_BIR")) #Birds 



#join now imputed trait data with consumer species list  

con_sp_traits_v1 <- dplyr::left_join(con_sp_list, impu_traits, by = "scientific_name") %>%
  dplyr::distinct(scientific_name, .keep_all =TRUE) %>%
  dplyr::mutate(active.time_category_ordinal = na_if(active.time_category_ordinal, "")) %>%
  dplyr::mutate(diet_trophic.level.broad_ordinal = na_if(diet_trophic.level.broad_ordinal, "")) %>%
  dplyr::mutate(diet_trophic.level.specific_ordinal = na_if(diet_trophic.level.specific_ordinal, "")) %>%
  dplyr::mutate(diet_trophic.level_ordinal = na_if(diet_trophic.level_ordinal, "")) %>%
  dplyr::mutate(reproduction_reproductive.mode_ordinal = na_if(reproduction_reproductive.mode_ordinal, ""))%>%
  dplyr::mutate(diet_trophic.level_ordinal = tolower(diet_trophic.level_ordinal))
  

con_sp_tratis_v2 <- con_sp_traits_v1 %>%
  dplyr::rename(species = scientific_name)
#-source,-project)

#focus on selected traits 
con_sp_tratis_v3 <- con_sp_tratis_v2  %>%
  dplyr::select(species, age_life.span_years,range_area_km2, age_maturity_years, active.time_category_ordinal, 
                reproduction_reproductive.mode_ordinal, diet_trophic.level_ordinal,mass_adult_g,
                diet_trophic.level.specific_ordinal, metabolism_metabolic.rate_oxygen.per.hour,
                metabolism_metabolic.rate_W, length_adult.max_cm, reproduction_fecundity_num, 
                length_offspring_cm,length_adult_cm,
                length_max_cm, reproduction_fecundity_num, diet_trophic.level_num)

#visualize trait and species coverage 

con_plot <-fb_plot_number_species_by_trait(con_sp_tratis_v3 )

con_plot + theme(axis.text.x =element_text(color="black"),
                 axis.text.y = element_text(color="black"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())

fb_plot_species_traits_missingness(con_sp_tratis_v3 , all_traits = TRUE)

#zoo_sp_list <- master_sp_list %>%
#  dplyr::filter(project %in% c("Arctic", "NorthLakes", "Palmer")) %>%
#  dplyr::select(project, habitat, raw_filename, scientific_name, kingdom,
#                phylum, class, order, family, genus)

#write.csv(zoo_sp_list, "/Users/shalandagrier/Documents/Post Doc Projects/freshwater_zooplankton_sp_list.csv")


