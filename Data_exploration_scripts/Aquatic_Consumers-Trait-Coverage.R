

#####   CFD Working Group ##########

# Aquatic Consumers Trait Coverage ###

####################################

#install.packages('funbiogeo', repos = c('https://frbcesab.r-universe.dev', 'https://cloud.r-project.org'))

library(librarian)

shelf(tidyverse, funbiogeo, ggplot2)

# Get set up
source("00_setup.R")

#call in data 

aqu_spp_trt_table_v1 <- read.csv(file.path("Data", "mixed_tidy-data", "aquatic-trait-species-table-coverage-check.csv"))


aqu_spp_trt_table_v2 <- aqu_spp_trt_table_v1 %>%
  dplyr::select(-source, -epithet, -taxonomic.resolution, -taxon, -order, -family.x, -genus.x) %>%
  dplyr::rename(species = scientific_name)%>%
  dplyr::mutate(across(where(is.character), ~na_if(., "")))

#select traits columns of interest 

aqu_spp_trt_table_v3 <- aqu_spp_trt_table_v2 %>%
  dplyr::select(species, length_adult.max_cm, mass_adult_g, reproduction_reproductive.mode.ordinal,
                diet_trophic.level_num, active.time_category_ordinal, diet_trophic.level.specific_ordinal,
                age_life.span_years, age_maturity_years, reproduction_fecundity_num, length_offspring_cm)
  

#plot number of species per traits 

aqu_plot <-fb_plot_number_species_by_trait(aqu_spp_trt_table_v3)

aqu_plot + theme(axis.text.x =element_text(color="black"),
                 axis.text.y = element_text(color="black"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())

fb_plot_species_traits_missingness(aqu_spp_trt_table_v3, all_traits = TRUE)
