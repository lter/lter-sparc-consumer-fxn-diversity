################################################################################
##
## Script to explore traits distribution across taxa and programs for the
## in-person meeting
## Taxa: Fish, Mammals, Amphibians
## Traits: MAss, Life Span, Trophic Level (num), Reproduction Rate, Active Time
##
## Camille Magneville
##
## 02/2026
##
################################################################################


# 0 - Set up ===================================================================

# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`

# Load libraries
librarian::shelf(tidyverse, dplyr, funbiogeo, ggplot2)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()


# 1 - Load data ================================================================


# Load master species list for all programs:
sp_pro_list <- read.csv(file.path("Data", "species_tidy-data", "23_species_master-spp-list.csv"))

# Load imputed traits:
imp_tr_df <- read.csv(file.path("Data", "traits_tidy-data", "consumer-trait-species-imputed-taxonmic-database.csv"))


# 2 - Check traits and clean ===================================================


# ----

# Subset to the studied traits and taxa:
subset_tr_df <- imp_tr_df %>% 
  dplyr::select(c("scientific_name",
                  "age_life.span_years",
                  "diet_trophic.level_num",
                  "reproduction_reproductive.rate_num.offspring.per.year",
                  "mass_adult_g",
                  "active.time_category_ordinal")) %>% 
  dplyr::rename(species = "scientific_name")

# Check traits cover:
funbiogeo::fb_plot_number_species_by_trait(subset_tr_df)
funbiogeo::fb_plot_number_traits_by_species(subset_tr_df)


# ----

# Check traits type:
class(subset_tr_df$age_life.span_years)
class(subset_tr_df$mass_adult_g)
class(subset_tr_df$diet_trophic.level_num)

# Order the Active time categories:
class(subset_tr_df$active.time_category_ordinal)
levels(subset_tr_df$active.time_category_ordinal)
unique(subset_tr_df$active.time_category_ordinal)
# Correct "" to NA:
subset_tr_df$active.time_category_ordinal[which(subset_tr_df$active.time_category_ordinal == "")] <- NA
unique(subset_tr_df$active.time_category_ordinal)
# Categorise:
subset_tr_df$active.time_category_ordinal <- as.factor(subset_tr_df$active.time_category_ordinal)


# ----

## Check numeric traits distribution, correct and log when needed:

# LIFESPAN:
hist(subset_tr_df$age_life.span_years)

# Correct some sp have - or 0 lifespan and homosapiens is in the db (?):
# CHANGE LATER: Remove data for these species (look for it later) and rm humans:
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Hemichromis letourneuxi")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Channa marulius")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Gambusia holbrooki")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Lucania parva")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Chrysiptera brownriggii")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Chrysiptera glauca")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Chrysiptera notialis")] <- NA
subset_tr_df <- subset_tr_df[which(subset_tr_df$species != "Homo sapiens"), ]
hist(subset_tr_df$age_life.span_years, breaks = 100)

# Log transform:
subset_tr_df2 <- subset_tr_df
subset_tr_df2$age_life.span_years <- log10(subset_tr_df2$age_life.span_years +1)
hist(subset_tr_df2$age_life.span_years, breaks = 50)


# MASS:
hist(subset_tr_df2$mass_adult_g, breaks = 50)

# Log transform:
subset_tr_df2$mass_adult_g <- log10(subset_tr_df2$mass_adult_g +1)
hist(subset_tr_df2$mass_adult_g, breaks = 50)


# TROPHIC LEVELS: keep it as it is
hist(subset_tr_df$diet_trophic.level_num, breaks = 50)


# 3 - Link traits to program and only keep fish/mammals/amphib =================





# 4 - Explore trats distribution across programs and taxa ======================




# After:
# For the functional space, remove reproductive rate (low coverage):

