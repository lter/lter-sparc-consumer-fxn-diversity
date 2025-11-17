#######################
#CFD - Aquatic  Trait Coverage
#######################

#Purpose: Add all aquatic trait data 
#Identify the proportion of missing data for each trait across aquatic sites  

# Load libraries
librarian::shelf(tidyverse)



# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()


###################
#Load Consumer Data 
##################


# Read in aquatic consumer species data 
aqu_con_v1 <- read.csv(file.path("Data", "community_tidy-data", "04_harmonized_consumer_excretion_sparc_cnd_site.csv"))

# Fliter desired columns 
aqu_con_v2 <- aqu_con_v1 |> 
  dplyr::select(project, habitat, raw_filename, scientific_name, 
                class,order, family, genus) |> 
  dplyr::group_by(project) |>
  dplyr::distinct()

# Check structure
#dplyr::glimpse(aqu_con_v2)


#distinct_spp_names <- aqu_con_v1 %>%
  #select(scientific_name) %>%
  #distinct() #1598 species 

  

#########################
#Load trait data
#######################

# Read the trait data 
trt_data_v1 <- read.csv(file.path("Data", "traits_tidy-data", "12_traits_wrangled.csv"))

#Read in SPARC trait data 

sparc_trt_data_v1 <- read_csv("Data/traits_raw-data/SPARC_fish_spp-traits-fishbase-and-lit.csv") 

sparc_trt_data_v2 <- sparc_trt_data_v1 %>%
  dplyr::rename(reproduction_fecundity.max_num = reproduction_fecundity.max)

# Check structure
#dplyr::glimpse(trt_data_v1)
#dplyr::glimpse(sparc_trt_data_v1)

# Pare down to just needed information
trt_data_v2 <- trt_data_v1  |> 
  dplyr::filter(source %in% c("FishTraits_14.csv","trait_dataset_level2-2023-09-14.csv", 
                              "TraitCollectionFishNAtlanticNEPacificContShelf-Traits.csv",
                              "Junker_et_al._dataFinal.csv"))
 

# Check structure
#dplyr::glimpse(trt_data_v2)


# Grab all traits that do not have a scientific name 
no_sci_name <-trt_data_v2 %>%
  dplyr::distinct() %>%
  dplyr::filter(is.na(scientific_name) | scientific_name =="")


# Remove all traits that do not have scientific_name from trait database 
trt_data_v3 <- trt_data_v2 %>%
  dplyr::filter(scientific_name != "") %>%
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(. %in% c("-1", "-555", "-999", ""),
                                              yes = NA, no = .))) %>%#remove NA indicators and go back and do this for trait wrangling as well 
  dplyr::mutate(scientific_name = str_replace_all(scientific_name, "\\.", " "))

trt_data_v3$active.time_category_ordinal <- as.character(trt_data_v3$active.time_category_ordinal)
trt_data_v3$life.stage_specific_ordinal <- as.character(trt_data_v3$life.stage_specific_ordinal)


#########

# Identify any species for which we have no trait information
no.trait <- aqu_con_v2|> 
  dplyr::filter(scientific_name %in% unique(trt_data_v3$scientific_name))

# Any in this category? (hopefully not)
nrow(no.trait) #491 species no trait data out of 1598

#calculate averages of trait values for duplicate species entries per source for all numeric trait values 
mean_traits <- trt_data_v3 %>% 
  dplyr::group_by(source, scientific_name) %>%
  dplyr::distinct() %>%
  dplyr::summarize(across(where(is.numeric),mean, na.rm = TRUE), .groups='drop')%>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))

#### filter for all character columns including all traits 
chr_triats <- trt_data_v3 %>%
  dplyr::select(where(is.character)) %>%
  dplyr::distinct()

chr_triats_v2 <-chr_triats [-c(11:12, 24:25),] 


### numeric and character traits back together 

all_traits_v1 <- merge(mean_traits, chr_triats_v2, by= c("scientific_name", "source"), all.x = TRUE)



length(unique(all_triats$scientific_name))

#### join species and traits table by scientific_name & keep all rows without data 

spp_traits_table <-(aqu_con_v2) %>%
  dplyr::left_join(all_traits_v1, by = "scientific_name") %>%
  dplyr::select(-ends_with(".y"))

#fill in any missing trait data using the traits table from SPARC traits table
#check again this week after meeting fecundity min and max numbers don't make sense! recheck fishbase code later.  

spp_traits_table_v2 <- dplyr::left_join(spp_traits_table, sparc_trt_data_v2, by = "scientific_name", suffix = c("", "_y")) %>%
  mutate(
    age_life.span_years = coalesce(age_life.span_years, age_life.span_years_y),
    age_maturity.min_years = coalesce(age_maturity.min_years, age_maturity.min_years_y),
    age_maturity.max_years = coalesce(age_maturity.max_years, age_maturity.max_years_y),
    age_maturity_years = coalesce(age_maturity_years, age_maturity_years_y),
    reproduction_fecundity.min_num = coalesce(reproduction_fecundity.min_num, reproduction_fecundity.min_num_y),
    reproduction_fecundity_num = coalesce(reproduction_fecundity_num, reproduction_fecundity_num_y),
    reproduction_fecundity.max_num = coalesce(reproduction_fecundity.max_num, reproduction_fecundity.max_num_y),
    diet_trophic.level_num = coalesce(diet_trophic.level_num, diet_trophic.level_num_y),
    length_adult.max_cm = coalesce(length_adult.max_cm, length_adult.max_cm_y),
    mass_adult_g = coalesce(mass_adult_g, mass_adult_g_y),
    metabolism_metabolic.rate_oxygen.per.hour = coalesce(metabolism_metabolic.rate_oxygen.per.hour, metabolism_metabolic.rate_oxygen.per.hour_y),
    diet_trophic.level.specific_ordinal = coalesce(diet_trophic.level.specific_ordinal, diet_trophic.level.specific_ordinal_y),
    active.time_category_ordinal = coalesce(active.time_category_ordinal, active.time_category_ordinal_y)
  ) %>%
  select(-ends_with("_y"))



## --------------------------- ##
# Export ----
## --------------------------- ##

# Make a final object
aqu_traits_v99 <-spp_traits_table_v2 #note to self: update this later to add family info for zooplankto species 

# Check structure
dplyr::glimpse(aqu_traits_v99)

# Identify the file name & path
aqu_traits_file <- "diagnose_species-trait-coverage-check.csv"
aqu_traits_path <- file.path("Data", "mixed_tidy-data", aqu_traits_file)

# Export locally
write.csv(x = aqu_traits_v99, na = '', row.names = F, file = aqu_traits_path)

# End ----






####trait coverage for aquatic consumers  

trait_ungroup <- spp_traits_table %>%
  ungroup() %>%
  select(-project) 
  
  
trait_cov <- trait_ungroup %>%
  dplyr::select(-habitat, -raw_filename, -class, -order, -family.x, -genus.x, -family.y, -genus.y, -taxon, 
                -epithet, -taxonomic.resolution, -migration)%>%
  dplyr::distinct(scientific_name, .keep_all = TRUE) %>%
  ungroup()

#Calulate the proportion of missing data for each trait 

trait_columns <- trait_cov[,c(3:46)]

#trait_prop_lifespan <- trait_cov %>%
 # summarise(
   # non_na_count = sum(!is.na(trait_cov$age_life.span_years)),
   # total_count = n(),
   # percentage_available = (non_na_count / total_count) * 100
 # )

all_trait_prop <- trait_cov %>%
  select(3:46) %>% # Select columns by number
  summarise(
    across(everything(), ~sum(!is.na(.))), # Count non-NA values
    .names = "{.col}_count"
  ) %>%
  bind_cols(
    trait_cov %>%
      select(3:46) %>%
      summarise(
        across(everything(), ~mean(!is.na(.)) * 100), # Calculate percentage of non-NA values
        .names = "{.col}_percentage"
      )
  ) 


trait_cov_per <- all_trait_prop[,c(44:87)]
#duplicate_rows <- chr_triats %>%
  #group_by(scientific_name) %>%
  #filter(n() > 1) %>%
  #ungroup()
  

unique(trait_cov$life.stage_specific_ordinal)






















