########################
#CFD Fish Traits Table 
#######################


#Purpose: Combine fish traits from FishBase and fish trait data from peer reviewed papers to create a fish database for fish species found across working 
#         group data sets (SPARC fish species). 

#Note: Will retroactively harmonize later to make reproducible. 


# Load libraries
librarian::shelf(tidyverse, rfishbase)


### Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()


###################
#Load Consumer Data 
##################


#rename dataframes 

# Read in aquatic consumer species data 
data_v1 <- read.csv(file.path("Data", "community_tidy-data", "04_harmonized_consumer_excretion_sparc_cnd_site.csv"))

# Fliter desired columns 
data_v2 <- data_v1 |> 
  dplyr::select(project, habitat, raw_filename, scientific_name, 
                class,order, family, genus) |> 
  dplyr::group_by(project) |>
  dplyr::distinct() #1801 distinct rows names 

# Check structure
dplyr::glimpse(data_v2)


spp_names_distinct <- data_v1 %>%
  select(scientific_name) %>%
  distinct() #1598 distinct species 

#################### Fish Species Names ##########################

## Gather all distinct species names from sites with fish consumers 

####################################################################


spp_names_v1 <- data_v2 %>%
  filter(project %in% c("MCR", "SBC", "FCE", "CoastalCA", "VCR", "PIE", "RLS", "FISHGLOB")) %>%
  select(project, scientific_name) %>%
  distinct() # 1356 fish species so that leaves about 242 zooplankton taxa 

unique(spp_names_v1$project)
#isolate distinct species names 

spp_names_v2 <- spp_names_v1$scientific_name

#validate species names against names found in FishBase 

spp_names_val <- validate_names(spp_names_v2) #NA character added for species without matches so now 1357


spp_names_val_df <- as.data.frame(spp_names_val) #turn list of validated names into a data frame. Note: some sp names not validated 

spp_names_val_df2 <- spp_names_val_df %>%
  distinct() %>% #grab unique taxa names only no replicates. There are replicates because some projects have similar species
  dplyr::rename(scientific_name = spp_names_val)
################## end #############################################


################ Traits ############################

#Wrangle Trait Data from FishBase using rfishbase functions 

####################################################


# Gather life history, metabolic, morphological, and diet data from FishBase 

# reproduction 
spp_reproduction_v1 <-reproduction(spp_names_val) 

spp_reproduction_v2 <- spp_reproduction_v1 %>%
  dplyr::select(Species, RepGuild2) %>%
  dplyr::rename(scientific_name = Species, reproduction_reproductive.mode.ordinal = RepGuild2) 

spp_reproduction_ready <- spp_reproduction_v2


#fecundity 
spp_fecundity_v1 <-fecundity(spp_names_val)

spp_fecundity_v2 <-spp_fecundity_v1 %>%
  dplyr::select(Species, FecundityMin, FecundityMax, FecundityMean)%>%
  dplyr::rename(scientific_name = Species) %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  dplyr::rename(reproduction_fecundity.min_num = FecundityMin, reproduction_fecundity.max= FecundityMax, reproduction_fecundity_num = FecundityMean)


spp_fecundity_ready <- spp_fecundity_v2


#maturity 
spp_maturity_v1 <-maturity(spp_names_val) #377 unique species  

#tm is the mean age at maturity reported from FishBase will use age_maturity_years 
#AgeMatMin and AgeMatMin2 represent the range in reported age at maturity for each species
#in some instances there are multiple entries per species so will
#calculate means of each column to get one value per species for each type of reported age at maturity

spp_maturity_v2 <-spp_maturity_v1 %>%
  dplyr::select(Species, AgeMatMin, AgeMatMin2, tm) %>%
  dplyr::rename(age_maturity.min_years= AgeMatMin, age_maturity.max_years= AgeMatMin2, age_maturity_years= tm)

spp_maturity_means <- spp_maturity_v2 %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE))%>%
  dplyr::rename(scientific_name = Species)


spp_maturity_ready <- spp_maturity_means


## popgrowth data 
spp_popdata_v1 <- popgrowth(spp_names_val) %>%
  #tmax is maximum reported age/lifespan in FishBase 
  dplyr::select(Species, tmax)

spp_tmax <- spp_popdata_v1%>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(age_life.span_years = mean(tmax, na.rm =TRUE))%>%
  dplyr::rename(scientific_name = Species)

spp_tmax_ready <- spp_tmax

  
## lengths in cm 
spp_lengths_v1 <- species(spp_names_val, fields = c("Species", "Length")) %>%
  dplyr::rename(length_adult.max_cm = Length)%>%
  dplyr::rename(scientific_name = Species)
 
spp_length_ready <- spp_lengths_v1


### oxygen consumption 
spp_oxygen_v1 <- oxygen(spp_names_val)

spp_oxygen_v2 <- spp_oxygen_v1 %>%
  dplyr::filter(MetabolicLevel == "standard" & AppliedStress == "none specified") %>% #only grab values not attributed to stress experiments (e.g., temp or starvation)
  dplyr::filter(!Comment %in% c("Larval.", "Metamorphosing.", "Yellow eel stage.", "Silver eel stage."))

spp_oxygen_v3 <- spp_oxygen_v2 %>%
  dplyr::group_by(Species)%>%
  dplyr::summarise(metabolism_metabolic.rate_oxygen.per.hour = mean(OxygenCons, na.rm =TRUE))%>%
  dplyr::rename(scientific_name = Species)

spp_oxygen_ready <- spp_oxygen_v3

#diet 

spp_diet_v1 <- diet(spp_names_val)

spp_diet_v2 <- spp_diet_v1 %>%
  dplyr::select(Species, Troph) %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(diet_trophic.level_num = mean(Troph, na.rm= TRUE))%>%
  dplyr::rename(scientific_name = Species)

spp_diet_ready <- spp_diet_v2


###grab species list and merge all traits by scientific name 

fish_spp_traits_rfishbase <- spp_names_val_df2 %>%
  left_join(spp_reproduction_ready, by= "scientific_name") %>%
  left_join(spp_fecundity_ready, by = "scientific_name") %>%
  left_join(spp_maturity_ready, by = "scientific_name") %>%
  left_join(spp_tmax_ready, by = "scientific_name") %>%
  left_join(spp_oxygen_ready, by = "scientific_name") %>%
  left_join(spp_length_ready, by = "scientific_name") %>%
  left_join(spp_diet_ready, by = "scientific_name") 


# Calculate fish wet weights when possible using a and b conversion factors/parameters from fish base and average fish length. 
# Note: A lot of variation since some sources from diff geographic regions and locations so filler for now

length_weight_data <- length_weight(spp_names_val)

avg_length <- length_weight_data %>%
  dplyr::select(Species, LengthMin, LengthMax) %>%
  dplyr::mutate(group_length = rowMeans(select(., LengthMin, LengthMax), na.rm= TRUE)) 

avg_lengths_v2 <- avg_length %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(spp_mean_length = mean(group_length, na.rm=TRUE))

fish_length_conversion_fct <- length_weight_data %>%
  dplyr::select(Species, a,b) %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    mean_a = mean(a, na.rm = TRUE),
    mean_b = mean(b, na.rm = TRUE)
  )


fish_length_wt_conv_info <- merge(avg_lengths_v2, fish_length_conversion_fct)

fish_biomass_eq_function <- function(length, a_con, b_con){
  fish_biomass_conv <- a_con * (length^b_con)
}
  
fish_biomass_rfishbase_conv <- fish_length_wt_conv_info %>%
  dplyr::group_by(Species) %>%
  dplyr::mutate(mass_adult_g = fish_biomass_eq_function(spp_mean_length, mean_a, mean_b)) %>%
  dplyr::rename(scientific_name = Species) %>%
  dplyr::select(-spp_mean_length, -mean_a, -mean_b)
  

fish_biomass_ready <- fish_biomass_rfishbase_conv

####### END######################################


############ Traits from Literature ##########################

##    Parravicini et al. 2020
#     This paper has table of fish lengths, diet, active time, 
#     column position, mobility, and schooling data
##############################################################


spp_trt <- read.csv(file.path("Data", "traits_raw-data", "Fish_species_and_traits.csv"))  

spp_trt_v1 <- spp_trt %>%
  dplyr::select(Genus, Species, Activity, Diet_Parravicini_2020) %>%
  dplyr::mutate(scientific_name = paste(Genus, Species, sep = " ")) %>%
  dplyr::relocate(scientific_name, .before = Genus) %>%
  dplyr::rename(diet_trophic.level.specific_ordinal = Diet_Parravicini_2020) %>%
  dplyr::rename(active.time_category_ordinal = Activity) %>%
  dplyr::mutate(active.time_category_ordinal = case_when(
    active.time_category_ordinal ==  1 ~ "diurnal",
    active.time_category_ordinal ==  2 ~ "cathermal",
    active.time_category_ordinal ==  3 ~ "nocturnal"
  )) %>%
  dplyr::select(-Genus, -Species)

spp_trt_ready <- spp_trt_v1




#########################################################

## Combine All Trait Information

##########################################################


final_fish_spp_traits <- fish_spp_traits_rfishbase %>%
  dplyr::left_join(spp_trt_ready, by = "scientific_name") %>% 
  dplyr::left_join(fish_biomass_ready, by = "scientific_name")


#### Make final object 

fish_spp_traits_v99 <- final_fish_spp_traits

# Identify the file name & path
sparc_fish_file <- "SPARC_fish_spp-traits-fishbase-and-lit.csv"
sparc_fish_path <- file.path("Data", "traits_raw-data", sparc_fish_file)

# Export locally
write.csv(x = fish_spp_traits_v99, na = '', row.names = F, file = sparc_fish_path)

#------- End ----------------------


#save and share with Camille 
