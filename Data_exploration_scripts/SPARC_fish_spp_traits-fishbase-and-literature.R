########################
#CFD Fish Traits Table 
#######################


#Purpose: Combine fish traits from FishBase to create a fish database for fish species found across working 
#         group data sets (SPARC fish species). 

#Note: Will retroactively harmonize later to make reproducible. 


# Load libraries
librarian::shelf(tidyverse, rfishbase, stringr)


### Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()


###################
#Load Consumer Data 
##################
#estimate(species_list, fields = c("SpecCode", "ScientificName", "Lmax", "K"))

#rename data frames 

# Read in aquatic consumer species data 
data_v1 <- read.csv(file.path("Data", "community_tidy-data", "04_harmonized_consumer_excretion_sparc_cnd_site.csv"))

# Fliter desired columns 
data_v2 <- data_v1 |> 
  dplyr::select(project, habitat, raw_filename, scientific_name, 
                class,order, family, genus) |> 
  dplyr::group_by(project) |>
  dplyr::distinct() 

# Check structure
dplyr::glimpse(data_v2)


spp_names_distinct <- data_v1 %>%
  select(scientific_name) %>%
  distinct() 

#################### Fish Species Names ##########################

## Gather all distinct species names from sites with fish consumers 

####################################################################


spp_names_v1 <- data_v2 %>%
  filter(project %in% c("MCR", "SBC", "FCE", "CoastalCA", "VCR", "PIE", "RLS")) %>%
  select(project, scientific_name) %>%
  distinct() 

unique(spp_names_v1$project)
#isolate distinct species names 

spp_names_v2 <- spp_names_v1$scientific_name

#validate species names against names found in FishBase 

spp_names_val <- validate_names(spp_names_v2) #NA character added for species without matches so now 1357


spp_names_val_df <- as.data.frame(spp_names_val) #turn list of validated names into a data frame. Note: some sp names not validated 

spp_names_val_df2 <- spp_names_val_df %>%
  distinct() %>% #grab unique taxa names only no replicates. There are replicates because some projects have similar species
  dplyr::rename(scientific_name = spp_names_val)


#go back and re-run all names regardless is names are validated or not 


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

# Diet available only for part of our species: use estimates from models 
# (fishbase - using length and diet composition) to complete
spp_missing_diet <- setdiff(spp_names_val, spp_diet_v2$scientific_name)
fish_TL <- rfishbase::estimate(species_list = spp_missing_diet,
                               server = c("fishbase", "sealifebase"),
                               version = "latest")
fish_TL_clean <- fish_TL %>% 
  dplyr::select(c("Species", "Troph")) %>% 
  dplyr::rename(scientific_name = "Species") %>% 
  dplyr::rename(diet_trophic.level_num = "Troph")

# Add new traits data to previously retrieved one:
spp_diet_v3 <- rbind(spp_diet_v2, fish_TL_clean)

spp_diet_ready <- spp_diet_v3


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
# Note: A lot of variation since some sources from diff geographic regions and locations 


### some species have estimates for total lengths. Need to calculate species with information for total lengths.
### species with estimates for SL or FL: Need to  convert total lengths to SL or FL and use available estimates for these lengths when necessary
length_weight_data <- length_weight(spp_names_val)

avg_length <- length_weight_data %>%
  dplyr::select(Species, LengthMin, LengthMax) %>%
  dplyr::mutate(group_length = rowMeans(select(., LengthMin, LengthMax), na.rm= TRUE)) 


avg_lengths_v2 <- avg_length %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(spp_mean_length = mean(group_length, na.rm=TRUE))


fish_length_conversion_fct <- length_weight_data %>%
  dplyr::filter(!a > 2.0)%>% #Mustelus antarcticus had an unusually high a value from Fishbase so exclude 
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


# see which species are missing length data 
# given that the remaining species should have length data now should be able to use a and b estimates to calculate weight. 
# pull a and b estimates and use function to calculate mass

sp_tr_fish_length <- read_csv("sp_tr_fish_length.csv") #species missing mass data from Camille's missing traits output
View(sp_tr_fish_length)


# filter names of species missing mass data but that have length data 

sp_tr_fish_length_v2 <- sp_tr_fish_length[, c(1,5,6)] %>%
  filter(is.na(tr.mass.adult.g) & !is.na(length_max_cm))



#isolate species names 
sp_names_tr_length <-sp_tr_fish_length_v2$scientific_name

#validate species names against names found in FishBase 
sp_tr_fish_names <- validate_names(sp_names_tr_length) 


#retrieve a and b estimates 

a_b_estimates <- length_weight(
  species_list = sp_tr_fish_names,
  server = c("fishbase", "sealifebase"),
  version = "latest"
) # only 47 of 308 with mass data that is missing was retrieved although data for other species present in FishBase but not being retrieved 
#likely an issue of non-scrapping nature of function and/or updated species name on database that does not reflect nomenclature in our files. see below

 
a_b_estimates_v2 <- a_b_estimates %>%
  dplyr::filter(Type == "TL") #34 species 

#nevertheless will calculate mass then combine with mass ready data 
#only use data with Total length information 

mean_a_b_estimates<- a_b_estimates_v2  %>%
  dplyr::select(Species, a, b) %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    mean_a = mean(a, na.rm = TRUE),
    mean_b = mean(b, na.rm = TRUE)
  ) %>%
  dplyr::rename( scientific_name = Species) %>%
  dplyr::mutate(scientific_name = case_when( # change updated names back to older names reported in our data frame. Fishbase uses Eschmeyer's Catalog of Fishes to check, validate and update names...we have used ITIS
    scientific_name == "Pycnochromis margaritifer" ~ "Chromis margaritifer",
    scientific_name == "Bodianus pulcher "~ "Semicossyphus pulcher",
    scientific_name == "Centropyge fisheri" ~ "Centropyge flavicauda",
    scientific_name == "Kyphosus azureus" ~ "Hermosilla azurea",
    scientific_name == "Mirolabrichthys pascalus" ~ "Pseudanthias pascalus",
    scientific_name == "Ostracion cubicum" ~ "Ostracion cubicus",
    scientific_name == "Plectroglyphidodon fasciolatus" ~ "Stegastes fasciolatus",
    scientific_name == "Turrum fulvoguttatum" ~ "Carangoides fulvoguttatus",
    scientific_name == "Bodianus pulcher" ~ "Semicossyphus pulcher",
    T ~ scientific_name
  ))


sp_trt_missing_mass <- dplyr::left_join(mean_a_b_estimates, sp_tr_fish_length_v2, by = "scientific_name")


sp_trt_fishbase_mass  <- sp_trt_missing_mass %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::mutate(tr.mass.adult.g = fish_biomass_eq_function(length_max_cm, mean_a, mean_b)) %>%
  dplyr::select(-length_max_cm, -mean_a, -mean_b)


sp_tr_fish_length_v3 <- dplyr::left_join(sp_tr_fish_length_v2,
                                         sp_trt_fishbase_mass, by = "scientific_name") %>%
  dplyr::mutate(tr.mass.adult.g = dplyr::coalesce(tr.mass.adult.g.x, tr.mass.adult.g.y)) %>%
  dplyr::select(-tr.mass.adult.g.x,-tr.mass.adult.g.y)


### match missing species with species with data from MCR_data_conversion [knb-lter-mcr.6001.7]

MCR_LTER_Fish_Weight_Con <- read.csv("MCR_LTER_Reference_Fish_Weight_Conversion_20260224.csv") %>%
  dplyr::rename(scientific_name = species)

MCR_LTER_Fish_Weight_Con$totlen2forklen <- as.numeric(MCR_LTER_Fish_Weight_Con$totlen2forklen)
MCR_LTER_Fish_Weight_Con$a <- as.numeric(MCR_LTER_Fish_Weight_Con$a)
MCR_LTER_Fish_Weight_Con$b <- as.numeric(MCR_LTER_Fish_Weight_Con$b)

#notes for later 
#tolen2forklen - value equals parameter Lb. FL = Lb * Total Length (TL). Lb estimated from photograph from FishBase
# W = a x FL^b; W = a x TL^b; W = a x SL^b. Must match appropriate estimates 
#MCR-LTER conversion Lb = 1 if no info available = overestimation of FL 


sp_tr_fish_conversions <- dplyr::left_join(sp_tr_fish_length_v3, MCR_LTER_Fish_Weight_Con, by = "scientific_name")

#Calculate mass values for species with fork length a and b estimates source knb-lter-mcr.6001.7 - Kulbicki  
sp_tr_fish_length_FL <- sp_tr_fish_conversions %>%
  dplyr::filter(length_code == "FL") %>%
  dplyr::mutate(length_FL = totlen2forklen*length_max_cm) %>%
  dplyr::mutate(tr.mass.adult.g = 
                  a * length_FL^b) %>%
  dplyr::select(scientific_name, tr.mass.adult.g)

sp_tr_fish_length_v4 <- dplyr::left_join(sp_tr_fish_length_v3, sp_tr_fish_length_FL,
                                         by = "scientific_name") %>%
  dplyr::mutate(tr.mass.adult.g = dplyr::coalesce(tr.mass.adult.g.x, tr.mass.adult.g.y)) %>%
  dplyr::select(-tr.mass.adult.g.x,-tr.mass.adult.g.y)
  
  

#remaining values focus on exported excel sheet

# and physically retrieve other missing values and or check names  

sp_remaining_miss_mass <- sp_tr_fish_length_v4 %>%
  filter(is.na(tr.mass.adult.g)) # <- need a and b estimates for these species 


#write locally to work on outside of script  
write_csv(sp_remaining_miss_mass,"/Users/shalandagrier/Documents/Post Doc Projects/SPARC_Consumer_FxN_Diversity/sp_remaining_miss_mass.csv")

options(scipen=999)
####### END######################################


############ Traits from Literature ##########################

##    Parravicini et al. 2020
#     This paper has table of fish lengths, diet, active time, 
#     column position, mobility, and schooling data
##############################################################


#spp_trt <- read.csv(file.path("Data", "traits_raw-data", "Fish_species_and_traits.csv"))  

#spp_trt_v1 <- spp_trt %>%
#  dplyr::select(Genus, Species, Activity, Diet_Parravicini_2020) %>%
#  dplyr::mutate(scientific_name = paste(Genus, Species, sep = " ")) %>%
#  dplyr::relocate(scientific_name, .before = Genus) %>%
#  dplyr::rename(diet_trophic.level.specific_ordinal = Diet_Parravicini_2020) %>%
#  dplyr::rename(active.time_category_ordinal = Activity) %>%
#  dplyr::mutate(active.time_category_ordinal = case_when(
#    active.time_category_ordinal ==  1 ~ "diurnal",
#    active.time_category_ordinal ==  2 ~ "cathermal",
#    active.time_category_ordinal ==  3 ~ "nocturnal"
#  )) %>%
#  dplyr::select(-Genus, -Species)

#spp_trt_ready <- spp_trt_v1




#########################################################

## Combine All Trait Information

##########################################################


final_fish_spp_traits <- fish_spp_traits_rfishbase %>%
  #dplyr::left_join(spp_trt_ready, by = "scientific_name") %>% 
  dplyr::left_join(fish_biomass_ready, by = "scientific_name")


#### Make final object 

fish_spp_traits_v99 <- final_fish_spp_traits

# Identify the file name & path
sparc_fish_file <- "SPARC_fish_spp-traits-fishbase-and-lit_v2.csv"
sparc_fish_path <- file.path("Data", "traits_raw-data", sparc_fish_file)

# Export locally
write.csv(x = fish_spp_traits_v99, na = '', row.names = F, file = sparc_fish_path)

#------- End ----------------------

