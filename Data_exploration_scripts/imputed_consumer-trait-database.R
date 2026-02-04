
#######################
#CFD - Trait Imputation by Taxonomic Levels 
#######################

##need to FIX code! 

#Purpose:  1) Combine all trait data 2) Wrangle and Clean species x trait matrix to use for further exploration  
#Identify the proportion of missing data for each trait across all consumer groups  

#Shalanda Grier
#2/2026

#----------------------------------------------------------------------------

# Load libraries
librarian::shelf(tidyverse, dplyr, funbiogeo, ggplot2)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()


###################
#Load Consumer Taxa List  
##################


# Read in master species list for all programs
sp_pro_list <- read.csv(file.path("Data", "species_tidy-data", "23_species_master-spp-list.csv"))

#####################
#Load Trait Data
####################

## Read in all trait databases and clean up if necessary

all_trt <- read.csv(file.path("Data", "traits_tidy-data", "12_traits_wrangled.csv")) %>% #all trait databases are present expecet functionaltraitsmatirx -not harmonized
  dplyr::select( -epithet, -taxonomic.resolution, -taxon) %>% # select columns of interest
  dplyr::filter(scientific_name != "") %>% #remove traits without scientific name 
  dplyr::mutate_if(is.integer, as.numeric) %>% #change all columns that are integer to numeric
  dplyr::mutate(across(where(is.character), ~na_if(., ""))) %>%
  dplyr::mutate(across(where(is.numeric),~ ifelse(.x <=0, NA, .x) #replace NA indicators and instances of '0' with NA. otherwise keep original values
    )
  ) %>%
  dplyr::relocate(order, .before= family)

#---------------- end loading data -------------------



# -----------Species list wrangling start --------

sp_pro_list_v2 <- sp_pro_list %>%
  dplyr::mutate(across(everything(), na_if, y = ""))

#Isolate all distinct taxa names, the original master list by project so duplicates across programs with same taxa
master_sp_list <- sp_pro_list %>%
  dplyr::select(scientific_name) %>%
  dplyr::distinct() 

#total_sci_name_duplicates <- sum(duplicated(master_sp_list$scientific_name))
#length(unique(master_sp_list$scientific_name)) # 2529 distinct taxa in master species list 

master_sp_list_v2 <- master_sp_list %>%
  dplyr::left_join(
    sp_pro_list_v2 %>%
      dplyr::select(scientific_name, phylum, class, order, family, genus)%>%
      dplyr::distinct(),
    by = "scientific_name")


#Check duplicates
master_sp_duplicates <- sum(duplicated(master_sp_list_v2$scientific_name))  
#species but have at least one difference in higher order taxonomic level 

#Extract duplicates scientific_names 160 unique scientific_names 
master_sp_dup_names <-master_sp_list_v2 %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::filter(n() > 1) %>%
  ungroup()

##For now will arbitrarily keep the first occurrence but will go back and check correct class names. Also which source?
master_sp_remove_dup <- master_sp_dup_names %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::slice(1) %>% # select first row for each scientific name
  dplyr::ungroup() #now 160 obs 

# remove duplicate names from 'master_sp_list_v2' 

master_sp_no_dup <- master_sp_list_v2 %>%
  dplyr::anti_join(master_sp_dup_names, by= "scientific_name")
  
#combine master sp. list with no duplicates with paired down scientific_names 'master_sp_remove_dup'

master_sp_list_ready <- bind_rows(master_sp_no_dup, master_sp_remove_dup) #now should have same names as orig. 'master_sp_list'


#-------------------------- end ------------------------------------------



#Join trait data with program species list 
program_sp_trt_data <- dplyr::left_join(master_sp_list, all_trt, by="scientific_name")

#-------------------Start calculate mean trait values for all trait databases for scientific_names: 
#-------------------either species, genus, family, along w/order and/or phylum for zoo 

#all numeric traits
mean_trt_source <-program_sp_trt_data %>%
  dplyr::group_by(scientific_name)%>%
  dplyr::summarise(across(where(is.numeric),mean, na.rm = TRUE, .groups = 'drop')) #%>%
 # dplyr::distinct(scientific_name, .keep_all =TRUE) 

#all character traits 
trt_chr_data <- program_sp_trt_data %>%
  dplyr::group_by(scientific_name) %>%
  #dplyr::select(5:11, where(is.character)) %>%
  #dplyr::mutate(across(where(is.character), ~na_if(., ""))) %>%
  dplyr::summarise(across(where(is.character), ~paste(unique(.), collapse = "; ")),.groups = "drop") %>%
  dplyr::distinct(scientific_name, .keep_all = TRUE)


#join all numeric and character traits for all databases 
master_sp_tr_mat <- dplyr::left_join(mean_trt_source, trt_chr_data, by = "scientific_name") %>%
  #dplyr::distinct(scientific_name, .keep_all = TRUE) %>%
  dplyr::mutate(across(everything(), ~replace(., is.nan(.), NA))) %>%
  dplyr::mutate(across(where(is.character), na_if, "NA")) %>%
  dplyr::mutate(across(where(is.character), ~ str_replace_all(.x, "; NA", "")))%>%
  dplyr::mutate(across(where(is.character), ~ str_replace_all(.x, "NA; ", ""))) %>%
  dplyr::mutate(scientific_name = gsub("\\.", " ", scientific_name))


#################### end #####################


# ----------- start genus --------
############### find mean for genus ##################


genus_data <- read.csv(file.path("Data", "traits_tidy-data", "12_traits_wrangled.csv")) %>%
  dplyr::select( -epithet, -taxonomic.resolution, -taxon) %>%
  dplyr::filter(genus!= "") %>% #remove traits without genus
  dplyr::mutate(dplyr::across(.cols = dplyr::everything()))%>%
  dplyr::mutate_if(is.integer, as.numeric) %>% #change all columns that are integer to numeric
  dplyr::mutate(across(where(is.character), ~na_if(., "")))

num_trt_genus <- genus_data  %>%
  dplyr::group_by(genus)%>%
  dplyr::summarise(across(where(is.numeric),mean, na.rm = TRUE,.groups = "drop"))%>%
  dplyr::distinct(genus, .keep_all =TRUE) %>%
  dplyr::mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))


chr_trt_gensus <- genus_data  %>%
  dplyr::group_by(genus) %>%
  dplyr::summarise(across(where(is.character), ~paste(unique(.), collapse = "; "))) %>%
  dplyr::distinct(genus, .keep_all =TRUE) %>%
  dplyr::mutate(across(where(is.character), ~ str_replace_all(.x, "; NA", "")))%>%
  dplyr::mutate(across(where(is.character), ~ str_replace_all(.x, "NA; ", ""))) %>%
  dplyr::mutate(across(where(is.character), na_if, "NA")) %>%
  ungroup()

all_genus <- left_join(num_trt_genus, chr_trt_gensus, by = "genus") 

#join master trait list with genus data ... join by genus 

imputed_genus <- dplyr::left_join(master_sp_tr_mat, all_genus, 
                                  by = "genus") 

collapse_genus <- imputed_genus %>%
      dplyr::mutate(
        scientific_name = coalesce(scientific_name.x, scientific_name.y),
        family = coalesce(family.x, family.y),
        source = coalesce(source.x, source.y),
        sex = coalesce(sex.x, sex.y),
        migration = coalesce(migration.x, migration.y),
        range_area_km2 = coalesce(range_area_km2.x, range_area_km2.y),                                                  
        age_life.span_years = coalesce(age_life.span_years.x, age_life.span_years.y),                                             
        age_maturity.min_years = coalesce(age_maturity.min_years.x, age_maturity.min_years.y),                                          
        age_maturity.max_years =  coalesce(age_maturity.max_years.x, age_maturity.max_years.y),                                          
        age_maturity.female_years =  coalesce(age_maturity.female_years.x, age_maturity.female_years.y),                                        
        age_maturity.male_years = coalesce(age_maturity.male_years.x, age_maturity.male_years.y),                                         
        age_maturity_years = coalesce(age_maturity_years.x, age_maturity_years.y),                                                
        age_first.reproduction_years = coalesce(age_first.reproduction_years.x, age_first.reproduction_years.y),                                   
        diet_trophic.level_num =   coalesce(diet_trophic.level_num.x, diet_trophic.level_num.y),                                         
        reproduction_reproductive.rate_num.offspring.per.clutch.or.litter = coalesce(reproduction_reproductive.rate_num.offspring.per.clutch.or.litter.x, reproduction_reproductive.rate_num.offspring.per.clutch.or.litter.y), 
        reproduction_reproductive.rate_num.litter.or.clutch.per.year = coalesce(reproduction_reproductive.rate_num.litter.or.clutch.per.year.x, reproduction_reproductive.rate_num.litter.or.clutch.per.year.y),      
        reproduction_reproductive.rate_num.events.per.year =  coalesce(reproduction_reproductive.rate_num.events.per.year.x, reproduction_reproductive.rate_num.events.per.year.y),              
        reproduction_reproductive.rate_num.offspring.per.year = coalesce(reproduction_reproductive.rate_num.offspring.per.year.x, reproduction_reproductive.rate_num.offspring.per.year.y),            
        reproduction_reproductive.rate_num.per.litter.or.clutch =  coalesce(reproduction_reproductive.rate_num.per.litter.or.clutch.x, reproduction_reproductive.rate_num.per.litter.or.clutch.y),          
        reproduction_reproductive.rate_num.offspring.per.litter =   coalesce(reproduction_reproductive.rate_num.offspring.per.litter.x, reproduction_reproductive.rate_num.offspring.per.litter.y),        
        reproduction_reproductive.rate_num.litter.per.year =  coalesce(reproduction_reproductive.rate_num.litter.per.year.x, reproduction_reproductive.rate_num.litter.per.year.y),              
        reproduction_reproductive.rate_num.offspring.per.clutch = coalesce(reproduction_reproductive.rate_num.offspring.per.clutch.x, reproduction_reproductive.rate_num.offspring.per.clutch.y),           
        reproduction_fecundity.min_num =  coalesce(reproduction_fecundity.min_num.x, reproduction_fecundity.min_num.y),                                   
        reproduction_fecundity.max_num =  coalesce(reproduction_fecundity.max_num.x, reproduction_fecundity.max_num.y),                                   
        metabolic_rate_oxygen_per_hour = coalesce(metabolic_rate_oxygen_per_hour.x, metabolic_rate_oxygen_per_hour.y),                                    
        reproduction_fecundity_num =  coalesce(reproduction_fecundity_num.x, reproduction_fecundity_num.y),                                        
        reproduction_fecundity.max =  coalesce(reproduction_fecundity.max.x, reproduction_fecundity.max.y),                                        
        reproduction_fecundity_num_eggs.female..1 =  coalesce(reproduction_fecundity_num_eggs.female..1.x, reproduction_fecundity_num_eggs.female..1.y),                         
        length_adult.max_cm =  coalesce(length_adult.max_cm.x, length_adult.max_cm.y),                                              
        length_adult_cm = coalesce(length_adult_cm.x, length_adult_cm.y),                                                   
        length_adult.female.max_cm =  coalesce(length_adult.female.max_cm.x, length_adult.female.max_cm.y),                                       
        length_adult.male.max_cm =  coalesce(length_adult.male.max_cm.x, length_adult.male.max_cm.y),                                         
        length_offspring_cm = coalesce(length_offspring_cm.x, length_offspring_cm.y),                                              
        length_offspring.max_cm =  coalesce(length_offspring.max_cm.x, length_offspring.max_cm.y),                                           
        length_offspring.min_cm =  coalesce(length_offspring.min_cm.x, length_offspring.min_cm.y),                                          
        length_egg_cm = coalesce(length_egg_cm.x, length_egg_cm.y),                                                     
        length_max_cm = coalesce(length_max_cm.x, length_max_cm.y),                                                     
        weight_egg_mg = coalesce(weight_egg_mg.x, weight_egg_mg.y),                                                     
        weight_adult.wet_mg =  coalesce(weight_adult.wet_mg.x, weight_adult.wet_mg.y),                                              
        weight_egg_ug = coalesce(weight_egg_ug.x, weight_egg_ug.y),                                                     
        mass_adult_g = coalesce(mass_adult_g.x, mass_adult_g.y),                                                      
        mass_offspring_g = coalesce(mass_offspring_g.x, mass_offspring_g.y),                                                 
        mass_adult.female_g =  coalesce(mass_adult.female_g.x, mass_adult.female_g.y),                                              
        mass_adult.male_g =  coalesce(mass_adult.male_g.x, mass_adult.male_g.y),                                                
        mass_adult_mg =  coalesce(mass_adult_mg.x, mass_adult_mg.y),                                                    
        mass_max.adult_g =  coalesce(mass_max.adult_g.x, mass_max.adult_g.y),                                                
        mass_min.adult_g =  coalesce(mass_min.adult_g.x, mass_min.adult_g.y),                                                
        volume_offspring_m3 = coalesce(volume_offspring_m3.x, volume_offspring_m3.y),                                            
        metabolism_metabolic.rate_W = coalesce(metabolism_metabolic.rate_W.x, metabolism_metabolic.rate_W.y),                                      
        metabolism_metabolic.rate_oxygen.per.hour = coalesce(metabolism_metabolic.rate_oxygen.per.hour.x, metabolism_metabolic.rate_oxygen.per.hour.y),                        
        sex = coalesce(sex.x, sex.y),                                                              
        diet_trophic.level.broad_ordinal = coalesce(diet_trophic.level.broad_ordinal.x, diet_trophic.level.broad_ordinal.y),                                
        diet_trophic.level.specific_ordinal = coalesce(diet_trophic.level.specific_ordinal.x, diet_trophic.level.specific_ordinal.y),                              
        diet_trophic.level_ordinal =  coalesce(diet_trophic.level_ordinal.x, diet_trophic.level_ordinal.y),                                     
        reproduction_reproductive.mode_ordinal = coalesce(reproduction_reproductive.mode_ordinal.x, reproduction_reproductive.mode_ordinal.y),                          
        life.stage_specific_ordinal =  coalesce(life.stage_specific_ordinal.x, life.stage_specific_ordinal.y),                                     
        active.time_category_ordinal =  coalesce(active.time_category_ordinal.x, active.time_category_ordinal.y)) %>%
        select(-ends_with(".x"), -ends_with(".y")) %>%
  dplyr::relocate(source, scientific_name, .before = "genus")


#############  end ##################


# find mean for family 
family_trt <- read.csv(file.path("Data", "traits_tidy-data", "12_traits_wrangled.csv")) %>%
  dplyr::select(-genus,-scientific_name, -epithet, -taxonomic.resolution, -taxon) %>%
  dplyr::filter(family!= "") %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything()))%>% 
  dplyr::mutate_if(is.integer, as.numeric) %>% #change all columns that are integer to numeric
  dplyr::mutate(across(where(is.character), ~na_if(., "")))


#find the mean value for all traits at family level  

mean_trt_family <- family_trt %>%
  dplyr::group_by(family)%>%
  dplyr::summarise(across(where(is.numeric),mean, na.rm = TRUE,.groups = "drop"))%>%
  dplyr::distinct(family, .keep_all =TRUE) %>%
  dplyr::mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) 


trt_chr_family <- family_trt %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(across(where(is.character), ~paste(unique(.), collapse = "; "))) %>%
  dplyr::distinct(family, .keep_all =TRUE) %>%
  dplyr::mutate(across(where(is.character), ~ str_replace_all(.x, "; NA", "")))%>%
  dplyr::mutate(across(where(is.character), ~ str_replace_all(.x, "NA; ", ""))) %>%
  dplyr::mutate(across(where(is.character), na_if, "NA")) %>%
  ungroup()

all_family <- left_join(mean_trt_family , trt_chr_family, by = "family") 


imputed_family <- dplyr::left_join(collapse_genus, all_family , 
                                  by = "family") 

test_family <- imputed_family %>%
  dplyr::mutate(
    source = coalesce(source.x, source.y),
    sex = coalesce(sex.x, sex.y),
    migration = coalesce(migration.x, migration.y),
    range_area_km2 = coalesce(range_area_km2.x, range_area_km2.y),                                                  
    age_life.span_years = coalesce(age_life.span_years.x, age_life.span_years.y),                                             
    age_maturity.min_years = coalesce(age_maturity.min_years.x, age_maturity.min_years.y),                                          
    age_maturity.max_years =  coalesce(age_maturity.max_years.x, age_maturity.max_years.y),                                          
    age_maturity.female_years =  coalesce(age_maturity.female_years.x, age_maturity.female_years.y),                                        
    age_maturity.male_years = coalesce(age_maturity.male_years.x, age_maturity.male_years.y),                                         
    age_maturity_years = coalesce(age_maturity_years.x, age_maturity_years.y),                                                
    age_first.reproduction_years = coalesce(age_first.reproduction_years.x, age_first.reproduction_years.y),                                   
    diet_trophic.level_num =   coalesce(diet_trophic.level_num.x, diet_trophic.level_num.y),                                         
    reproduction_reproductive.rate_num.offspring.per.clutch.or.litter = coalesce(reproduction_reproductive.rate_num.offspring.per.clutch.or.litter.x, reproduction_reproductive.rate_num.offspring.per.clutch.or.litter.y), 
    reproduction_reproductive.rate_num.litter.or.clutch.per.year = coalesce(reproduction_reproductive.rate_num.litter.or.clutch.per.year.x, reproduction_reproductive.rate_num.litter.or.clutch.per.year.y),      
    reproduction_reproductive.rate_num.events.per.year =  coalesce(reproduction_reproductive.rate_num.events.per.year.x, reproduction_reproductive.rate_num.events.per.year.y),              
    reproduction_reproductive.rate_num.offspring.per.year = coalesce(reproduction_reproductive.rate_num.offspring.per.year.x, reproduction_reproductive.rate_num.offspring.per.year.y),            
    reproduction_reproductive.rate_num.per.litter.or.clutch =  coalesce(reproduction_reproductive.rate_num.per.litter.or.clutch.x, reproduction_reproductive.rate_num.per.litter.or.clutch.y),          
    reproduction_reproductive.rate_num.offspring.per.litter =   coalesce(reproduction_reproductive.rate_num.offspring.per.litter.x, reproduction_reproductive.rate_num.offspring.per.litter.y),        
    reproduction_reproductive.rate_num.litter.per.year =  coalesce(reproduction_reproductive.rate_num.litter.per.year.x, reproduction_reproductive.rate_num.litter.per.year.y),              
    reproduction_reproductive.rate_num.offspring.per.clutch = coalesce(reproduction_reproductive.rate_num.offspring.per.clutch.x, reproduction_reproductive.rate_num.offspring.per.clutch.y),           
    reproduction_fecundity.min_num =  coalesce(reproduction_fecundity.min_num.x, reproduction_fecundity.min_num.y),                                   
    reproduction_fecundity.max_num =  coalesce(reproduction_fecundity.max_num.x, reproduction_fecundity.max_num.y),                                   
    metabolic_rate_oxygen_per_hour = coalesce(metabolic_rate_oxygen_per_hour.x, metabolic_rate_oxygen_per_hour.y),                                    
    reproduction_fecundity_num =  coalesce(reproduction_fecundity_num.x, reproduction_fecundity_num.y),                                        
    reproduction_fecundity.max =  coalesce(reproduction_fecundity.max.x, reproduction_fecundity.max.y),                                        
    reproduction_fecundity_num_eggs.female..1 =  coalesce(reproduction_fecundity_num_eggs.female..1.x, reproduction_fecundity_num_eggs.female..1.y),                         
    length_adult.max_cm =  coalesce(length_adult.max_cm.x, length_adult.max_cm.y),                                              
    length_adult_cm = coalesce(length_adult_cm.x, length_adult_cm.y),                                                   
    length_adult.female.max_cm =  coalesce(length_adult.female.max_cm.x, length_adult.female.max_cm.y),                                       
    length_adult.male.max_cm =  coalesce(length_adult.male.max_cm.x, length_adult.male.max_cm.y),                                         
    length_offspring_cm = coalesce(length_offspring_cm.x, length_offspring_cm.y),                                              
    length_offspring.max_cm =  coalesce(length_offspring.max_cm.x, length_offspring.max_cm.y),                                           
    length_offspring.min_cm =  coalesce(length_offspring.min_cm.x, length_offspring.min_cm.y),                                          
    length_egg_cm = coalesce(length_egg_cm.x, length_egg_cm.y),                                                     
    length_max_cm = coalesce(length_max_cm.x, length_max_cm.y),                                                     
    weight_egg_mg = coalesce(weight_egg_mg.x, weight_egg_mg.y),                                                     
    weight_adult.wet_mg =  coalesce(weight_adult.wet_mg.x, weight_adult.wet_mg.y),                                              
    weight_egg_ug = coalesce(weight_egg_ug.x, weight_egg_ug.y),                                                     
    mass_adult_g = coalesce(mass_adult_g.x, mass_adult_g.y),                                                      
    mass_offspring_g = coalesce(mass_offspring_g.x, mass_offspring_g.y),                                                 
    mass_adult.female_g =  coalesce(mass_adult.female_g.x, mass_adult.female_g.y),                                              
    mass_adult.male_g =  coalesce(mass_adult.male_g.x, mass_adult.male_g.y),                                                
    mass_adult_mg =  coalesce(mass_adult_mg.x, mass_adult_mg.y),                                                    
    mass_max.adult_g =  coalesce(mass_max.adult_g.x, mass_max.adult_g.y),                                                
    mass_min.adult_g =  coalesce(mass_min.adult_g.x, mass_min.adult_g.y),                                                
    volume_offspring_m3 = coalesce(volume_offspring_m3.x, volume_offspring_m3.y),                                            
    metabolism_metabolic.rate_W = coalesce(metabolism_metabolic.rate_W.x, metabolism_metabolic.rate_W.y),                                      
    metabolism_metabolic.rate_oxygen.per.hour = coalesce(metabolism_metabolic.rate_oxygen.per.hour.x, metabolism_metabolic.rate_oxygen.per.hour.y),                        
    sex = coalesce(sex.x, sex.y),                                                              
    diet_trophic.level.broad_ordinal = coalesce(diet_trophic.level.broad_ordinal.x, diet_trophic.level.broad_ordinal.y),                                
    diet_trophic.level.specific_ordinal = coalesce(diet_trophic.level.specific_ordinal.x, diet_trophic.level.specific_ordinal.y),                              
    diet_trophic.level_ordinal =  coalesce(diet_trophic.level_ordinal.x, diet_trophic.level_ordinal.y),                                     
    reproduction_reproductive.mode_ordinal = coalesce(reproduction_reproductive.mode_ordinal.x, reproduction_reproductive.mode_ordinal.y),                          
    life.stage_specific_ordinal =  coalesce(life.stage_specific_ordinal.x, life.stage_specific_ordinal.y),                                     
    active.time_category_ordinal =  coalesce(active.time_category_ordinal.x, active.time_category_ordinal.y)) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  dplyr::relocate(source, .before = scientific_name)


 

################################## end ##########################

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make a final object
consumer_imputed_traits_v99 <-test_family 

# Check structure
dplyr::glimpse(consumer_imputed_traits_v99)

# Identify the file name & path
consumer_traits_file <- "consumer-trait-species-imputed-taxonmic-database.csv"
consumer_traits_path <- file.path("Data", "traits_tidy-data", consumer_traits_file)

# Export locally
write.csv(x = consumer_imputed_traits_v99, na = '', row.names = F, file = consumer_traits_path)

# End ----




################ test #######################
# Impute missing values with the family mean
get_mode <- function(x) {
  # Remove NAs first
  x <- na.omit(x)
  if (length(x) == 0) return(NA) # Return NA if all values are missing in a group
  # Calculate mode
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

test_imputed_data <- all_trt %>%
  group_by(family) %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), mean(., na.rm = TRUE)))) %>%
  mutate(across(where(is.character), ~replace_na(.x, get_mode(.x)))) %>%
  ungroup() %>%
  dplyr::mutate(across(where(is.character), ~na_if(., "")))



ch_imputed <- all_trt %>%
  group_by(family) %>%
  mutate(across(where(is.character), ~replace_na(.x, get_mode(.x)))) %>%
  ungroup()

