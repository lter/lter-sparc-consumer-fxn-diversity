#----------------------------------##
# SPARC - Consumer Functional Diversity (CFD) 
###--------------------------------##

# Script author(s): Shalanda Grier, Li Kui, Nick Lyons
# Sites: Palmer LTER, Arctic LTER, North Lakes LTER


#Purpose
#1) Data cleaning for each site to include: species/scientific names, standarized densities, diet_cat and dry/wt per ind

# Load libraries
librarian::shelf(tidyverse, ltertools, stringr, taxize, purrr)

# Clear environment & collect garbage
rm(list = ls()); gc()


# read in harmonized data and start wrangling by project 

data_02 <- read.csv(file = file.path("data", "02_community_processed_data", "02_consumer_harmonized.csv"))



# read in zooplankton dry weight data 

zoo_dry_wts <- read.csv(file=file.path('data', "11_traits_raw_data", "Zooplankton_dry_ind_wt.csv"))

#remove instances of 'sp.',  'spp.' , 'sp'  , 'spp. ' 

zoo_dry_wts_d1 <- zoo_dry_wts %>% dplyr::mutate(scientific_name = stringr::str_replace(scientific_name, " sp.$| spp.$| sp$| spp. $", "")) 

# Remove trailing spaces from 'text_column'
zoo_dry_wts_d1$scientific_name<- str_trim(zoo_dry_wts_d1$scientific_name, side = "right")

################## General Wrangling before site level data wrangling #####################################


#### General Wrangling of Harmonized Community Data 
## attend to capitalization 

#change capitalization of species names to lower case 
data_02$species <- tolower(data_02$species)

#capitilize the first letter of each species 
data_02$species <-str_to_sentence(data_02$species) 

#change date from character to date and separate year, month, and day 

data_02$date <- as.Date(data_02$date, format = "%Y-%m-%d")

data_02_date <- data_02 %>% 
  #separate year, month and date into distinct columns 
  dplyr::mutate(year = lubridate::year(date),month = lubridate::month(date), day = lubridate::day(date))%>%
  # bring columns related to the date behind the date column
  dplyr::relocate(c(year,month,day), .before = subsite_level2) 



################ End #######

###  Wrangling zooplankton species names and pull kingdom, phylum, class, order, family, and species names 

#code junk  below adapted from CND step1_consumer_data_harmonization.R originally written by Angel Chen 
# Li can you try to run this...wifi not great at home at the moment and timing out. 

#zoo_taxa <- zoo_dry_wts_d1 %>% 
#  select the scientific_name column. This column originally filled based on LTER sites reported species names
#  dplyr::select(scientific_name)%>%
  #Grab all unique species names 
#  dplyr::distinct() %>%
  #Create an empty placeholder column to fill later 
#  dplyr::mutate(kingdom = NA, 
#                phylum = NA,
#                class = NA,
#                order = NA,
#                family = NA,
#                genus = NA, 
#                species = NA,
#                common_name = NA) %>% 
  #Rename species column 
#  dplyr::rename(scientific_name_check = scientific_name)



### add kingdom, phylum, order, family, genus and species name using taxcise 

#for (i in 1:length(zoo_taxa$scientific_name_check)){
  
  #Query species taxonomic information 
#  identified_species_names <- taxize::tax_name(sci = zoo_taxa[i,]$scientific_name_check,
#                                                get = c("kindgom", "phylum", "class", "order", "family", "genus", "species"),
#                                                db = "itis",
#                                                accepted = TRUE,
#                                                ask = FALSE)
  #Query species for common name 
#  identified_common_names <- taxize::sci2comm(sci = zoo_taxa[i,]$scientific_name_check,
#                                               db = "itis",
#                                              accepted = TRUE,
#                                              ask = FALSE)

  
  # Save the query results
  # In case the query returns NULL, the paste0(..., collapse = "") will coerce NULL into an empty string
#  zoo_taxa[i,]$kingdom <- paste0(identified_species_names$kingdom, collapse = "")
#  zoo_taxa[i,]$phylum <- paste0(identified_species_names$phylum, collapse = "")
#  zoo_taxa[i,]$class <- paste0(identified_species_names$class, collapse = "")
#  zoo_taxa[i,]$order <- paste0(identified_species_names$order, collapse = "")
#  zoo_taxa[i,]$family <- paste0(identified_species_names$family, collapse = "")
#  zoo_taxa[i,]$genus <- paste0(identified_species_names$genus, collapse = "")
#  zoo_taxa[i,]$species <- paste0(identified_species_names$species, collapse = "")
#  zoo_taxa[i,]$common_name <- paste0(identified_common_names[[1]], collapse = "; ")
  
#}

#zoo_taxa_v2 <- zoo_taxa %>%
  # Replace the string "NA" with actual NA values
#  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~dplyr::na_if(., y = "NA"))) %>%
  # Replace empty strings with NA values
#  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~dplyr::na_if(., y = "")))


# Left join our current tidy dataframe with the table of taxonomic info
#zoo_taxa_tidy <- left_join(zoo_dry_wts_d1, zoo_taxa_v2, by = c("scientific_name" = "scientific_name_fix"))
  


#fill in remaining taxonomy columns where applicable. Can do manually using mutate and case_when - Shalanda 

#!!!Once code above is complete replace 'replace zoo_dry_wts_d1' with 'zoo_taxa_ready' in code down below and run 





#### Arctic ###############


Arctic  <- data_02_date %>% 
  dplyr::filter(project == "Arctic")


#convert density from num/l to num/m3 


Arctic_den <- Arctic %>% 
  dplyr::mutate(`density_num/m3` = density*1000)

#add dry weight, diet_cat and scientific names and rename group column 

Arctic_dry_wt <- dplyr::left_join(Arctic_den,zoo_dry_wts_d1, by= join_by("species" == "sp_code")) %>%
  dplyr::select(-c('program','drymass_mg','drymass_source','additional.notes')) %>%
  dplyr::rename(taxa_group = group)

Arctic_species_col <- Arctic_dry_wt %>% 
  dplyr::select(-species) %>%
  dplyr::rename(species= species.y )%>%
#add temp data. Mean temp of Toolik main lake from 2004 - 2021 
  dplyr::mutate(temp_c = "7.074284")


########################### end #############################





#########Palmer LTER 

Palmer <- data_02_date %>%
  dplyr::filter(project == "Palmer ")


#convert density from 1000m3 to 1m3. 

Palmer_den <- Palmer %>% 
  dplyr::mutate(`density_num/m3` = density/1000)

#add dry weight, diet_cat and scientific names and rename 'group' column 

Palmer_dry_wt <- dplyr::left_join(Palmer_den, zoo_dry_wts_d1, by= join_by("species" == "sp_code")) %>%
  dplyr::select(-c('program','drymass_mg','drymass_source','additional.notes')) %>%
  dplyr::rename(taxa_group = group)

Palmer_species_col <- Palmer_dry_wt %>% 
  dplyr::select(-species) %>%
  dplyr::rename(species= species.y )

#add temp data. SST retrieved from Palmer LTER weather station. 
#Note average SST temp over duration of survey period 2009-2024. 

#I believe every site had one temp value from CND or mean value for every year site combination? Not necessarily by depth. 

Palmer_temp <- Palmer_species_col %>%
  dplyr::mutate(temp_c = "-0.4598606")




######### end ################




###### North Lakes LTER 

NorthLakes <- data_02_date %>% 
  dplyr::filter(project == "NorthLakes")



#convert density from m2 to m3. Divide density by tow_depth per EDI 

NorthLakes_den <- NorthLakes %>% 
  dplyr::mutate(`density_num/m3` = density/subsite_level2)

#add dry weight, diet_cat and scientific names and rename 'group' column 

NorthLakes_dry_wt <- dplyr::left_join(NorthLakes_den, zoo_dry_wts, by= join_by(species)) %>%
  dplyr::select(-c('program','drymass_mg','drymass_source','additional.notes')) %>%
  dplyr::rename(taxa_group = group)


#add temperature data for Southern Lakes  <https://lter.limnology.wisc.edu/core-study-lakes/>
#note temp not by tow depths 

NorthLakes_temp <- NorthLakes_dry_wt %>% 
  mutate(temp_c = case_when(site %in% 'FI' ~ 24.6,
                            site %in% 'WI' ~ 24.2,
                            site %in% 'MO' ~ 23.9,
                            site %in% 'ME' ~ 23.2))



###################### North Lakes end  ##########################################






##remove species name from each dataframe if taxonomicLevel  does not equal species then each site will be 'ready' for next step. 

