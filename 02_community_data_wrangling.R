#----------------------------------##
# SPARC - Consumer Functional Diversity (CFD)
###--------------------------------##

# Script author(s): Shalanda Grier, Li Kui, Nick Lyons
# Sites: Palmer LTER, Arctic LTER, North Lakes LTER, North Sea, English Channel


#Purpose
#1) Data cleaning for each site to include: species/scientific names, standardized densities, diet_cat and dry/wt per ind


#remaining questions/actions:
# some species don't have matching name in ITIS, double check the "Scientific Name" column; If for sure there is no matching species in ITIS, should be hardcode the taxa info at the end of the zoo_taxa_v2 data frame
# the taxa table has a species column, the same as scientific name; Right now, this code keep only the scientific name column; should we keep both columns?
# We remove the phylum and family columns from the dry weight data since we will get that info from ITIS; is that ok? -okay and DONE
# change the name from group to taxa_group so we don't have to change the code everywhere - DONE
# what if there are more other taxa got added, how do we update the taxa table? So far it looks like a mannual process?
# move the zoo_dry_wts data from the trait folder to the community folder. - DONE

# Load libraries
librarian::shelf(tidyverse, lter/ltertools, stringr, taxize, purrr)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

#options(scipen = 999)

##### user input###

run_species_check = "N" # "Y" indicate we need to run species check against ITIS "N" indicate we can skip the checking process

###  Wrangling zooplankton species names and pull kingdom, phylum, class, order, family, and species names from ITIS
# read in zooplankton dry weight data
zoo_dry_wts <- read.csv(file=file.path('Data', "community_raw-data", "Zooplankton_dry_ind_wt.csv"),na.strings=c("NA","NA ",""))


#remove instances of 'sp.',  'spp.' , 'sp'  , 'spp. '; the scientific names and the "species" column is the same.

zoo_dry_wts_d1 <- zoo_dry_wts %>%
  dplyr::mutate(scientific_name = stringr::str_replace(scientific_name, " sp.$| spp.$| sp$| spp. $", ""),
# Remove trailing spaces from 'text_column'
                scientific_name = str_trim(scientific_name, side = "right")) %>%
  dplyr::select(-species_name_ori, -phylum,-family) %>%
  dplyr::mutate_if(is.character, str_trim, side = "right")

#look for duplicates
# peace <- zoo_dry_wts_d1 %>%
#   dplyr::group_by(program,scientific_name) %>%
#   dplyr::summarise(n=n()) %>%
#   dplyr::filter(n>1)
# peace1 <- zoo_dry_wts_d1 %>%
#   dplyr::group_by(program,sp_code) %>%
#   dplyr::summarise(n=n()) %>%
#   dplyr::filter(n>1)

##########Only run this if we need to check the species against ITIS###############

if (run_species_check =="Y"){

zoo_taxa <- zoo_dry_wts_d1 %>%
 #select the scientific_name column. This column originally filled based on LTER sites reported species names
 dplyr::select(scientific_name)%>%
#Grab all unique species names
 dplyr::distinct() %>%
#Create an empty placeholder column to fill later
 dplyr::mutate(kingdom = NA,
               phylum = NA,
               class = NA,
               order = NA,
               family = NA,
               genus = NA,
               species = NA)


### add kingdom, phylum, order, family, genus and species name using taxize

for (i in 1:length(zoo_taxa$scientific_name)) {

  sp <- zoo_taxa[i, ]$scientific_name

  # Query species taxonomic information with error handling
  identified_species_names <- tryCatch(
    taxize::tax_name(
      sci = sp,
      get = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
      db = "itis",
      accepted = TRUE,
      ask = FALSE
    ),
    error = function(e) NULL
  )

  # Query species for common name with error handling; Note, ITIS does not have common names; IGNORE this step

  # Fill taxonomy info safely
  zoo_taxa[i, ]$kingdom <- if (!is.null(identified_species_names)) paste0(identified_species_names$kingdom, collapse = "") else NA
  zoo_taxa[i, ]$phylum  <- if (!is.null(identified_species_names)) paste0(identified_species_names$phylum, collapse = "") else NA
  zoo_taxa[i, ]$class   <- if (!is.null(identified_species_names)) paste0(identified_species_names$class, collapse = "") else NA
  zoo_taxa[i, ]$order   <- if (!is.null(identified_species_names)) paste0(identified_species_names$order, collapse = "") else NA
  zoo_taxa[i, ]$family  <- if (!is.null(identified_species_names)) paste0(identified_species_names$family, collapse = "") else NA
  zoo_taxa[i, ]$genus   <- if (!is.null(identified_species_names)) paste0(identified_species_names$genus, collapse = "") else NA
  zoo_taxa[i, ]$species <- if (!is.null(identified_species_names)) paste0(identified_species_names$species, collapse = "") else NA

} #close the for loop

# --- Error check at the end ---
# Find which species didn’t get a match
unmatched <- zoo_taxa[zoo_taxa$kingdom=="NA", "scientific_name"]

if (length(unmatched) > 0) {
  message("Warning: No ITIS records found for these species:\n",
          paste(unmatched, collapse = ", "))
} else {
  message("All species matched successfully!")
}

zoo_taxa_v2 <- zoo_taxa
#Replace the string "NA" with actual NA values
zoo_taxa_v2[zoo_taxa_v2 == "NA"] <- NA



write.csv(x = zoo_taxa_v2, row.names = F, na = '',
          file = file.path("Data", "community_tidy-data","02_zoo_taxa_checked.csv"))
} # close the species check

# if no need to check the taxa information, we can read the file directly
zoo_taxa_checked<- read.csv(file = file.path("Data", "community_tidy-data", "02_zoo_taxa_checked.csv"),na="")


# Left join our current tidy dataframe with the table of taxonomic info
zoo_taxa_ready <- left_join(zoo_dry_wts_d1, zoo_taxa_checked, by = "scientific_name") %>%
     dplyr::select(-c('drymass_mg','drymass_source','additional.notes')) %>%  #remove unnecessary columns
     dplyr::mutate(kingdom = case_when(
       scientific_name == "Copepod nauplii" & is.na(kingdom) ~ "Animalia",
       scientific_name == "Copepodites" & is.na(kingdom) ~ "Animalia",
       scientific_name == "Ctenophora" & is.na(kingdom) ~ "Animalia",
       scientific_name == "Gammaridea" & is.na(kingdom) ~ "Animalia",
       scientific_name == "Spongiobranchia" & is.na(kingdom) ~ "Animalia",
       TRUE ~ kingdom # keep existing values for kingdom column & remaining NA values
     ),
     phylum = case_when(
       scientific_name == "Copepod nauplii" & is.na(phylum) ~ "Arthropoda",
       scientific_name == "Copepodites" & is.na(phylum) ~ "Arthropoda",
       scientific_name == "Ctenophora" & is.na(phylum) ~ "Ctenophora",
       scientific_name == "Gammaridea" & is.na(phylum) ~ "Arthropoda",
       scientific_name == "Spongiobranchia" & is.na(phylum) ~ "Mollusca", #WoRMS assignment
       TRUE ~ phylum # keep existing values for phylum column & remaining NA values
     ),
     class = case_when(
       scientific_name == "Copepod nauplii" & is.na(class) ~ "Copepoda",
       scientific_name == "Copepodites" & is.na(class) ~ "Copepoda",
       scientific_name == "Gammaridea" & is.na(class) ~ "Malacostraca",
       scientific_name == "Spongiobranchia" & is.na(class) ~ "Gastropoda",
       TRUE ~ class # keep existing values for class column & remaining NA values
     ),
     order = case_when(
       scientific_name == "Spongiobranchia" & is.na(order) ~ "Pteropoda",
       TRUE ~ order # keep existing values for order column & remaining NA values
     ),
     family = case_when(
       scientific_name == "Spongiobranchia" & is.na(family) ~ "Pneumodermatidae",
       TRUE ~ family # keep existing values for family column & remaining NA values
     ),
     genus = case_when(
       scientific_name == "Spongiobranchia" & is.na(genus) ~ "Spongiobranchia",
       TRUE ~ genus #keep existing values for genus column & remaining NA values
     )
     )

##################### end taxa cleaning ###############





#### community data ##############
# read in harmonized data and start wrangling by project

com_dt<- read.csv(file = file.path("Data", "community_tidy-data", "01_community_harmonized.csv"))


#### General Wrangling of Harmonized Community Data
## attend to capitalization

#change capitalization of species names to lower case
com_dt2 <- com_dt %>%
  dplyr::mutate(species = tolower(species),
                #remove trailing spaces from species column
                species = str_trim(species, side = "right"),
                #capitilize the first letter of each species
                species = str_to_sentence(species),
                #change date from character to date and separate year, month, and day
                date = as.Date(date)
                ) %>%
  dplyr::mutate_if(is.character, str_trim, side = "right")

#### Arctic ###############

Arctic  <-com_dt2 %>%
  dplyr::filter(project == "Arctic")%>%
  dplyr::select(-class) #drop class only need for RLS


#convert density from num/l to num/m3

Arctic_den <- Arctic %>%
  dplyr::mutate(`density_num/m3` = density*1000,
                temp_c = "7.074284") %>%
  mutate(sp_code = species)

Arctic_ready <- Arctic_den
########################### end #############################

#########Palmer LTER

Palmer <-com_dt2 %>%
  dplyr::filter(project == "Palmer") %>%
  dplyr::select(-class) #drop class

#convert density from 1000m3 to 1m3.
#add temp data. SST retrieved from Palmer LTER weather station.
#Note average SST temp over duration of survey period 2009-2024.
#I believe every site had one temp value from CND or mean value for every year site combination? Not necessarily by depth.

Palmer_den <- Palmer %>%
  dplyr::mutate(`density_num/m3` = density/1000,
                 temp_c = "-0.4598606") %>%
  mutate(sp_code = species) %>%
  dplyr::filter(!density == -1) #remove all rows where species identified but not quantified


Palmer_ready <- Palmer_den
######### end ################

###### North Lakes LTER

NorthLakes <- com_dt2 %>%
  dplyr::filter(project == "NorthLakes") %>%
  dplyr::select(-class) #drop class

#convert density from m2 to m3. Divide density by tow_depth per EDI
#add temperature data for Southern Lakes  <https://lter.limnology.wisc.edu/core-study-lakes/>
#note temp not by tow depths

NorthLakes_den <- NorthLakes %>%
  dplyr::mutate(`density_num/m3` = density/subsite_level2)%>%
  mutate(temp_c = case_when(site %in% 'FI' ~ 24.6,
                            site %in% 'WI' ~ 24.2,
                            site %in% 'MO' ~ 23.9,
                            site %in% 'ME' ~ 23.2))
 NorthLakes_ready <- NorthLakes_den

###################### North Lakes end  ##########################################

#### Combine all sites (zooplankton) back together and add dry weight, diet_cat and scientific names

com_dt3 <- rbind(Arctic_ready, Palmer_ready, NorthLakes_ready) %>%
  dplyr::select(-c(density, kingdom,phylum, order, family, genus, species)) %>% #remove original density column
   #remove some duplicate column before joining, and use the taxa table information
#add dry weight, diet_cat and scientific names and rename group column
 dplyr::left_join(select(zoo_taxa_ready,-species), by= c("project","sp_code")) %>% # the "species" is the scientific name but from worms. #remove extra column names added after merge
 dplyr::rename_with(~ str_remove(., "\\..*")) %>%
 #dplyr::mutate(`density_num/m2` = NA) %>%
 dplyr::mutate(`dmperind_g/ind` = case_when(
   `density_num/m3` == 0 ~ 0,
   `density_num/m3`> 0 ~ drymass_g, #convert mg to g
    TRUE ~ NA
 )) %>%
   dplyr::mutate(`dmperind_g/ind`=ifelse(is.na(`drymass_g`),NA,`dmperind_g/ind`)) %>%
 dplyr::select(-drymass_g)


com_dt4 <- com_dt3 #  [,-31] #remove boolean column

##################################################


 ########################Reef Life Survey Australian Sites #############################

 RLS <- com_dt %>%
   dplyr::filter(project == "RLS")

 # call in dry weight conversions
 dm_conversion <- read.csv(file=file.path('Data', "community_raw-data", "dm_conversions_cndwg.csv"),na.strings=c("NA","NA ",""))


 #obtain individual biomass
 #subsite_level2 = method
 #subsite_level3 = block
 #for each block for method 1 a total of 250m2 surveyed
 #for each block for method 2 a total of 50m2 surveyed
 #there are two blocks for each method

 RLS_ind_bio <- RLS %>%
   dplyr::mutate(ind_bio = biomass_g/count_num) %>%
   dplyr::mutate(transectarea_m2 = case_when(
     subsite_level2 == 1 ~ 250,
     subsite_level2 == 2 ~ 50
   ))


 ##call in dry mass coeffeciet conversion data

 dm_filter <- dm_conversion %>%
   dplyr::filter(kingdom == "Animalia"&dm_wm_mean < 1)

 dm_coeff <- dm_filter %>%
   dplyr::group_by(class)%>%
   dplyr::summarise(dm_coeff= mean(as.numeric(dm_wm_mean),
                                   na.rm=TRUE), .groups = "drop")%>%
   dplyr::ungroup()

 #join dm_coeff with RLS survey data

 RLS_dm_coeff_data <- left_join(RLS_ind_bio, dm_coeff, by = "class")


 RLS_den_dm <- RLS_dm_coeff_data %>%
   dplyr::mutate(`dmperind_g/ind` = ind_bio *dm_coeff,
                 `density_num/m2` = count_num/transectarea_m2,
                 temp_c= case_when(
                   site %in% "Rottnest Island" ~ 21,# assign temperature
                   site %in% "Maria Island" ~ 17,
                   site %in% "Ningaloo Reef" ~ 24.9
                 )
   ) %>%
   dplyr::select(-ind_bio)

 
 RLS_and_BottomTrawl_Fish_dietcat <-read.csv(file=file.path('Data', "community_raw-data", "RLS_FISHGLOB_updated_diet.csv"),na.strings=c("NA","NA ",""),strip.white = T)

 diet_cat_1 <- RLS_and_BottomTrawl_Fish_dietcat[,-c(1,3:6,8:9)] %>%
   distinct()

 RLS_den_dm2 <- RLS_den_dm %>%
  left_join(diet_cat_1, by = c("sp_code" = "taxon"))


  RLS_ready <- RLS_den_dm2 #%>%
   #dplyr::select(-transectarea_m2) #keep the transectarea for later in case


 ############ end #################


 #############FISHGLOB Bottom trawl survey data North Sea and English Channel ######################
 # # we decide to remove this fighglob dataset for now.
 # FISHGLOB<- com_dt %>%
 #   dplyr::filter(project == "FISHGLOB")
 #
 # #dplyr::filter(!project %in% c("Arctic", "NorthLakes", "RLS", "Palmer")) # before fixing the issue with FISHGLOB data, metadata did not transfer over during harmonization
 #
 #
 # FISHGLOB_dm_coeff <- left_join(FISHGLOB, dm_coeff, by = "class")
 #
 # FISHGLOB_ind_bio <- FISHGLOB_dm_coeff %>%
 #   dplyr::mutate(biomass_g = biomass_kg*1000, #convert wet mass from kg to g
 #                 ind_bio = biomass_g/count_num, #calculate biomass per individual
 #                 `dmperind_g/ind` = ind_bio * dm_coeff, #calculate dry mass per individual in g
 #                 `density_num/m2` = `density_num_km2` /1000000, #convert density from km2 to m2
 #                 temp_c = case_when(
 #                   site %in% "FR-CGFS" ~ 17, #add temp data for North Sea
 #                   site %in% "NS-IBTS" ~ 16) #add temp data for English Channel
 #   )%>%
 #   dplyr::select(-ind_bio)
 #
 #
 # FISHGLOB_ind_bio1 <-  FISHGLOB_ind_bio %>%
 #   left_join(diet_cat_1, by = c("species" = "taxon"))
 #
 # FISHGLOB_ready <-FISHGLOB_ind_bio1

##############  end #############


 #combine fish data and add diet_cat information

 fish_com_dt1 <- RLS_ready %>%
   dplyr::select(-c(density))


###combine consumer data
#check differences in column names and check column names to prepare to combine all consumer data
#setdiff(names(com_dt4), names(fish_com_dt2))


fish_com_ready <- fish_com_dt1 %>%
  dplyr::rename(scientific_name = species)%>%
  dplyr::mutate(`density_num/m3` = NA) %>%
  dplyr::mutate(taxon_group = "fish")%>%
  dplyr::mutate(taxonomicLevel = NA) %>%
  dplyr::mutate(subsite_level3= NA) %>%
  dplyr::select(-c(dm_coeff))


# # desired columns with their NA type
needed <- list(length_cm = NA_integer_,
                biomass_kg = NA_real_,
                taxonomicLevel = NA_character_,
                `density_num/m2` = NA_real_,
               count_num = NA_integer_,
               transectarea_m2 = NA_real_,
               density_num_km2 = NA_real_ 
)

missing <- setdiff(names(needed), names(com_dt4))

zoo_com_ready<- com_dt4 %>% #add new column names so each dataframes matches. can delete later
      dplyr::mutate(!!!needed[missing])  

#setdiff(colnames(zoo_com_ready), colnames(fish_com_ready))
#sort(colnames(zoo_com_ready))
#sort(colnames(fish_com_ready))


all_com_data <- rbind(zoo_com_ready, fish_com_ready)


 ## --------------------------- ##
 # Export ----
 ## --------------------------- ##

 # Make one last version of the data
 com_v99 <-all_com_data

 # Check structure
 dplyr::glimpse(com_v99)

 # Identify the file name & path
 comm_file <- "02_community_wrangled.csv"
 comm_path <- file.path("Data", "community_tidy-data", comm_file)

 # Export locally
 write.csv(x =  com_v99, na = '', row.names = F, file = comm_path)

 # End ----


