## ---------------------------------------------------- ##
# CFD - Species Wrangling
## ---------------------------------------------------- ##
# Purpose:
## Data cleaning and taxa standardization for terrestrial species data

# Script author(s): Shalanda Grier, Li Kui, Nick Lyons


#Load libraries
librarian::shelf(tidyverse, stringr, taxize)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

# Read in the harmonized species data
species_list_v1 <- read.csv(file.path("Data", "species_tidy-data","01_species_harmonized.csv"),stringsAsFactors = F,na.strings =c("")) 


run_taxa_check = "N" # "Y" indicate we need to run species check against ITIS "N" indicate we can skip the checking process


### fill in scientific_name column for projects: HARVARD, PHOENIX, KONZA, SEV and SBC_BEACH


########## HARVARD wrangling ###############

HARVARD_v1 <- species_list_v1 %>%
  dplyr::filter(project %in% "HARVARD") %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::relocate(c("kingdom", "phylum", "class"), .before = order) %>%
  dplyr::distinct(scientific_name,.keep_all = TRUE)



HARVARD_ready <- HARVARD_v1


############# end ######################


############# PHOENIX ###########################

PHOENIX_v1 <- species_list_v1 %>%
  dplyr::filter(project %in% "PHOENIX") %>%
  dplyr::filter(!is.na(scientific_name)) %>%
  dplyr::distinct(scientific_name,.keep_all = TRUE)

PHOENIX_ready <- PHOENIX_v1


########### KONZA wrangling ######################
KONZA_v1 <- species_list_v1 %>%
  dplyr::filter(project %in% "KONZA") %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::relocate(c("kingdom", "phylum", "class"), .before = order) 


#remove unknown and unidentified species 

KONZA_v2 <- KONZA_v1 %>%
  dplyr::filter(!str_detect(scientific_name, "NA$")) %>% #remove species with suffix 'NA' 
  dplyr::filter(!str_detect(scientific_name, "^unk")) %>%   #remove species with unknown scientific name - unk #
  dplyr::filter(!grepl("\\d", scientific_name)) %>% #remove "sp."; "sp" and scientific_name with numbered species identifiers
  dplyr::filter(!grepl("\\?", scientific_name)) %>%  #remove species with ? in name
  dplyr::filter(sapply(strsplit(scientific_name, " "), length) <= 2) %>% #remove species with more than two names to include (), etc
  dplyr::filter(!grepl("\\(|\\)", scientific_name)) %>%   #remove species with / in name
  dplyr::filter(!str_detect(scientific_name, "^NA")) %>%     #remove species that begin with "NA_"
  dplyr::distinct(scientific_name,.keep_all = TRUE) %>% # keep unique species names
  dplyr::mutate(scientific_name = str_replace_all(scientific_name, pattern = "\\s+(sp\\.|spp\\.|sp|spp)$", replacement = ""))


KONZA_ready <- KONZA_v2


############ end ########################


########### SEV #########################
SEV_v1 <- species_list_v1 %>%
  dplyr::filter(project %in% "SEV") %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::relocate(c("kingdom", "phylum", "class"), .before = order)%>%
  dplyr::distinct(scientific_name,.keep_all = TRUE) %>%
  dplyr::mutate(scientific_name = str_replace_all(scientific_name, pattern = "\\s+(sp\\.|spp\\.|sp|spp)$", replacement = ""))


SEV_ready <- SEV_v1


######## end ############################

###### SBC #############################

SBC_BEACH_v1 <- species_list_v1 %>%
  dplyr::filter(project %in% "SBC_BEACH") %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::relocate(c("kingdom", "phylum", "class"), .before = order) %>%
  dplyr::filter(class == "Aves") %>% #filter to remove all mammals and inanimate observations
  dplyr::distinct(scientific_name,.keep_all = TRUE) %>%
  dplyr::mutate(scientific_name = str_replace_all(scientific_name, pattern = "\\s+(sp\\.|spp\\.|sp|spp)$", replacement = "")) %>% # remove suffix sp.; sp; spp 
  dplyr::filter(!str_detect(scientific_name, "\\d")) # remove rows with numbers -99999 na indicator 


SBC_BEACH_ready <- SBC_BEACH_v1

###### end ###############################



#combine all sites back together once error in harmonization fixed  

#sites_v00 <- species_list_v1 %>%
  #dplyr::filter(project %in% c("HUBBARD", "MOHWAK"))

sites_v00 <- species_list_v1 %>%
  dplyr::filter(project %in% c("HUBBARD"))

wrangled_sites_00 <- rbind(HARVARD_ready, PHOENIX_ready, KONZA_ready, SEV_ready, SBC_BEACH_ready)


all_sites <- rbind(sites_v00, wrangled_sites_00)



##########Only run this if we need to check the species against ITIS###############

if (run_taxa_check =="Y"){
  
  terrestrial_taxa<- all_sites %>%
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

for (i in 1:length(terrestrial_taxa $scientific_name)) {
  
  sp_name <- terrestrial_taxa[i, ]$scientific_name
  
  # Query species taxonomic information with error handling
  located_species_names <- tryCatch(
    taxize::tax_name(
      sci = sp_name,
      get = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
      db = "itis",
      accepted = TRUE,
      ask = FALSE
    ),
    error = function(e) NULL
  )
  
  # Fill taxonomy info safely
  terrestrial_taxa[i, ]$kingdom <- if (!is.null(located_species_names)) paste0(located_species_names$kingdom, collapse = "") else NA
  terrestrial_taxa[i, ]$phylum  <- if (!is.null(located_species_names)) paste0(located_species_names$phylum, collapse = "") else NA
  terrestrial_taxa[i, ]$class   <- if (!is.null(located_species_names)) paste0(located_species_names$class, collapse = "") else NA
  terrestrial_taxa[i, ]$order   <- if (!is.null(located_species_names)) paste0(located_species_names$order, collapse = "") else NA
  terrestrial_taxa[i, ]$family  <- if (!is.null(located_species_names)) paste0(located_species_names$family, collapse = "") else NA
  terrestrial_taxa[i, ]$genus   <- if (!is.null(located_species_names)) paste0(located_species_names$genus, collapse = "") else NA
  terrestrial_taxa[i, ]$species <- if (!is.null(located_species_names))paste0(located_species_names$species, collapse = "") else NA
  
} #close the for loop
  
  
  # --- Error check at the end ---
  # Find which species didn’t get a match
  unmatched_taxa <- terrestrial_taxa[terrestrial_taxa$kingdom=="NA", "scientific_name"]
  
  if (length(unmatched_taxa) > 0) {
    message("Warning: No ITIS records found for these species:\n",
            paste(unmatched_taxa, collapse = ", "))
  } else {
    message("All species matched successfully!")
  } 
  
  terrestrial_taxa_v2 <- terrestrial_taxa
  #Replace the string "NA" with actual NA values
  terrestrial_taxa_v2[terrestrial_taxa_v2 == "NA"] <- NA
  
  
  
  write.csv(x = terrestrial_taxa_v2, row.names = F, na = '',
            file = file.path("Data", "species_tidy-data","02_terrestrial_taxa_checked.csv"))
} # close the species check 
#more than 100 taxa without itis records 
#will address at later date 


## join all_sites species data with taxonomic information from itis 
terrestrial_taxa_ready <- left_join(all_sites, terrestrial_taxa_v2, by = "scientific_name") %>%
  dplyr::select(-ends_with(".x")) %>%
  dplyr::select(-taxon_group) %>%
  dplyr::rename_with(~ str_remove(., "\\.y$"), ends_with(".y"))


# Make one last version of the data
terr_taxa_v99 <-terrestrial_taxa_ready

# Check structure
dplyr::glimpse(terr_taxa_v99)

# Identify the file name & path
terr_taxa_file <- "02_species_wrangled.csv"
terr_taxa_path <- file.path("Data", "species_tidy-data", terr_taxa_file)

# Export locally
write.csv(x = terr_taxa_v99, na = '', row.names = F, file = terr_taxa_file)


# End ----


