## ---------------------------------------------------- ##
# CFD - Species Wrangling
## ---------------------------------------------------- ##
# Purpose:
## Data cleaning and taxa standardization for terrestrial species data

# Load libraries
librarian::shelf(tidyverse,  taxize)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Load Data ----
## --------------------------- ##

# Read in the harmonized species data
spp_v1 <- read.csv(file.path("Data", "species_tidy-data", "21_species_harmonized.csv")) 

# Check structure
dplyr::glimpse(spp_v1)

## --------------------------- ##
# Misc. Housekeeping ----
## --------------------------- ##

# Let's do some quality of life tweaks
spp_v2 <- spp_v1 |> 
  # Repair/remove malformed entries
  dplyr::mutate(
    dplyr::across(.cols = dplyr::everything(), 
                  .fns = ~ stringi::stri_escape_unicode(str = stringi::stri_trans_general(., "latin-ascii") ))) |> 
  dplyr::mutate(
    dplyr::across(.cols = dplyr::everything(), 
                  .fns = ~ gsub(pattern = "\\\\ufffd", replacement = "", x = .))) |> 
  # Change all empty cells to true NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(test = nchar(.) == 0, yes = NA, no = .))) |> 
  # Remove any rows where no taxonomic information is included
  dplyr::filter(!is.na(scientific_name) | !is.na(order) | !is.na(family) | !is.na(genus) | !is.na(species) | !is.na(phylum) | !is.na(class) | !is.na(taxon_group) | !is.na(kingdom))

# Check structure
dplyr::glimpse(spp_v2)

## --------------------------- ##
# Fix Scientific Name Info ----
## --------------------------- ##

# Need to standardize how the scientific name is listed
spp_v3 <- spp_v2 |> 
  # Filter irreparable issues
  ## Genera issues
  dplyr::filter(!genus %in% c("sp.", paste0("unk", c(".", 1:3)))) |> 
  dplyr::filter(stringr::str_detect(string = genus, pattern = "\\?") != T) |> 
  ## Species issues
  dplyr::filter(nchar(species) > 1 & !species %in% c("(HALIDAY)", "(LINNAEUS)", "(SAY)")) |> 
  dplyr::filter(stringr::str_detect(string = species, pattern = "\\d") != T) |> 
  dplyr::filter(stringr::str_detect(string = species, pattern = "\\?") != T) |> 
  dplyr::filter(stringr::str_detect(string = species, pattern = "&") != T) |> 
  # Repair what we can
  dplyr::mutate(sci = dplyr::case_when(
    is.na(scientific_name) & (!is.na(genus) & !is.na(species)) ~ paste0(genus, " ", species),
    is.na(scientific_name) & (!is.na(genus) & is.na(species)) ~ paste0(genus, " sp."),
    T ~ scientific_name))

# Count NAs before/after
supportR::count(vec = spp_v2$scientific_name) |> filter(is.na(value))
supportR::count(vec = spp_v3$sci) |> filter(is.na(value))

## --------------------------- ##
# Tidy Rows/Columns ----
## --------------------------- ##

# Let's do some general cleaning
spp_v4 <- spp_v3 |> 
  # Reorder columns by taxonomic granularity
  dplyr::relocate(kingdom, phylum, class, order, family, genus, species, sci, 
    .after = source) |> 
  # Remove superseded scientific name column
  dplyr::select(-scientific_name) |> 
  # Clarify actual species name column
  dplyr::rename(scientific_name = sci, 
    specific_epithet = species) |> 
  # Keep only unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(spp_v4)

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


