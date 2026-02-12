## ---------------------------------------------------- ##
# CFD - Species Wrangling
## ---------------------------------------------------- ##
# Purpose:
## Data cleaning and taxa standardization for terrestrial species data

# Load libraries
librarian::shelf(tidyverse, supportR, taxize)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Load Data ----
## --------------------------- ##

run_species_check <- "N"

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
  # Remove weird bonus spaces from some family names
  dplyr::mutate(dplyr::across(.cols = order:family,
    .fns = ~ gsub(pattern = "[[:space:]]", replacement = "", x = .))) |> 
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
  dplyr::distinct() |>
  # remove the homo sapiens from the sbc beach data
  dplyr::filter(!scientific_name %in% c("Homo sapiens"))

# Check structure
dplyr::glimpse(spp_v4)

##########Only run this if we need to check the species against ITIS###############


if (run_species_check =="Y"){

 #read in the species check file if it exists, if not create a new one and run the check. This file will be updated with the new species that are added to the dataset and will be used for future checks.
  if (file.exists(file.path("Data", "species_tidy-data", "22_terrestrial_species_ITIS-taxonomy.csv"))) {
    message("Species check file already exists. Loading existing file.")
   spe_check <- read.csv(file.path("Data", "species_tidy-data", "22_terrestrial_species_ITIS-taxonomy.csv"),stringsAsFactors = F,na.strings =c(""))
   #spe_check <- spe_check %>%
   #  select(-db) %>%
   #  rename(scientific_name = query)
  } else {
    message("Species check file does not exist. Creating new file and running species check.")
    spe_check <- data.frame(scientific_name = character(),
                            kingdom = character(),
                            phylum = character(),
                            class = character(),
                            order = character(),
                            family = character(),
                            genus = character(),
                            species = character(),
                            stringsAsFactors = FALSE)
  }

spe_check 1 <- spe_check %>%
  dplyr::filter(!is.na(kingdom)) 

taxa_check <- spp_v4 %>%
 #select the scientific_name column. This column originally filled based on LTER sites reported species names
 dplyr::select(scientific_name)%>%
#Grab all unique species names
 mutate(scientific_name = sub("\\s+spp?\\.?$", "", scientific_name),
        scientific_name = trimws(scientific_name)) %>%
 dplyr::distinct() %>%
#exclude the species that have already been checked and have a match in the spe_check1 dataset
 dplyr::filter(!scientific_name %in% spe_check1$scientific_name) %>%
#Create an empty placeholder column to fill later
 dplyr::mutate(kingdom = NA,
               phylum = NA,
               class = NA,
               order = NA,
               family = NA,
               genus = NA,
               species = NA)


### add kingdom, phylum, order, family, genus and species name using taxize

for (i in 1:length(taxa_check$scientific_name)) {

  sp <- taxa_check[i, ]$scientific_name

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

  # Fill taxonomy info safely
  taxa_check[i, ]$kingdom <- if (!is.null(identified_species_names)) paste0(identified_species_names$kingdom, collapse = "") else NA
  taxa_check[i, ]$phylum  <- if (!is.null(identified_species_names)) paste0(identified_species_names$phylum, collapse = "") else NA
  taxa_check[i, ]$class   <- if (!is.null(identified_species_names)) paste0(identified_species_names$class, collapse = "") else NA
  taxa_check[i, ]$order   <- if (!is.null(identified_species_names)) paste0(identified_species_names$order, collapse = "") else NA
  taxa_check[i, ]$family  <- if (!is.null(identified_species_names)) paste0(identified_species_names$family, collapse = "") else NA
  taxa_check[i, ]$genus   <- if (!is.null(identified_species_names)) paste0(identified_species_names$genus, collapse = "") else NA
  taxa_check[i, ]$species <- if (!is.null(identified_species_names)) paste0(identified_species_names$species, collapse = "") else NA

} #close the for loop

# --- Error check at the end ---
# Find which species didn’t get a match
unmatched <- taxa_check[taxa_check$kingdom=="NA", "scientific_name"]

if (length(unmatched) > 0) {
  message("Warning: No ITIS records found for these species:\n",
          paste(unmatched, collapse = ", "))
} else {
  message("All species matched successfully!")
}

taxa_check_v2 <- taxa_check
#Replace the string "NA" with actual NA values
taxa_check_v2[taxa_check_v2 == "NA"] <- NA

#combine with spe_check1 to get the full list of species and their taxonomic information
taxa_check_v3<- rbind(spe_check1, taxa_check_v2) %>%
  arrange(kingdom, phylum, class, order, family, genus, species)
  
write.csv(x = taxa_check_v3, row.names = F, na = '',
          file = file.path("Data", "species_tidy-data", "22_terrestrial_species_ITIS-taxonomy.csv"))
} # close the species check


# If not re-checking ITIS, read in last check's outputs
itis_actual <- read.csv(file = file.path("Data", "species_tidy-data", "22_terrestrial_species_ITIS-taxonomy.csv")) %>%
  #change the name to scientific_name_check to avoid confusion with the scientific_name column in the comball dataset
  dplyr::rename(scientific_name_check = scientific_name) %>%
  dplyr::select(-species) # remove the species column since it is not needed for the merge

# Check structure
dplyr::glimpse(itis_actual)

#extract the column name list from the spp_v4 dataset to use for the merge later
col_list <- colnames(spp_v4)

# Safely join to the data
spp_v5 <- spp_v4 %>%
  dplyr::mutate(scientific_name_check = scientific_name) %>%
  dplyr::left_join(itis_actual, by = "scientific_name_check") %>%
    dplyr::mutate(kingdom = if_else(is.na(kingdom.y), kingdom.x, kingdom.y),
                  phylum = if_else(is.na(phylum.y), phylum.x, phylum.y),
                  class = if_else(is.na(class.y), class.x, class.y),
                  order = if_else(is.na(order.y), order.x, order.y),
                  family = if_else(is.na(family.y), family.x, family.y),
                  genus = if_else(is.na(genus.y), genus.x, genus.y)) %>%
  dplyr::select(all_of(col_list))

# Check structure of the result
dplyr::glimpse(spp_v5)

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make one last version of the data
spp_v99 <- spp_v5

# Check structure
dplyr::glimpse(spp_v99)

# Identify the file name & path
spp_file <- "22_species_wrangled.csv"
spp_path <- file.path("Data", "species_tidy-data", spp_file)

# Export locally
write.csv(x = spp_v99, na = '', row.names = F, file = spp_path)

# End ----
