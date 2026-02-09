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

## --------------------------- ##
# ITIS Species Checks ----
## --------------------------- ##

# Should these species be checked against ITIS?
run_taxa_check <- FALSE

# If desired (see prior line), check the species against ITIS
if(run_taxa_check == T){

  # Make a list for storing outputs
  itis_list <- list()

  # Pare down to just unique scientific names
  all_spp <- spp_v4 |>   
    dplyr::pull(scientific_name) |> 
    unique() |> sort()

for (i in seq_along(all_spp)) {
  # i <- 1
  
  # Grab that species name
  sp_name <- all_spp[i]

  # Query species taxonomic information with error handling
  located_species_names <- tryCatch(
    taxize::tax_name(
      sci = sp_name,
      get = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
      db = "itis", accepted = TRUE, ask = FALSE),
    error = function(e) NULL
  )
  
  # Add this to the list
  itis_list[[sp_name]] <- located_species_names } # close the loop
  
  # Unlist the list
  itis_df <- purrr::list_rbind(x = itis_list)

  # Check structure
  dplyr::glimpse(itis_df)

  # Export a CSV of species where we fail to identify info from ITIS
  itis_missing <- dplyr::filter(itis_df, is.na(kingdom))
  write.csv(x = itis_missing, na = '', row.names = F,
    file = file.path("Data", "checks", "22_species-check_missing-from-ITIS.csv"))
  
  # Get just the non-missing ITIS info
  itis_actual <- dplyr::filter(itis_df, !is.na(kingdom))

  # Export it
  write.csv(x = itis_actual, na = '', row.names = F,
    file = file.path("Data", "species_tidy-data", "22_species_ITIS-taxonomy.csv"))  
    
} # Close the conditional

# If not re-checking ITIS, read in last check's outputs
itis_actual <- read.csv(file = file.path("Data", "species_tidy-data", "22_species_ITIS-taxonomy.csv"))

# Check structure
dplyr::glimpse(itis_actual)

# Safely join to the data
spp_v5 <- spp_v4 |> 
  dplyr::left_join(y = itis_actual, by = c("scientific_name" = "query"))

# Check structure of the result
dplyr::glimpse(spp_v5)

## --------------------------- ##
# Tidy Integrated ITIS Info ----
## --------------------------- ##

# Now that we've combined it, let's do some tidying of the 'with ITIS' data
spp_v6 <- spp_v5 |> 
  dplyr::mutate(kingdom = dplyr::coalesce(kingdom.x, kingdom.y),
    phylum = dplyr::coalesce(phylum.x, phylum.y),
    class = dplyr::coalesce(class.x, class.y),
    order = dplyr::coalesce(order.x, order.y),
    family = dplyr::coalesce(family.x, family.y),
    genus = dplyr::coalesce(genus.x, genus.y)) |> 
  # Tidy specific epithet info too
  tidyr::separate_wider_delim(cols = species, delim = " ", 
    names = c("itis_genus", "itis_epithet")) |> 
  dplyr::mutate(epithet = dplyr::coalesce(specific_epithet, itis_epithet)) |> 
  # Relocate columns slightly
  dplyr::relocate(dplyr::starts_with("itis_"), .after = epithet) |> 
    # Ditch superseded/unwanted columns
  dplyr::select(-dplyr::ends_with(c(".x", ".y")), -db, -specific_epithet)

# Check structure
dplyr::glimpse(spp_v6)

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make one last version of the data
spp_v99 <- spp_v6

# Check structure
dplyr::glimpse(spp_v99)

# Identify the file name & path
spp_file <- "22_species_wrangled.csv"
spp_path <- file.path("Data", "species_tidy-data", spp_file)

# Export locally
write.csv(x = spp_v99, na = '', row.names = F, file = spp_path)

# End ----
