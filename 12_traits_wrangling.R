## ---------------------------------------------------- ##
# CFD - Trait Wrangling
## ---------------------------------------------------- ##
# Purpose:
## Do all post-harmonization quality control (QC)

# Load libraries
librarian::shelf(tidyverse, supportR)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Load Data ----
## --------------------------- ##

# Read in the harmonized trait data
trt_v1 <- read.csv(file.path("data", "12_traits_processed_data", "11_traits_harmonized.csv"))

# Check structure
dplyr::glimpse(trt_v1)

## --------------------------- ##
# Reorder Columns ----
## --------------------------- ##

# Following QC will be easier if columns are in a reasonable order
trt_v2 <- trt_v1 %>% 
  dplyr::relocate(source, family, genus, taxon, sex,
                  .before = dplyr::everything()) %>% 
  # Age in the middle
  dplyr::relocate(dplyr::contains(c("age")),
                  .after = taxon) %>% 
  # Length / weight stuff last
  dplyr::relocate(dplyr::contains(c("length", "weight")),
                  .after = dplyr::everything())

# Check structure
dplyr::glimpse(trt_v2)

# Make sure no columns were inadvertently lost
supportR::diff_check(old = names(trt_v1), new = names(trt_v2))

## --------------------------- ##
# Handle Missing Values ----
## --------------------------- ##

# Standardize missing values
trt_v3 <- trt_v2 %>% 
  # Replace zero-length characters with true NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(nchar(.) == 0,
                                              yes = NA, no = .))) %>% 
  # Handle other weird placeholder values for NA
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(. %in% c("Missing[\"NoInput\"]"),
                                              yes = NA, no = .)))

# Check structure
dplyr::glimpse(trt_v3)


## --------------------------- ##
# Parse Malformed Entries ----
## --------------------------- ##

# Some entries include bizarre format and text mixed in with 'actual' trait value
trt_v4 <- trt_v3 %>% 
  # Extract numbers where possible/relevant
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = ~ dplyr::case_when(
        ## Quantities that include decimals
        stringr::str_detect(string = ., pattern = "Quantity") & 
          stringr::str_detect(string = ., pattern = "\\.") ~ stringr::str_extract(
            string = ., 
            pattern = "[:digit:]{1,100000}\\.[:digit:]{1,100000}"),
        ## Quantities that _don't_ include decimals
        stringr::str_detect(string = ., pattern = "Quantity") & 
          stringr::str_detect(string = ., pattern = "\\.") != T ~ stringr::str_extract(
            string = ., 
            pattern = "[:digit:]{1,100000}"),
        ## Otherwise, retain the original entry
        T ~ as.character(.))))

# Re-check structure
dplyr::glimpse(trt_v4)

## --------------------------- ##
# Drop Empty Rows / Columns ----
## --------------------------- ##

# If there are any completely empty rows or columns, drop them
trt_v5 <- trt_v4 %>% 
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.))))

# What changed?
dim(trt_v4); dim(trt_v5)

# Check structure
dplyr::glimpse(trt_v5)

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make a final object
trt_v99 <- trt_v5

# Check structure
dplyr::glimpse(trt_v99)

# Identify the file name & path
trait_file <- "12_traits_wrangled.csv"
trait_path <- file.path("data", "12_traits_processed_data", trait_file)

# Export locally
write.csv(x = trt_v99, na = '', row.names = F, file = trait_path)

# End ----
