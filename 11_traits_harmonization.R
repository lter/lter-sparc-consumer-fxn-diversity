## ---------------------------------------------------- ##
# CFD - Trait Harmonization
## ---------------------------------------------------- ##
# Purpose:
## Combine separate trait tables into a single, "harmonized" traits table

# Load libraries
librarian::shelf(tidyverse, ltertools, supportR)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Read in Keys ----
## --------------------------- ##

# Read in 'regular' trait key
trt_key_v0 <- read.csv(file = file.path("data", "00_keys", "traits_datakey.csv"))

# Make sure it's all pared down to the needed content
trt_key <- ltertools::check_key(key = trt_key_v0)

# Check structure
dplyr::glimpse(trt_key)

# Also read in the key for long-format traits
trtlong_key_v0 <- read.csv(file = file.path("Data", "00_keys", "long_traits_datakey.csv"))
trtlong_key <- ltertools::check_key(key = trtlong_key_v0 )

# Check structure
dplyr::glimpse(trtlong_key)

## --------------------------- ##
# Use Data Keys ----
## --------------------------- ##

# Harmonize the 'regular' trait data
trt_v1 <- ltertools::harmonize(key = trt_key, data_format = "csv",
                               raw_folder = file.path("Data", "11_traits_raw_data"))

# Check structure
dplyr::glimpse(trt_v1)

# Do the same for the long-format traits
trtlong_v1 <- ltertools::harmonize(key = trtlong_key, data_format = "csv",
                               raw_folder = file.path("Data", "11_traits_raw_data"))

# Check structure
dplyr::glimpse(trtlong_v1)

## --------------------------- ##
# Fix Long Format Trait Data ----
## --------------------------- ##

# Eventually, we want all trait data in wide format
## So we need to tidy up the long format trait data and flip it to that 'shape'

# What trais are in long format?
sort(unique(trtlong_v1$trait_name))

# Do needed tidying
trtlong_v2 <- trtlong_v1 %>% 
  # Pare down to only traits of interest
  dplyr::filter(trait_name %in% c("Mass", "Activity", "wetWeight", 
                                  "bodyLengthMax", "reproductionMode", 
                                  "clutchSize", "eggWeight", "trophicGroup")) %>% 
  # Standardize trait names
  dplyr::mutate(trait_std = dplyr::case_when(
    ## Reproductive traits
    trait_name %in% c("clutchSize") ~ "reproductive.rate.num",
    trait_name %in% c("reproductionMode") ~ "reproduction.mode",
    ## Temperature traits
    trait_name %in% c("CTmax") ~ "ct.temperature.max",
    trait_name %in% c("CTmin") ~ "ct.temperature.min",
    trait_name %in% c("Tmerge") ~ "temperature.merge",
    trait_name %in% c("Tpref") ~ "temperature.preference",
    trait_name %in% c("Tbask") ~ "temperature.bask",
    trait_name %in% c("Tforage_max") ~ "temperature.forage.max",
    trait_name %in% c("Tforage_min") ~ "temperature.forage.min",
    ## Size traits
    trait_name %in% c("Mass") ~ "mass",
    trait_name %in% c("wetWeight") ~ "weight.wet",
    trait_name %in% c("eggWeight") ~ "weight.egg",
    trait_name %in% c("bodyLengthMax") ~ "length.body.max",
    ## Others
    trait_name %in% c("Activity") ~ "activity.time",
    trait_name %in% c("trophicGroup") ~ "trophic.level.category",
    ## If not standardized, add a flag
    T ~ "NOT FIXED"))

# Do any trait names still need to be standardized?
trtlong_v2 %>% 
  dplyr::filter(trait_std == "NOT FIXED") %>% 
  dplyr::select(trait_name, trait_std) %>% 
  dplyr::distinct()

# Check structure
dplyr::glimpse(trtlong_v2)

# What units are used for our desired traits?
sort(unique(trtlong_v2$trait_unit))

# More necessary wrangling
trtlong_v3 <- trtlong_v2 %>% 
  # Tidy up units (if necessary)
  dplyr::mutate(trait_unit = dplyr::case_when(
    trait_unit == "eggs clutch^-1" ~ "eggs.per.clutch",
    T ~ trait_unit))

# Check structure
dplyr::glimpse(trtlong_v3)

# Separate numeric and non-numeric traits
## Numeric traits are those that R can 'force' into being numbers
trtlong_v3_num <- trtlong_v3 %>%
  dplyr::filter(is.na(supportR::force_num(x = trait_value)) != T)
## Non-numeric traits would be changed into NAs by R forcing numeric interpretation
trtlong_v3_char <- trtlong_v3 %>%
  dplyr::filter(is.na(supportR::force_num(x = trait_value)) == T)

# Didn't lose any rows, right?
nrow(trtlong_v3) == (nrow(trtlong_v3_num) + nrow(trtlong_v3_char))

# Collapse each of these into one observation per taxon as appropriate for the type of trait value
## Average numbers
trtlong_v3_num_avg <- trtlong_v3_num %>% 
  dplyr::mutate(trait_value = as.numeric(trait_value)) %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(names(.), c("trait_value"))))) %>% 
  dplyr::summarize(trait_value = mean(trait_value, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(trait_value = as.character(trait_value))
## Concatenate characters
trtlong_v3_char_concat <- trtlong_v3_char %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(names(.), c("trait_value"))))) %>% 
  dplyr::summarize(trait_value = paste(unique(trait_value), collapse = "; "),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Recombine regardless of trait value type (now that we have 1 obs / species)
trtlong_v4 <- dplyr::bind_rows(trtlong_v3_num_avg, trtlong_v3_char_concat) %>% 
  # Assemble wide-format trait column names
  dplyr::mutate(trait_columns = ifelse(!is.na(trait_unit) & nchar(trait_unit) != 0,
                                       yes = paste(trait_std, trait_unit, sep = "_"),
                                       no = trait_std)) %>% 
  # Ditch superseded columns
  dplyr::select(-trait_name, -trait_unit, -trait_std) %>% 
  # And pivot to wide format with our new (tidy) trait names
  tidyr::pivot_wider(names_from = trait_columns,
                     values_from = trait_value)

# Check structure
dplyr::glimpse(trtlong_v4)

## --------------------------- ##
# Combine Long & Wide Format Traits ----
## --------------------------- ##

# Now that originally wide and long format traits are both in wide format, we can combine
trt_v2 <- dplyr::bind_rows(trt_v1, trtlong_v4)

# Check structure
dplyr::glimpse(trt_v2)

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make a final object
trt_v99 <- trt_v2

# Check structure
dplyr::glimpse(trt_v99)

# Identify the file name & path
trait_file <- "11_traits_harmonized.csv"
trait_path <- file.path("Data", "12_traits_processed_data", trait_file)

# Export locally
write.csv(x = trt_v99, na = '', row.names = F, file = trait_path)

# End ----
