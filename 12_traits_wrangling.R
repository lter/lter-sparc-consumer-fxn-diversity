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
trt_v1 <- read.csv(file.path("Data", "traits_tidy-data", "11_traits_harmonized.csv"))

# Check structure
dplyr::glimpse(trt_v1)

## --------------------------- ##
# Reorder Columns ----
## --------------------------- ##

# Following QC will be easier if columns are in a reasonable order
trt_v2 <- trt_v1 %>% 
  dplyr::relocate(source, family, genus, species, taxonomic.resolution, taxon, sex,
                  .before = dplyr::everything()) %>% 
  ## 'Actual' traits after that and group by trait category
  dplyr::relocate(migration, range_area_km2, .after = sex) %>%
  dplyr::relocate(dplyr::starts_with(c("age_life.span_", "age_maturity", "age_first.",
                                       "diet_")),
                  .after = range_area_km2) %>%
  dplyr::relocate(dplyr::contains(c("reproductive.rate", "reproductive.mode")),
                  .after = dplyr::starts_with("diet_")) %>%
  dplyr::relocate(dplyr::starts_with(c("length_", "weight_", "mass_", "volume_",
                                       "life.stage_", "metabolism_", "active.time_")),
                  .after = dplyr::starts_with("reproduction_"))
    
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
# Make True Numbers ----
## --------------------------- ##

# Identify all columns that should contain numbers
trt_numcols <- c(setdiff(x = names(trt_v5),
                         y = c("source", "family", "genus", "species", 
                               "taxonomic.resolution", "taxon", "sex", "migration")))
(trt_numcols <- trt_numcols[stringr::str_detect(string = trt_numcols, pattern = "_ordinal") != T])

# Check for non-numbers in these
supportR::num_check(data = trt_v5, col = trt_numcols)

# Fix non-numbers
trt_v6 <- trt_v5
## No such fixing currently needed

# Re-check for non-numbers in these
supportR::num_check(data = trt_v6, col = trt_numcols)

# Now make those all into real numbers
trt_v7 <- trt_v6 %>%
  dplyr::mutate(dplyr::across(.cols = dplyr::all_of(trt_numcols),
                              .fns = as.numeric))

# Check structure
dplyr::glimpse(trt_v7)

## --------------------------- ##
# Handle Synonymous Traits ----
## --------------------------- ##

# Some columns are essentially the same but different units
trt_v8 <- trt_v7 %>%
  ## Age traits - convert days to years & then ditch superseded columns\
  ### Life span
  dplyr::mutate(age_life.span_years = dplyr::case_when(
    !is.na(age_life.span_years) ~ age_life.span_years,
    !is.na(age_life.span_days) ~ age_life.span_days / 365,
    T ~ NA)) %>%
  dplyr::select(-age_life.span_days) %>%
  ### Maturity
  dplyr::mutate(age_maturity_years = dplyr::case_when(
    !is.na(age_maturity_years) ~ age_maturity_years,
    !is.na(age_maturity_days) ~ age_maturity_days / 365,
    T ~ NA)) %>%
  dplyr::select(-age_maturity_days) %>%
  ### Female maturity
  dplyr::mutate(age_maturity.female_years = dplyr::case_when(
    !is.na(age_maturity.female_years) ~ age_maturity.female_years,
    !is.na(age_maturity.female_days) ~ age_maturity.female_days / 365,
    T ~ NA)) %>%
  dplyr::select(-age_maturity.female_days) %>%
  ### Male maturity
  dplyr::mutate(age_maturity.male_years = dplyr::case_when(
    !is.na(age_maturity.male_years) ~ age_maturity.male_years,
    !is.na(age_maturity.male_days) ~ age_maturity.male_days / 365,
    T ~ NA)) %>%
  dplyr::select(-age_maturity.male_days) %>%
  ### First reproduction
  dplyr::mutate(age_first.reproduction_years = dplyr::case_when(
    !is.na(age_first.reproduction_days) ~ age_first.reproduction_days / 365,
    T ~ NA), .after = age_first.reproduction_days) %>%
  dplyr::select(-age_first.reproduction_days) %>%
  # Diet traits - integrate binary columns into ordinal variants
  ### Flesh out 'broad' trophic level
  dplyr::mutate(diet_trophic.level.broad_ordinal = dplyr::case_when(
    !is.na(diet_trophic.level.broad_ordinal) ~ tolower(diet_trophic.level.broad_ordinal),
    diet_herbivore.leaves_binary == 1  ~ "herbivore",
    diet_herbivore.flowers_binary == 1 ~ "herbivore",
    diet_herbivore.seeds_binary == 1 ~ "herbivore",
    diet_herbivore.fruits_binary == 1 ~ "herbivore",
    diet_invertivore_binary == 1 ~ "carnivore",
    diet_carnivore_binary == 1 ~ "carnivore",
    diet_omnivore.BENTHIC_binary == 1 ~ "omnivore",
    diet_omnivore.SURWCOL_binary == 1 ~ "omnivore",
    diet_herbivore.ALGPHYTO_binary == 1 ~ "herbivore",
    diet_herbivore.MASVASCU_binary == 1 ~ "herbivore",
    diet_detritivore.DETRITUS_binary == 1 ~ "detritivore",
    diet_invertivore.INVLVFSH_binary == 1 ~ "carnivore",
    diet_carnivore.FSHCRCRB_binary == 1 ~ "carnivore",
    T ~ NA)) %>%
  ### Flesh out 'specific' trophic level
  dplyr::mutate(diet_trophic.level.specific_ordinal = dplyr::case_when(
    !is.na(diet_trophic.level.specific_ordinal) ~ tolower(diet_trophic.level.specific_ordinal),
    diet_herbivore.leaves_binary == 1  ~ "herbivore",
    diet_herbivore.flowers_binary == 1 ~ "herbivore",
    diet_herbivore.seeds_binary == 1 ~ "granivore",
    diet_herbivore.fruits_binary == 1 ~ "frugivore",
    diet_invertivore_binary == 1 ~ "invertivore",
    diet_carnivore_binary == 1 ~ "carnivore",
    diet_omnivore.BENTHIC_binary == 1 ~ "omnivore",
    diet_omnivore.SURWCOL_binary == 1 ~ "omnivore",
    diet_herbivore.ALGPHYTO_binary == 1 ~ "herbivore aquatic",
    diet_herbivore.MASVASCU_binary == 1 ~ "herbivore terrestrial",
    diet_detritivore.DETRITUS_binary == 1 ~ "detritivore",
    diet_invertivore.INVLVFSH_binary == 1 ~ "invertivore",
    diet_carnivore.FSHCRCRB_binary == 1 ~ "aquatic predator",
    T ~ NA)) %>%
  ### Ditch all binary diet columns
  dplyr::select(-dplyr::starts_with(c("diet_herbivore", "diet_omnivore", "diet_carnivore",
                                      "diet_invertivore", "diet_detritivore"))) %>%
  # Reproduction traits - synonymize where possible
  ### Collapse binary reproductive modes into one ordinal column
  dplyr::mutate(reproduction_reproductive.mode_ordinal = dplyr::case_when(
    !is.na(reproduction_reproductive.mode_ordinal) ~ reproduction_reproductive.mode_ordinal,
    reproduction_reproductive.mode.direct_binary == 1 ~ "direct",
    reproduction_reproductive.mode.larval_binary == 1 ~ "larval",
    reproduction_reproductive.mode.viviparous_binary == 1 ~ "viviparous",
    T ~ NA), .before = reproduction_reproductive.mode.direct_binary) %>%
  dplyr::select(-dplyr::starts_with(c("reproduction_reproductive.mode."))) %>%
  ### Calculate offspring per year where possible
  dplyr::mutate(reproduction_reproductive.rate_num.offspring.per.year = dplyr::case_when(
    !is.na(reproduction_reproductive.rate_num.offspring.per.year) ~ reproduction_reproductive.rate_num.offspring.per.year,
    !is.na(reproduction_reproductive.rate_num.offspring.per.clutch.or.litter) & !is.na(reproduction_reproductive.rate_num.litter.or.clutch.per.year) ~ reproduction_reproductive.rate_num.offspring.per.clutch.or.litter * reproduction_reproductive.rate_num.litter.or.clutch.per.year,
    !is.na(reproduction_reproductive.rate_num.offspring.per.litter) & !is.na(reproduction_reproductive.rate_num.litter.per.year) ~ reproduction_reproductive.rate_num.offspring.per.litter * reproduction_reproductive.rate_num.litter.per.year,
    T ~ NA)) %>%
  # Size traits - convert to centimeters
  ### Adult length
  dplyr::mutate(length_adult_cm = dplyr::case_when(
    !is.na(length_adult_mm) ~ length_adult_mm / 10,
    T ~ NA)) %>%
  ### Adult max length
  dplyr::mutate(length_adult.max_cm = dplyr::case_when(
    !is.na(length_adult.max_cm) ~ length_adult.max_cm,
    !is.na(length_adult.max_mm) ~ length_adult.max_mm / 10,
    T ~ NA)) %>%
  ### Adult female length
  dplyr::mutate(length_adult.female.max_cm = dplyr::case_when(
    !is.na(length_max.female_mm) ~ length_max.female_mm / 10,
    T ~ NA)) %>%
  ### Adult male length
  dplyr::mutate(length_adult.male.max_cm = dplyr::case_when(
    !is.na(length_max.male_mm) ~ length_max.male_mm / 10,
    T ~ NA)) %>%
  ### Offspring length
  dplyr::mutate(length_offspring_cm = dplyr::case_when(
    !is.na(length_offspring_cm) ~ length_offspring_cm,
    !is.na(length_offspring_mm) ~ length_offspring_mm / 10,
    T ~ NA)) %>%
  ### Offspring max length
  dplyr::mutate(length_offspring.max_cm = dplyr::case_when(
    !is.na(length_offspring.max_mm) ~ length_offspring.max_mm / 10,
    T ~ NA)) %>%
  ### Offspring max length
  dplyr::mutate(length_offspring.min_cm = dplyr::case_when(
    !is.na(length_offspring.min_mm) ~ length_offspring.min_mm / 10,
    T ~ NA)) %>%
  ### Egg length
  dplyr::mutate(length_egg_cm = dplyr::case_when(
    !is.na(length_egg_mm) ~ length_egg_mm / 10,
    T ~ NA)) %>%
  dplyr::select(-dplyr::ends_with("_mm")) %>%
  # Active time - collapse binary options into ordinal
  dplyr::mutate(active.time_category_ordinal = dplyr::case_when(
    !is.na(active.time_category_ordinal) ~ tolower(active.time_category_ordinal),
    active.time_diurnal_binary == 1 ~ "diurnal",
    active.time_nocturnal_binary == 1 ~ "nocturnal",
    active.time_crepuscular_binary == 1 ~ "crepuscular",
    T ~ NA)) %>%
  dplyr::select(-active.time_diurnal_binary, -active.time_nocturnal_binary, -active.time_crepuscular_binary) %>%
  # And relocate some of these columns better
  dplyr::relocate(dplyr::starts_with(c("length_adult", "length_offspring", "length_egg")),
                  .after = dplyr::starts_with("reproduction_"))

# Re-check structure
dplyr::glimpse(trt_v8)

# We didn't lose anything intentionally, right?
supportR::diff_check(old = names(trt_v7), new = names(trt_v8))

## --------------------------- ##
# Export ----
## --------------------------- ##

# Make a final object
trt_v99 <- trt_v8

# Check structure
dplyr::glimpse(trt_v99)

# Identify the file name & path
trait_file <- "12_traits_wrangled.csv"
trait_path <- file.path("Data", "traits_tidy-data", trait_file)

# Export locally
write.csv(x = trt_v99, na = '', row.names = F, file = trait_path)

# End ----
