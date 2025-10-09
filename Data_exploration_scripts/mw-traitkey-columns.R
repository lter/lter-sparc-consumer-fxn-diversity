# Script Details ----------------------------------------------------------
###project: LTER SPARC Consumer Functional Diversity
###author(s): MW
###goal(s): generate trait key for my assigned datasets
###date(s): October 2025
###note(s): 

# Housekeeping ------------------------------------------------------------
### load necessary libraries ----
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, scales, broom, purrr, dataRetrieval,
                 splitstackshape)

### define custom functions ----
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

### load necessary data ----
### proprietary data ---
trait1 <- read_csv('../../../../../../Downloads/trait_data_reported.csv')
glimpse(trait1)
nacheck(trait1)

trait2 <- read_csv('../../../../../../Downloads/ATraiU_summary_values_2020AUG.csv')
glimpse(trait2)
nacheck(trait2)

tidynames <- read_csv('../../../../../../Downloads/tidynames.csv')

# first trait database ----------------------------------------------------
trait_df1 <- tibble(raw_name = names(trait1)) |> 
      mutate(column = row_number()) |> 
      select(column, raw_name) |> 
      mutate(
            trait_database = "COMBINE",
            data_type = 'trait',
            source = 'trait_data_reported.csv',
            tidy_name = NA_character_,
            units = NA_character_,
            na_indicator = "NA",
            trait_values = NA_character_,
            notes = NA_character_
      ) |> 
      mutate(tidy_name = case_when(
            raw_name == "family" ~ "family",
            raw_name == "genus" ~ "genus",
            raw_name == "iucn2020_binomial" ~ "taxon",
            raw_name == "adult_mass_g" ~ "body_mass",
            raw_name == "max_longevity_d" ~ "life_span",
            raw_name == "maturity_d" ~ "age_maturity ",
            raw_name == "age_first_reproduction_d" ~ "age_reproduction",
            raw_name == "neonate_mass_g" ~ "offspring_mass",
            raw_name == "home_range_km2" ~ "range",
            raw_name == "trophic_level" ~ "trophic_level",
            raw_name == "trophic_level" ~ "diet_category",
            raw_name == "activity_cycle" ~ "active_time",
            TRUE ~ NA_character_
      )) |> 
      mutate(units = case_when(
            raw_name == "family" ~ "character",
            raw_name == "genus" ~ "character",
            raw_name == "iucn2020_binomial" ~ "character",
            raw_name == "adult_mass_g" ~ "g",
            raw_name == "max_longevity_d" ~ "d",
            raw_name == "maturity_d" ~ "d",
            raw_name == "age_first_reproduction_d" ~ "d",
            raw_name == "neonate_mass_g" ~ "g",
            raw_name == "home_range_km2" ~ "km2",
            # raw_name == "trophic_level" ~ "ordinal",
            raw_name == "trophic_level" ~ "ordinal",
            raw_name == "activity_cycle" ~ "ordinal",
            TRUE ~ NA_character_
      )) |> 
      mutate(trait_values = case_when(
            # raw_name == "family" ~ "character",
            # raw_name == "genus" ~ "character",
            # raw_name == "iucn2020_binomial" ~ "character",
            # raw_name == "adult_mass_g" ~ "gram",
            # raw_name == "max_longevity_d" ~ "days",
            # raw_name == "maturity_d" ~ "days",
            # raw_name == "age_first_reproduction_d" ~ "days",
            # raw_name == "neonate_mass_g" ~ "grams",
            # raw_name == "home_range_km2" ~ "km2",
            # raw_name == "trophic_level" ~ "ordinal",
            raw_name == "trophic_level" ~ "1=herbivore, 2=omnivore, 3=carnivore",
            raw_name == "activity_cycle" ~ "1=nocturnal only, 2=mixed/other, 3=diurnal only",
            TRUE ~ NA_character_
      )) |> 
      mutate(notes = case_when(
            # raw_name == "family" ~ "family",
            # raw_name == "genus" ~ "genus",
            raw_name == "iucn2020_binomial" ~ "two columns for 'specific epithet, but can't identify difference...",
            # raw_name == "adult_mass_g" ~ "body_mass",
            # raw_name == "max_longevity_d" ~ "life_span",
            # raw_name == "maturity_d" ~ "age_maturity ",
            # raw_name == "age_first_reproduction_d" ~ "age_reproduction",
            # raw_name == "neonate_mass_g" ~ "offspring_mass",
            raw_name == "home_range_km2" ~ "also a column for biogeographical_realm + binary rows for fw vs marine vs terrestiral if of more interest",
            # raw_name == "trophic_level" ~ "trophic_level",
            # raw_name == "trophic_level" ~ "diet_category",
            # raw_name == "activity_cycle" ~ "active_time",
            TRUE ~ NA_character_
      )) |> 
      select(trait_database, data_type, source, column, raw_name, tidy_name, units,
              na_indicator, trait_values, notes)

glimpse(trait_df1)
head(trait_df1)
writexl::write_xlsx(trait_df1, '../../../../../../Downloads/combine-datakey-clean.xlsx')

# second trait database ---------------------------------------------------
unique(trait_df2$raw_name)
trait_df2 <- tibble(raw_name = names(trait2)) |> 
      mutate(column = row_number()) |> 
      select(column, raw_name) |> 
      mutate(
            trait_database = "ATraiU",
            data_type = 'trait',
            source = 'ATraiU_summary_values_2020AUG.csv',
            tidy_name = NA_character_,
            units = NA_character_,
            na_indicator = "NA",
            trait_values = NA_character_,
            notes = NA_character_
      ) |> 
      mutate(tidy_name = case_when(
            raw_name == "family" ~ "family",
            raw_name == "max.max_length_mm_fem" ~ "max_length_female",
            raw_name == "max.max_length_mm_male" ~ "max_length_male",
            raw_name == "latin_name" ~ "taxon",
            raw_name == "max.longevity_max_yrs" ~ "life_span",
            raw_name == "max.egg_size_mm" ~ "offspring_length",
            raw_name == "mn.hatch_size_mm" ~ 'offspring_length',
            raw_name == "min.maturity_min_yrs_fem" ~ "age_maturity_female",
            raw_name == "min.maturity_min_yrs_male" ~ "age_maturity_male",
            TRUE ~ NA_character_
      )) |> 
      mutate(units = case_when(
            raw_name == "family" ~ "character",
            raw_name == "max.max_length_mm_fem" ~ "mm",
            raw_name == "max.max_length_mm_male" ~ "mm",
            raw_name == "latin_name" ~ "character",
            raw_name == "max.longevity_max_yrs" ~ "yrs",
            raw_name == "max.egg_size_mm" ~ "mm",
            raw_name == "mn.hatch_size_mm" ~ 'mm',
            raw_name == "min.maturity_min_yrs_fem" ~ "yrs",
            raw_name == "min.maturity_min_yrs_male" ~ "yrs",
            TRUE ~ NA_character_
      )) |> 
      # mutate(trait_values = case_when(
            # raw_name == "family" ~ "family",
            # raw_name == "max.max_length_mm_fem" ~ "max_length_female",
            # raw_name == "max.max_length_mm_male" ~ "max_length_male",
            # raw_name == "latin_name" ~ "taxon",
            # raw_name == "max.longevity_max_yrs" ~ "life_span",
            # raw_name == "max.egg_size_mm" ~ "offspring_length",
            # raw_name == "mn.hatch_size_mm" ~ 'offspring_length',
            # raw_name == "min.maturity_min_yrs_fem" ~ "age_maturity_female",
            # raw_name == "min.maturity_min_yrs_male" ~ "age_maturity_male",
            # TRUE ~ NA_character_
      # )) |> 
      mutate(notes = case_when(
            raw_name == "family" ~ "family",
            raw_name == "max.max_length_mm_fem" ~ "I noticed there was male and female tidy names for mass, but length so I created a new column - hope that is alright!",
            raw_name == "max.max_length_mm_male" ~ "I noticed there was male and female tidy names for mass, but length so I created a new column - hope that is alright!",
            raw_name == "latin_name" ~ "taxon",
            raw_name == "max.longevity_max_yrs" ~ "life_span",
            raw_name == "max.egg_size_mm" ~ "offspring_length",
            raw_name == "mn.hatch_size_mm" ~ 'offspring_length',
            raw_name == "min.maturity_min_yrs_fem" ~ "Similar to length here, maturity was reported for both male and female so created new tidy name for column - maybe we can just average it all out?",
            raw_name == "min.maturity_min_yrs_male" ~ "Similar to length here, maturity was reported for both male and female so created new tidy name for column - maybe we can just average it all out?",
            TRUE ~ NA_character_
      )) |> 
      select(trait_database, data_type, source, column, raw_name, tidy_name, units,
             na_indicator, trait_values, notes)

glimpse(trait_df2)
head(trait_df2)
writexl::write_xlsx(trait_df2, '../../../../../../Downloads/atraiu-datakey-clean.xlsx')

all_traits <- rbind(trait_df1, trait_df2)
glimpse(all_traits)
writexl::write_xlsx(trait_df2, '../../../../../../Downloads/mwhite-datakey-traits-clean.xlsx')
