# Script Details ----------------------------------------------------------
### project: LTER SPARC Consumer Functional Diversity
### author(s): MW
### goal(s): generate trait key for Aves + NA-fill from BirdFuncDatExcel
### date(s): October 2025

# Housekeeping ------------------------------------------------------------
# install.packages("librarian")
librarian::shelf(
      tidyverse, readxl, scales, broom, purrr, dataRetrieval,
      splitstackshape, forcats
)

# Helper ------------------------------------------------------------------
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

# Read species list (Aves) ---------------------------------------------
dat <- readr::read_csv('../Collaborative/FnxSynthBase/23_species_master-spp-list.csv') |>
      filter(class == "Aves")

glimpse(dat)
nacheck(dat)
unique(dat$project)

# Read imputed trait database ------------------------------------------
dat1 <- readr::read_csv('../Collaborative/FnxSynthBase/consumer-trait-species-imputed-taxonmic-database.csv')
glimpse(dat1)
unique(dat1$family)

# Join species list to imputed DB + keep first row per species ----------
dat1.1 <- dat |>
      left_join(dat1) |>  
      dplyr::select(
            source, family, scientific_name, genus,
            mass_adult_g, length_adult_cm, age_life.span_years,
            reproduction_reproductive.rate_num.events.per.year,
            reproduction_reproductive.rate_num.litter.or.clutch.per.year,
            diet_trophic.level_num, active.time_category_ordinal
      ) |>
      group_by(scientific_name) |>
      slice(1) |>
      ungroup()

glimpse(dat1.1)
nacheck(dat1.1)

# Read BirdFuncDatExcel traits + recode + keep first row per species ----
traits <- readxl::read_xlsx('../Collaborative/FnxSynthBase/BirdFuncDatExcel.xlsx') |>
      select(Scientific, `Diet-5Cat`, Nocturnal, `BodyMass-Value`) |>
      rename(
            scientific_name = Scientific,
            diet_cat = `Diet-5Cat`,
            active.time_category_ordinal = Nocturnal,
            mass_adult_g = `BodyMass-Value`
      ) |>
      mutate(
            # MW, NL, and LE made a call on these
            diet_trophic.level_num = case_when(
                  diet_cat == "PlantSeed"    ~ 1,
                  diet_cat == "FruiNect"     ~ 1,
                  diet_cat == "Omnivore"     ~ 1.5,
                  diet_cat == "Invertebrate" ~ 2,
                  diet_cat == "VertFishScav" ~ 3,
                  TRUE ~ NA_real_
            )
      ) |>
      dplyr::select(scientific_name, diet_trophic.level_num, mass_adult_g) |> 
      mutate(notes = 'see mw_bird_species_list_goodies for preclean')
glimpse(traits)
nacheck(traits)
write_csv(traits, '../Collaborative/FnxSynthBase/elton_traits_preclean.csv')

# Read next one ----
traits1 <- readxl::read_xlsx('../Collaborative/FnxSynthBase/birdbase_clean.xlsx') |>
      rename(
            common_name      = `English Name (BirdLife > IOC > Clements>AviList)`,
            scientific_name  = `Latin (BirdLife > IOC > Clements>AviList)`,
            female_min_mass  = `Female MinMass`,
            female_max_mass  = `Female MaxMass`,
            male_min_mass    = `Male MinMass`,
            male_max_mass    = `Male MaxMass`,
            primary_habitat  = `Primary Habitat`,
            primary_diet     = `Primary Diet`,
            clutch_min       = Clutch_Min,
            clutch_max       = Clutch_Max
      ) |>
      rowwise() |>
      mutate(
            mass_g = mean(
                  c_across(c(female_min_mass, female_max_mass,
                             male_min_mass,   male_max_mass)),
                  # na.rm = TRUE
            ),
            clutch_num = mean(
                  c_across(c(clutch_min, clutch_max)),
                  # na.rm = TRUE
            )
      ) |>
      ungroup() |>
      select(common_name, scientific_name, primary_habitat, 
             primary_diet, mass_g, clutch_num) |> 
      mutate(
            across(
                  where(is.numeric),
                  ~ ifelse(is.nan(.x), NA_real_, .x)
            )
      ) |> 
      mutate(
            trophic_level_num = case_when(
                  primary_diet %in% c("Plant", "Herbivore", "Seed", "Fruit", "Nectar", "Beeswax") ~ 1,
                  primary_diet %in% c("Omnivore") ~ 1.5,
                  primary_diet %in% c("Invertebrate", "Ovivore") ~ 2,
                  primary_diet %in% c("Fish", "Vertebrate", "Carnivore") ~ 3,
                  primary_diet == "No Information" ~ NA_real_,
                  TRUE ~ NA_real_
            )
      ) |> 
      rename(
            reproduction_reproductive.rate_num.offspring.per.clutch.or.litter = clutch_num,
            diet_trophic.level_num = trophic_level_num,
            mass_adult_g = mass_g
             ) |> 
      mutate(notes = 'see mw_bird_species_list_goodies for preclean') |> 
      dplyr::select(common_name, scientific_name, mass_adult_g, reproduction_reproductive.rate_num.offspring.per.clutch.or.litter,
                    diet_trophic.level_num, notes)
glimpse(traits1)      
nacheck(traits1)
write_csv(traits1, '../Collaborative/FnxSynthBase/birdbase_traits_preclean.csv')

# Read last one - number three ----
traits2 <- readr::read_csv('../Collaborative/FnxSynthBase/traits6_v2.csv') |>
      dplyr::select(name, eng_name, fc_categ, bmass, egglow, eggupp) |>
      rename(
            common_name      = eng_name,
            scientific_name  = name,
            mass_g           = bmass,
            primary_diet     = fc_categ,
            clutch_min       = egglow,
            clutch_max       = eggupp
      ) |>
      rowwise() |>
      mutate(
            clutch_num = mean(
                  c_across(c(clutch_min, clutch_max)),
                  # na.rm = TRUE
            )
      ) |>
      ungroup() |> 
      mutate(
            # MW, NL, and LE made a call on these
            trophic_level_num = case_when(
                  primary_diet == "PlantSeed"    ~ 1,
                  primary_diet == "FruiNect"     ~ 1,
                  primary_diet == "Omnivore"     ~ 1.5,
                  primary_diet == "Invertebrate" ~ 2,
                  primary_diet == "VertFishScav" ~ 3,
                  TRUE ~ NA_real_
            )
      ) |> 
      rename(
            reproduction_reproductive.rate_num.offspring.per.clutch.or.litter = clutch_num,
            diet_trophic.level_num = trophic_level_num,
            mass_adult_g = mass_g
      ) |> 
      mutate(notes = 'see mw_bird_species_list_goodies for preclean') |> 
      dplyr::select(common_name, scientific_name, mass_adult_g, reproduction_reproductive.rate_num.offspring.per.clutch.or.litter,
                    diet_trophic.level_num, notes)
glimpse(traits2)
nacheck(traits2)

write_csv(traits2, '../Collaborative/FnxSynthBase/oleksii2024_preclean.csv')

# NA-fill possible columns for traits -----------------------------------------
cols_to_fill <- c("mass_adult_g", "diet_trophic.level_num", "active.time_category_ordinal")
?coalesce()
comb <- dat1.1 |> 
      left_join(traits, by = "scientific_name", suffix = c("", "_traits")) %>%
      mutate(
            mass_adult_g = coalesce(mass_adult_g, mass_adult_g_traits),
            diet_trophic.level_num = coalesce(diet_trophic.level_num, diet_trophic.level_num_traits),
            active.time_category_ordinal = coalesce(active.time_category_ordinal, active.time_category_ordinal_traits)
      ) |> 
      select(
            -mass_adult_g_traits,
            -diet_trophic.level_num_traits,
            -active.time_category_ordinal_traits,
            -diet_cat
      )
nacheck(dat1.1)
nacheck(comb)

tibble(
      column = cols_to_fill,
      filled = purrr::map_int(cols_to_fill, ~ sum(is.na(dat1.1[[.x]]) & !is.na(comb[[.x]]))),
      remaining_NA = purrr::map_int(cols_to_fill, ~ sum(is.na(comb[[.x]])))
) |> print()

# NA-fill possible columns for traits1 -----------------------------------------
cols_to_fill <- c("mass_adult_g", "diet_trophic.level_num", "active.time_category_ordinal")
?coalesce()
comb <- dat1.1 |> 
      left_join(traits, by = "scientific_name", suffix = c("", "_traits")) %>%
      mutate(
            mass_adult_g = coalesce(mass_adult_g, mass_adult_g_traits),
            diet_trophic.level_num = coalesce(diet_trophic.level_num, diet_trophic.level_num_traits),
            active.time_category_ordinal = coalesce(active.time_category_ordinal, active.time_category_ordinal_traits)
      ) |> 
      select(
            -mass_adult_g_traits,
            -diet_trophic.level_num_traits,
            -active.time_category_ordinal_traits,
            -diet_cat
      )
nacheck(dat1.1)
nacheck(comb)

tibble(
      column = cols_to_fill,
      filled = purrr::map_int(cols_to_fill, ~ sum(is.na(dat1.1[[.x]]) & !is.na(comb[[.x]]))),
      remaining_NA = purrr::map_int(cols_to_fill, ~ sum(is.na(comb[[.x]])))
) |> print()

# NA-fill possible columns for traits2 -----------------------------------------
cols_to_fill <- c("mass_adult_g", "diet_trophic.level_num", "active.time_category_ordinal")
?coalesce()
comb <- dat1.1 |> 
      left_join(traits, by = "scientific_name", suffix = c("", "_traits")) %>%
      mutate(
            mass_adult_g = coalesce(mass_adult_g, mass_adult_g_traits),
            diet_trophic.level_num = coalesce(diet_trophic.level_num, diet_trophic.level_num_traits),
            active.time_category_ordinal = coalesce(active.time_category_ordinal, active.time_category_ordinal_traits)
      ) |> 
      select(
            -mass_adult_g_traits,
            -diet_trophic.level_num_traits,
            -active.time_category_ordinal_traits,
            -diet_cat
      )
nacheck(dat1.1)
nacheck(comb)

tibble(
      column = cols_to_fill,
      filled = purrr::map_int(cols_to_fill, ~ sum(is.na(dat1.1[[.x]]) & !is.na(comb[[.x]]))),
      remaining_NA = purrr::map_int(cols_to_fill, ~ sum(is.na(comb[[.x]])))
) |> print()