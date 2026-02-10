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

dat2 <- readr::read_csv('../Collaborative/FnxSynthBase/consumer-trait-species-imputed-taxonmic-database.csv')
glimpse(dat2)
unique(dat2$family)

dat3 <- readr::read_csv('../Collaborative/FnxSynthBase/consumer-trait-species-imputed-taxonmic-database.csv')
glimpse(dat3)
unique(dat3$family)

# Join species list to imputed DB + keep first row per species ----------
dat1.1 <- dat |>
      left_join(dat1) |>  # assumes same column name in both
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
            ),
            # MW, NL, and LE made a call on these
            active.time_category_ordinal = case_when(
                  active.time_category_ordinal == 1 ~ "nocturnal",
                  active.time_category_ordinal == 0 ~ "diurnal",
                  TRUE ~ NA_character_
            )
      ) |>
      group_by(scientific_name) |>
      slice(1) |>
      ungroup()

glimpse(traits)
unique(traits$diet_cat)
nacheck(traits)

# NA-fill possible columns -----------------------------------------
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
