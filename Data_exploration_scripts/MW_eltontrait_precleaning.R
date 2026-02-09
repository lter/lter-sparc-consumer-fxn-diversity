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
      ungroup() |> 
      mutate(notes = 'ME,NL,LE coded activity time and trophic level') |> 
      filter(!is.na(scientific_name))
glimpse(traits)
unique(traits$diet_cat)
nacheck(traits)

# NA-fill possible columns -----------------------------------------
cols_to_fill <- c("mass_adult_g", "diet_trophic.level_num", "active.time_category_ordinal")
?coalesce()
comb <- dat1.2 |> 
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
nacheck(dat1.2)
nacheck(comb)

tibble(
      column = cols_to_fill,
      filled = purrr::map_int(cols_to_fill, ~ sum(is.na(dat1.2[[.x]]) & !is.na(comb[[.x]]))),
      remaining_NA = purrr::map_int(cols_to_fill, ~ sum(is.na(comb[[.x]])))
) |> print()
