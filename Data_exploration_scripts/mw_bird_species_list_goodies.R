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
                 splitstackshape, forcats)

### define custom functions ----
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

dat <- read_csv('../Collaborative/FnxSynthBase/23_species_master-spp-list.csv') |> 
      filter(class == 'Aves')
glimpse(dat)
nacheck(dat)
unique(dat$project)
# write_csv(dat, '../Collaborative/FnxSynthBase/bird_species_list.csv')

dat1 <- read_csv('../Collaborative/FnxSynthBase/12_traits_wrangled.csv')
glimpse(dat1)
dat2 <- dat1 |> select(source, family, scientific_name, genus, epithet, 
                       taxonomic.resolution, taxon, mass_adult_g, length_adult_cm, age_life.span_years,
                       reproduction_reproductive.rate_num.events.per.year, reproduction_reproductive.rate_num.litter.or.clutch.per.year,
                       diet_trophic.level_num, active.time_category_ordinal)

dat3 <- dat |> left_join(dat2)
glimpse(dat3)

dat4 <- dat3 |> 
      select(kingdom, phylum, class, order, family, genus, epithet, scientific_name,
             mass_adult_g, length_adult_cm, age_life.span_years,
             reproduction_reproductive.rate_num.events.per.year, reproduction_reproductive.rate_num.litter.or.clutch.per.year,
             diet_trophic.level_num, active.time_category_ordinal) |> 
      distinct() |> 
      group_by(kingdom, phylum, class, order, family, genus, epithet, scientific_name) |> 
      mutate(across(where(is.numeric), ~(mean(.x, na.m = TRUE)))) |> 
      ungroup() |> 
      distinct()
glimpse(dat4)

# write_csv(dat4, '../Collaborative/FnxSynthBase/bird_species_list_withtraitsmissing.csv')

traits <- read_xlsx('../Collaborative/FnxSynthBase/BirdFuncDatExcel.xlsx') |> 
      select(Scientific,`Diet-5Cat`, Nocturnal, `BodyMass-Value`) |> 
      rename(
            scientific_name = Scientific,
            diet_cat = `Diet-5Cat`,
            active.time_category_ordinal = Nocturnal,
            mass_adult_g = `BodyMass-Value`
      ) |> 
      distinct() |> 
      ### MW, NL, and LE made a call on these
      mutate(diet_trophic.level_num = case_when(
            diet_cat == "PlantSeed" ~ 1,
            diet_cat == "FruiNect" ~ 1,
            diet_cat == "Omnivore" ~ 1.5,
            diet_cat ==  "Invertebrate" ~ 2,
            diet_cat == "VertFishScav" ~ 3,
            TRUE ~ NA_real_
      )) |> 
      ### MW, NL, and LE made a call on these
      mutate(active.time_category_ordinal = case_when(
            active.time_category_ordinal == 1 ~ 'nocturnal',
            active.time_category_ordinal == 0 ~ 'diurnal'
      ))
glimpse(traits)
unique(traits$diet_cat)
glimpse(traits)

# write_csv(traits, '../Collaborative/FnxSynthBase/macktratis_prelunch_birds_goodies_forLE.csv')
comb <- dat4 |> 
      select(scientific_name) |>
      distinct() |>
      left_join(traits) |> 
      mutate(source = 'Elton Traits_Wilmanetal2016') |> 
      select(-diet_cat)
glimpse(comb)
nacheck(comb)
nacheck(comb)

all <- dat4 |> 
      select()
