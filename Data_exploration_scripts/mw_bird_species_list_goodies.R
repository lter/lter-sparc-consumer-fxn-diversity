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
write_csv(dat, '../Collaborative/FnxSynthBase/bird_species_list.csv')

dat1 <- read_csv('../Collaborative/FnxSynthBase/12_traits_wrangled.csv')

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

write_csv(dat4, '../Collaborative/FnxSynthBase/bird_species_list_withtraitsmissing.csv')
