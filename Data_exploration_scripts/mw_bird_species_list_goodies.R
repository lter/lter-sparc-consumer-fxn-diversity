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
