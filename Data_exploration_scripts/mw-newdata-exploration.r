###########################################################################
# Script Information ------------------------------------------------------
###########################################################################

### project: LTER SPARC Consumer FXN Diversity
### author(s): MW
### goal: vizualizing some of the new datasets for Consumer WG

# Housekeeping ------------------------------------------------------------

### load necessary packages -----
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, readr, ggpubr, corrplot, ggeffects, janitor,
                 lubridate)

# read in necessary data and wrangle/summarize ----------------------------

arc <- read_csv('Data_exploration_scripts/localdata/Toolik_Zoop.csv') |> 
      janitor::clean_names()
ntl <- read_csv('Data_exploration_scripts/localdata/NorthTemperateLakes_Zoop.csv') |> 
      janitor::clean_names()
pal <- read_csv('Data_exploration_scripts/localdata/Palmer_Zoop.csv') |> 
      janitor::clean_names()

# ARC LTER Summary --------------------------------------------------------
glimpse(arc)
arc_summary <- arc |>
      mutate(year = year(date), month = month(date)) |>
      summarize(
            n_sites = n_distinct(site),
            tow_depth = mean(depth_of_tow),
            min_year = min(year),
            max_year = max(year),
            n_months = n_distinct(month)
      )
print(arc_summary)

month_uniq <- arc |>
      mutate(year = year(date), month = month(date)) |>

      group_by(month) |> 
      summarize(samp = n())
print(month_uniq)

# NTL LTER Summary --------------------------------------------------------
glimpse(ntl)
ntl_summary <- ntl |>
      mutate(year = year(sample_date), month = month(sample_date),
             lakesite = paste(lakeid, station, sep = "_")) |>
      summarize(
            n_lakes = n_distinct(lakeid),
            n_sites = n_distinct(station),
            n_lakesites = n_distinct(lakesite),
            n_species = n_distinct(species_code),
            min_year = min(year),
            max_year = max(year),
            n_months = n_distinct(month)
      )
print(ntl_summary)

month_uniq <- ntl |>
      mutate(year = year(sample_date), month = month(sample_date)) |>
      group_by(month) |> 
      summarize(samp = n())
print(month_uniq)

# PAL LTER Summary --------------------------------------------------------
glimpse(pal)
pal_summary <- pal |>
      mutate(year = year(date), month = month(date),
             site = paste(grid_line, grid_station, sep = "_")) |>
      summarize(
            n_sites = n_distinct(site),
            # n_species = n_distinct(species_code),
            min_year = min(year),
            max_year = max(year),
            n_months = n_distinct(month)
      )
print(pal_summary)

n_species <- (110-26)/2
print(n_species)

month_uniq <- pal |>
      mutate(year = year(date), month = month(date)) |>
      group_by(month) |> 
      summarize(samp = n())
print(month_uniq)

station_uniq <- pal |> 
      mutate(year = year(date), month = month(date)) |>
      group_by(grid_line) |> 
      summarize(samp = n())
