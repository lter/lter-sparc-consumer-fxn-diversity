###########################################################################
# Script Information ------------------------------------------------------
###########################################################################

### project: LTER SPARC Consumer FXN Diversity
### author(s): MW
### goal: vizualizing some of the new datasets for Consumer WG

# Housekeeping ------------------------------------------------------------

### load necessary packages -----
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, readr, ggpubr, corrplot, ggeffects)

# read in necessary data and wrangle/summarize ----------------------------

arc <- read_csv('Data_exploration_scripts/localdata/Toolik_Zoop.csv')
ntl <- read_csv('Data_exploration_scripts/localdata/NorthTemperateLakes_Zoop.csv')
pal <- read_csv('Data_exploration_scripts/localdata/Palmer_Zoop.csv')
