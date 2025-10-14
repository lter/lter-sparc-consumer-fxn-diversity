## ---------------------------------------------------- ##
# CFD - Trait Harmonization
## ---------------------------------------------------- ##
# Purpose:
## Combine separate trait tables into a single, "harmonized" traits table

# Load libraries
librarian::shelf(tidyverse, ltertools)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

## --------------------------- ##
# Read in Keys ----
## --------------------------- ##

# Read in 'regular' trait key
trt_key_v0 <- read.csv(file = file.path("data", "00_keys", "traits_datakey.csv"))

# Make sure it's all pared down to the needed content
trt_key <- ltertools::check_key(key = trt_key_v0)

# Check structure
dplyr::glimpse(trt_key)

# Also read in the key for long-format traits
trtlong_key_v0 <- read.csv(file = file.path("data", "00_keys", "long_traits_datakey.csv"))
trtlong_key <- ltertools::check_key(key = trtlong_key_v0 )

# Check structure
dplyr::glimpse(trtlong_key)

## --------------------------- ##
# Use Data Keys ----
## --------------------------- ##

# Harmonize the 'regular' trait data
trt_v1 <- ltertools::harmonize(key = trt_key, data_format = "csv",
                               raw_folder = file.path("data", "11_traits_raw_data"))

# Check structure
dplyr::glimpse(trt_v1)

# Do the same for the long-format traits
trtlong_v1 <- ltertools::harmonize(key = trtlong_key, data_format = "csv",
                               raw_folder = file.path("data", "11_traits_raw_data"))

# Check structure
dplyr::glimpse(trtlong_v1)


# End ----
