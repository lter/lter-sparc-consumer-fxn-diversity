################################################################################
##
## Script to explore traits distribution across taxa and programs for the
## in-person meeting
## Taxa: Fish, Mammals, Amphibians
## Traits: MAss, Life Span, Trophic Level (num), Reproduction Rate, Active Time
##
## Camille Magneville
##
## 02/2026
##
################################################################################


# 0 - Set up ===================================================================

# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`

# Load libraries
librarian::shelf(tidyverse, dplyr, funbiogeo, ggplot2)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()


# 1 - Load data ================================================================


# Load master species list for all programs:
sp_pro_list <- read.csv(file.path("Data", "species_tidy-data", "23_species_master-spp-list.csv"))

# Load imputed traits:
imp_tr_df <- read.csv(file.path("Data", "traits_tidy-data", "consumer-trait-species-imputed-taxonmic-database.csv"))


# 2 - Check traits and clean ===================================================


# ----

# Subset to the studied traits and taxa:
subset_tr_df <- imp_tr_df %>% 
  dplyr::select(c("scientific_name",
                  "age_life.span_years",
                  "diet_trophic.level_num",
                  "reproduction_reproductive.rate_num.offspring.per.year",
                  "mass_adult_g",
                  "active.time_category_ordinal")) %>% 
  dplyr::rename(species = "scientific_name")

# Check traits cover:
funbiogeo::fb_plot_number_species_by_trait(subset_tr_df)
funbiogeo::fb_plot_number_traits_by_species(subset_tr_df)


# ----

# Check traits type:
class(subset_tr_df$age_life.span_years)
class(subset_tr_df$mass_adult_g)
class(subset_tr_df$diet_trophic.level_num)

# Order the Active time categories:
class(subset_tr_df$active.time_category_ordinal)
levels(subset_tr_df$active.time_category_ordinal)
unique(subset_tr_df$active.time_category_ordinal)
# Correct "" to NA:
subset_tr_df$active.time_category_ordinal[which(subset_tr_df$active.time_category_ordinal == "")] <- NA
unique(subset_tr_df$active.time_category_ordinal)
# Categorise:
subset_tr_df$active.time_category_ordinal <- as.factor(subset_tr_df$active.time_category_ordinal)


# ----

## Check numeric traits distribution, correct and log when needed:

# LIFESPAN:
hist(subset_tr_df$age_life.span_years)

# Correct some sp have - or 0 lifespan and homosapiens is in the db (?):
# CHANGE LATER: Remove data for these species (look for it later) and rm humans:
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Hemichromis letourneuxi")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Channa marulius")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Gambusia holbrooki")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Lucania parva")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Chrysiptera brownriggii")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Chrysiptera glauca")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Chrysiptera notialis")] <- NA
subset_tr_df <- subset_tr_df[which(subset_tr_df$species != "Homo sapiens"), ]
hist(subset_tr_df$age_life.span_years, breaks = 100)

# Log transform:
subset_tr_df2 <- subset_tr_df
subset_tr_df2$age_life.span_years <- log10(subset_tr_df2$age_life.span_years +1)
hist(subset_tr_df2$age_life.span_years, breaks = 50)


# MASS:
hist(subset_tr_df2$mass_adult_g, breaks = 50)

# Log transform:
subset_tr_df2$mass_adult_g <- log10(subset_tr_df2$mass_adult_g +1)
hist(subset_tr_df2$mass_adult_g, breaks = 50)


# TROPHIC LEVELS: keep it as it is
hist(subset_tr_df2$diet_trophic.level_num, breaks = 50)


# REPRODCTION RATE:
hist(subset_tr_df2$reproduction_reproductive.rate_num.offspring.per.year, breaks = 50)

# Log transform:
subset_tr_df2$reproduction_reproductive.rate_num.offspring.per.year <- log10(subset_tr_df2$reproduction_reproductive.rate_num.offspring.per.year +1)
hist(subset_tr_df2$reproduction_reproductive.rate_num.offspring.per.year, breaks = 50)


# 3 - Link traits to program and only keep fish/mammals/amphib =================


# Keep only programs for fish, mammals and amphibians:
subset_sp_list <- sp_pro_list %>% 
  dplyr::filter(project %in% c("CoastalCA", "FCE", "SBC", "MCR", "VCR", "RLS",
                               "FISHGLOB", "KBS_MAM", "SEV", 
                               "MOHONK", "KBS_AMP")) %>% 
  dplyr::mutate(project = factor(project))

# Create a new column for Taxa:
subset_sp_list <- subset_sp_list %>% 
  mutate(taxa = case_when(
    project == "CoastalCA" ~ "Fish",
    project == "FCE" ~ "Fish",
    project == "SBC" ~ "Fish",
    project == "MCR" ~ "Fish",
    project == "VCR" ~ "Fish",
    project == "RLS" ~ "Fish",
    project == "FISHGLOB" ~ "Fish",
    project == "KBS_MAM" ~ "Mammals",
    project == "SEV" ~ "Mammals",
    project == "MOHONK" ~ "Amphibians",
    project == "KBS_AMP" ~ "Amphibians")) %>% 
  dplyr::mutate(Taxa = factor(taxa))

# Reorder and select columns:
sp_list_final <- subset_sp_list %>% 
  dplyr::select(c("project", "habitat", "scientific_name",
                  "taxa")) %>% 
  dplyr::rename(species = "scientific_name")

# Link species list (projects) with traits:
proj_traits_df <- dplyr::left_join(sp_list_final, 
                                   subset_tr_df2,
                                   by = "species")


# 4 - Explore traits distribution across programs and taxa ======================


# Define the order of projects:
projects_levels <- c("CoastalCA", "SBC", 
                    "FCE", 
                    "VCR",
                    "MCR", "RLS", 
                    "FISHGLOB",
                    "SEV",
                    "KBS_MAM",
                    "MOHONK",
                    "KBS_AMP")
proj_traits_df$project <- factor(proj_traits_df$project,
                                 levels = projects_levels)

# Define colors:
cols <- c(
  Coastal_CA = "#0B3C49",
  SBC = "#2F8F83",
  FCE = "#3A6F4D",
  VCR = "#66C2A5",
  MCR = "#1F78B4",
  RLS  = "#33A1C9",
  FISHGLOB = "#4C6A92",
  SEV = "#C9A227",
  KBS_MAM = "#8B6B3E",
  MOHONK = "#E78AC3",
  KBS_AMP = "#C51B7D")


# Log body mass:
body_mass_distrib <- ggplot2::ggplot(proj_traits_df, 
                                     ggplot2::aes(x = mass_adult_g,
                                                  colour = project,
                                                  fill   = project)) +
  ggplot2::geom_density(alpha = 0.5, linewidth = 0.8) +
  ggplot2::facet_wrap(~ taxa, ncol = 1) +
  ggplot2::scale_colour_manual(values = cols, guide = "none") +
  ggplot2::scale_fill_manual(values = cols) +
  ggplot2::labs(
    x = "Adult body mass (log)",
    y = "Density",
    fill = "Project") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "grey90"),
    panel.grid.minor = ggplot2::element_blank())
body_mass_distrib


# Log life span:
life_span_distrib <- ggplot2::ggplot(proj_traits_df, 
                                     ggplot2::aes(x = age_life.span_years,
                                                  colour = project,
                                                  fill   = project)) +
  ggplot2::geom_density(alpha = 0.5, linewidth = 0.8) +
  ggplot2::facet_wrap(~ taxa, ncol = 1) +
  ggplot2::scale_colour_manual(values = cols, guide = "none") +
  ggplot2::scale_fill_manual(values = cols) +
  ggplot2::labs(
    x = "Life Span (log)",
    y = "Density",
    fill = "Project") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "grey90"),
    panel.grid.minor = ggplot2::element_blank())
life_span_distrib

# Trophic Level:
trophic_level_distrib <- ggplot2::ggplot(proj_traits_df, 
                                     ggplot2::aes(x = diet_trophic.level_num,
                                                  colour = project,
                                                  fill   = project)) +
  ggplot2::geom_density(alpha = 0.5, linewidth = 0.8) +
  ggplot2::facet_wrap(~ taxa, ncol = 1) +
  ggplot2::scale_colour_manual(values = cols, guide = "none") +
  ggplot2::scale_fill_manual(values = cols) +
  ggplot2::labs(
    x = "Trophic Level",
    y = "Density",
    fill = "Project") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "grey90"),
    panel.grid.minor = ggplot2::element_blank())
trophic_level_distrib


# Reproduction Rate:
reprod_rate_distrib <- ggplot2::ggplot(proj_traits_df, 
                                         ggplot2::aes(x = reproduction_reproductive.rate_num.offspring.per.year,
                                                      colour = project,
                                                      fill   = project)) +
  ggplot2::geom_density(alpha = 0.5, linewidth = 0.8) +
  ggplot2::facet_wrap(~ taxa, ncol = 1) +
  ggplot2::scale_colour_manual(values = cols, guide = "none") +
  ggplot2::scale_fill_manual(values = cols) +
  ggplot2::labs(
    x = "Reproduction Rate (log)",
    y = "Density",
    fill = "Project") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "grey90"),
    panel.grid.minor = ggplot2::element_blank())
reprod_rate_distrib


# Activity period (as categorical compute prop of sp having a given cat):
prop_df <- proj_traits_df %>%
  dplyr::filter(!is.na(active.time_category_ordinal)) %>%
  dplyr::group_by(taxa, project, active.time_category_ordinal) %>%
  dplyr::summarise(n = n_distinct(species), .groups = "drop") %>%
  dplyr::group_by(taxa, project) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup()
prop_df <- prop_df %>%
  dplyr::mutate(active.time_category_ordinal =
           factor(active.time_category_ordinal,
                  ordered = TRUE))

act_period_distrib <- ggplot2::ggplot(prop_df,
                                 ggplot2::aes(x = active.time_category_ordinal,
                                              y = prop,
                                              group = project,
                                              colour = project)) +
  ggplot2::geom_line(alpha = 0.8, linewidth = 0.9) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ taxa, ncol = 1) +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::labs(
    x = "Activity time category",
    y = "Proportion of species") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(angle = 45,
                                                     hjust = 1,
                                                     vjust = 1))
act_period_distrib



# After:
# For the functional space, remove reproductive rate (low coverage):

# 4 - Explore traits distribution - Focus on fish ==============================
