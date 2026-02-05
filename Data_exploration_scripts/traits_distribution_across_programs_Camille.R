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

# Order and create the Active time categories:
subset_tr_df <- subset_tr_df %>% 
  dplyr::mutate(active.time_category_new = case_when(
    active.time_category_ordinal == "diurnal" ~ "diurnal",
    active.time_category_ordinal == "nocturnal" ~ "nocturnal",
    ! active.time_category_ordinal %in% c("diurnal", "nocturnal", "") ~ "cathemeral")) %>% 
  dplyr::select(-c(active.time_category_ordinal)) %>% 
  dplyr::rename(active.time_category_ordinal = active.time_category_new)

class(subset_tr_df$active.time_category_ordinal)
levels(subset_tr_df$active.time_category_ordinal)
unique(subset_tr_df$active.time_category_ordinal)
# Categorise:
subset_tr_df$active.time_category_ordinal <- factor(subset_tr_df$active.time_category_ordinal)


# ----

## Check numeric traits distribution, correct and log when needed:

# LIFESPAN:
hist(subset_tr_df$age_life.span_years, breaks = 50)

# Correct some sp have - or 0 lifespan and homosapiens is in the db (?):
# CHANGE LATER: Remove data for these species (look for it later) and rm humans:
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Hemichromis letourneuxi")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Channa marulius")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Gambusia holbrooki")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Lucania parva")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Atheriniformes")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Menidia peninsulae")] <- NA
subset_tr_df$age_life.span_years[which(subset_tr_df$species == "Fundulus")] <- NA
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
hist(subset_tr_df2$mass_adult_g, breaks = 100)

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
  Coastal_CA = "#08306B",
  SBC = "#1B9E77",
  FCE = "#00441B",
  VCR = "#40B5C4",
  MCR = "#2171B5",
  RLS  = "#6BAED6",
  FISHGLOB = "#636363",
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
  ggplot2::facet_wrap(~ taxa, ncol = 1, scale = "free_y") +
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


# 5 - Explore traits distribution - Focus on fish ==============================


# Subset data set for fish:
fish_proj_traits_df <- dplyr::filter(proj_traits_df,
                                     taxa == "Fish")

# Set up colors
cols <- c(
  Coastal_CA = "#08306B",
  SBC = "#1B9E77",
  FCE = "#00441B",
  VCR = "#40B5C4",
  MCR = "#2171B5",
  RLS  = "#6BAED6",
  FISHGLOB = "#636363")


# Body mass:
# Add the number of values:
n_df <- fish_proj_traits_df %>%
  dplyr::filter(!is.na(mass_adult_g)) %>%
  dplyr::group_by(taxa, project) %>%
  dplyr::summarise(n = n(), .groups = "drop") %>%
  dplyr::mutate(label = paste0("n = ", n))

# Get the position to place the labels:
pos_df <- fish_proj_traits_df %>%
  dplyr::filter(!is.na(mass_adult_g)) %>%
  dplyr::group_by(taxa) %>% # Have to group by taxa to get right x pos
  dplyr::summarise(x = max(mass_adult_g, na.rm = TRUE), y = Inf,
                   .groups = "drop")
label_df <- n_df %>%
  dplyr::left_join(pos_df, by = "taxa")

# Log body mass plot:
fish_body_mass_distrib <- ggplot2::ggplot(fish_proj_traits_df, 
                                     ggplot2::aes(x = mass_adult_g,
                                                  colour = project,
                                                  fill   = project)) +
  ggplot2::geom_density(alpha = 0.5, linewidth = 0.8) +
  ggplot2::facet_wrap(~ project, ncol = 2, scale = "free_y") +
  ggplot2::geom_text(data = label_df,
    ggplot2::aes(x = x, y = y, label = label, colour = project),
    hjust = 1.05, vjust = 1.2, size = 3, show.legend = FALSE) +
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
fish_body_mass_distrib


# Life span:
# Add the number of values:
n_df <- fish_proj_traits_df %>%
  dplyr::filter(!is.na(age_life.span_years)) %>%
  dplyr::group_by(taxa, project) %>%
  dplyr::summarise(n = n(), .groups = "drop") %>%
  dplyr::mutate(label = paste0("n = ", n))

# Get the position to place the labels:
pos_df <- fish_proj_traits_df %>%
  dplyr::filter(!is.na(age_life.span_years)) %>%
  dplyr::group_by(taxa) %>%
  dplyr::summarise(x = max(age_life.span_years, na.rm = TRUE), y = Inf,
                   .groups = "drop")
label_df <- n_df %>%
  dplyr::left_join(pos_df, by = "taxa")

# Log life span plot:
fish_life_span_distrib <- ggplot2::ggplot(fish_proj_traits_df, 
                                          ggplot2::aes(x = age_life.span_years,
                                                       colour = project,
                                                       fill   = project)) +
  ggplot2::geom_density(alpha = 0.5, linewidth = 0.8) +
  ggplot2::facet_wrap(~ project, ncol = 2, scale = "free_y") +
  ggplot2::geom_text(data = label_df,
                     ggplot2::aes(x = x, y = y, label = label, colour = project),
                     hjust = 1.05, vjust = 1.2, size = 3, show.legend = FALSE) +
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
fish_life_span_distrib


# Trophic Level:
# Add the number of values:
n_df <- fish_proj_traits_df %>%
  dplyr::filter(!is.na(diet_trophic.level_num)) %>%
  dplyr::group_by(taxa, project) %>%
  dplyr::summarise(n = n(), .groups = "drop") %>%
  dplyr::mutate(label = paste0("n = ", n))

# Get the position to place the labels:
pos_df <- fish_proj_traits_df %>%
  dplyr::filter(!is.na(diet_trophic.level_num)) %>%
  dplyr::group_by(taxa) %>%
  dplyr::summarise(x = max(diet_trophic.level_num, na.rm = TRUE), y = Inf,
                   .groups = "drop")
label_df <- n_df %>%
  dplyr::left_join(pos_df, by = "taxa")

# Trophic Level plot:
fish_trophic_level_distrib <- ggplot2::ggplot(fish_proj_traits_df, 
                                              ggplot2::aes(x = diet_trophic.level_num,
                                                           colour = project,
                                                           fill   = project)) +
  ggplot2::geom_density(alpha = 0.5, linewidth = 0.8) +
  ggplot2::facet_wrap(~ project, ncol = 2, scale = "free_y") +
  ggplot2::geom_text(data = label_df,
                     ggplot2::aes(x = x, y = y, label = label, colour = project),
                     hjust = 1.05, vjust = 1.2, size = 3, show.legend = FALSE) +
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
fish_trophic_level_distrib


# Reproductive Rate:
# Add the number of values:
n_df <- fish_proj_traits_df %>%
  dplyr::filter(!is.na(reproduction_reproductive.rate_num.offspring.per.year)) %>%
  dplyr::group_by(taxa, project) %>%
  dplyr::summarise(n = n(), .groups = "drop") %>%
  dplyr::mutate(label = paste0("n = ", n))

# Get the position to place the labels:
pos_df <- fish_proj_traits_df %>%
  dplyr::filter(!is.na(reproduction_reproductive.rate_num.offspring.per.year)) %>%
  dplyr::group_by(taxa) %>%
  dplyr::summarise(x = max(reproduction_reproductive.rate_num.offspring.per.year, na.rm = TRUE), y = Inf,
                   .groups = "drop")
label_df <- n_df %>%
  dplyr::left_join(pos_df, by = "taxa")

#  Reproductive Rate plot:
fish_reprod_rate_distrib <- ggplot2::ggplot(fish_proj_traits_df, 
                                            ggplot2::aes(x = reproduction_reproductive.rate_num.offspring.per.year,
                                                         colour = project,
                                                         fill   = project)) +
  ggplot2::geom_density(alpha = 0.5, linewidth = 0.8) +
  ggplot2::facet_wrap(~ project, ncol = 2, scale = "free_y") +
  ggplot2::geom_text(data = label_df,
                     ggplot2::aes(x = x, y = y, label = label, colour = project),
                     hjust = 1.05, vjust = 1.2, size = 3, show.legend = FALSE) +
  ggplot2::scale_colour_manual(values = cols, guide = "none") +
  ggplot2::scale_fill_manual(values = cols) +
  ggplot2::labs(
    x = "Reproduction rate (log)",
    y = "Density",
    fill = "Project") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "grey90"),
    panel.grid.minor = ggplot2::element_blank())
fish_reprod_rate_distrib


# Active time:
# Add the number of values:
n_df <- fish_proj_traits_df %>%
  dplyr::filter(!is.na(active.time_category_ordinal)) %>%
  dplyr::group_by(taxa, project) %>%
  dplyr::summarise(n = n(), .groups = "drop") %>%
  dplyr::mutate(label = paste0("n = ", n))

# Get the position to place the labels:
pos_df <- proj_traits_df %>%
  dplyr::filter(!is.na(active.time_category_ordinal)) %>%
  dplyr::group_by(taxa) %>%
  dplyr::summarise(x = max(as.numeric(factor(active.time_category_ordinal))),
                   y = 1,.groups = "drop")
label_df <- n_df %>%
  dplyr::left_join(pos_df, by = "taxa")

# Subset the proportion df:
fish_prop_df <- dplyr::filter(prop_df,
                              taxa == "Fish")

# Active period distrib:
fish_act_period_distrib <- ggplot2::ggplot(fish_prop_df,
                                      ggplot2::aes(x = active.time_category_ordinal,
                                                   y = prop,
                                                   group = project,
                                                   colour = project)) +
  ggplot2::geom_line(alpha = 0.8, linewidth = 0.9) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_text(data = label_df,
                     ggplot2::aes(x = x, y = y, label = label, colour = project),
                     hjust = 1.05, vjust = 1.2, size = 3, show.legend = FALSE) +
  ggplot2::facet_wrap(~ project, ncol = 2) +
  ggplot2::scale_colour_manual(values = cols, guide = "none") +
  ggplot2::labs(
    x = "Activity time category",
    y = "Proportion of species") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(angle = 45,
                                                     hjust = 1,
                                                     vjust = 1,
                                                     size = 8))
fish_act_period_distrib




# After:
# For the functional space, remove reproductive rate (low coverage):