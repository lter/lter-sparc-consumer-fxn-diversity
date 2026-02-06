################################################################################
##
## Script to explore functional spaces across taxa and programs
## in-person meeting
## Taxa: Fish, Mammals, Amphibians
## Traits: Mass, Life Span, Trophic Level (num), Active Time
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
librarian::shelf(tidyverse, dplyr, funbiogeo, ggplot2, mFD, tibble)

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
subset_tr_df$active.time_category_ordinal <- as.factor(subset_tr_df$active.time_category_ordinal)


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


# 4 - Explore traits and assemblages ===========================================


# Reduce the project trait dataframe to species with tr data complete:
proj_traits_noNA_df <- proj_traits_df %>% 
  tidyr::drop_na()

# Build an asb df: projects vs species: 793 species
asb_sp_df <- proj_traits_noNA_df %>%
  dplyr::select(c("project", "species")) %>% 
  dplyr::mutate(value = 1) %>%
  dplyr::distinct() %>% 
  tidyr::pivot_wider(names_from  = species, values_from = value, 
                     values_fill = 0) %>% 
  tibble::column_to_rownames(var = "project")

# Only keep species with all traits and in studied asb in the tr df: 
sp_tr_df <- proj_traits_noNA_df %>% 
  dplyr::select(colnames(subset_tr_df2)) %>% 
  dplyr::distinct() %>% 
  tibble::column_to_rownames(var = "species")
sp_tr_df$active.time_category_ordinal <- as.factor(sp_tr_df$active.time_category_ordinal)

# Check that species are the same in the asb df and in the tr df:
setdiff(rownames(sp_tr_df), colnames(asb_sp_df))
setdiff(colnames(asb_sp_df), rownames(sp_tr_df))

# Build a dataframe gathering traits categories:
tr_nm <- colnames(sp_tr_df)
tr_cat <- c("Q", "Q", "Q", "N")
tr_cat_df <- as.data.frame(matrix(ncol = 2, nrow = 4))
tr_cat_df[, 1] <- tr_nm
tr_cat_df[, 2] <- tr_cat
colnames(tr_cat_df) <- c("trait_name", "trait_type")


# Explore traits repartition:
traits_summ <- mFD::sp.tr.summary(
  tr_cat     = tr_cat_df,   
  sp_tr      = sp_tr_df, 
  stop_if_NA = TRUE)
traits_summ$tr_summary_list

# Explore projects:
asb_sp_matrix <- as.matrix(asb_sp_df)
asb_sp_summ <- mFD::asb.sp.summary(asb_sp_w = asb_sp_matrix)
asb_sp_summ$asb_sp_richn

# Look at continuous traits correlations:
corr_df <- sp_tr_df %>% 
  tibble::rownames_to_column(var = "species") %>% 
  dplyr::left_join(proj_traits_noNA_df[, c(3, 4)], by = "species") %>% 
  dplyr::distinct()
GGally::ggpairs(corr_df[, c(2:4)], aes(color = corr_df$taxa, 
                                        alpha = 0.5))


# 5 - Build functional space for all species ===================================


# Compute functional distance:
sp_dist_all <- mFD::funct.dist(
  sp_tr         = sp_tr_df,
  tr_cat        = tr_cat_df,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)
dist_df <- mFD::dist.to.df(list("df" = sp_dist_all))

# Build functional space - check quality dimensions:
fspaces_quality_all <- mFD::quality.fspaces(
  sp_dist             = sp_dist_all,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality_all,
  quality_metric             = "mad",
  fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d", 
                                 "pcoa_4d"))

# Get species coordinates:
sp_faxes_coord_all <- fspaces_quality_all$"details_fspaces"$"sp_pc_coord"


# Get link between axes and traits:
all_tr_faxes <- mFD::traits.faxes.cor(
  sp_tr          = sp_tr_df, 
  sp_faxes_coord = sp_faxes_coord_all[ , c("PC1", "PC2", "PC3", "PC4")], 
  plot           = TRUE)
all_tr_faxes

# Plot functional space:
fctsp_all <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_all[ , c("PC1", "PC2", "PC3", "PC4")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "#53868B",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "#EEAD0E",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "#EEAD0E",
  fill_vert       = "#EEAD0E",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)
fctsp_all


# -----

## Do functional space, highligthing activity convex hull:
# Compute the range of functional axes:
range_sp_coord  <- range(sp_faxes_coord_all)

# Based on the range of species coordinates values, compute a nice range ...
# ... for functional axes:
range_faxes <- range_sp_coord +
  c(-1, 1) * (range_sp_coord[2] - range_sp_coord[1]) * 0.05
range_faxes


# get species coordinates along the two studied axes:
sp_faxes_coord_xy <- sp_faxes_coord_all[, c("PC1", "PC2")]

# Plot background with grey backrgound:
plot_k <- mFD::background.plot(range_faxes = range_faxes,
                               faxes_nm = c("PC1", "PC2"),
                               color_bg = "grey98")
plot_k


# Retrieve vertices coordinates along the two studied functional axes:
vert <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_xy,
                      order_2D = FALSE,
                      check_input = TRUE)

plot_k <- mFD::pool.plot(ggplot_bg = plot_k,
                         sp_coord2D = sp_faxes_coord_xy,
                         vertices_nD = vert,
                         plot_pool = FALSE,
                         color_pool = NA,
                         fill_pool = NA,
                         alpha_ch =  0.8,
                         color_ch = "white",
                         fill_ch = "white",
                         shape_pool = NA,
                         size_pool = NA,
                         shape_vert = NA,
                         size_vert = NA,
                         color_vert = NA,
                         fill_vert = NA)
plot_k


# Create a dataframe with diurnal/others instead of programs:
asb_sp_act_df <- proj_traits_noNA_df %>%
  dplyr::select(c("species", "active.time_category_ordinal")) %>% 
  dplyr::mutate(value = 1) %>%
  dplyr::distinct() %>% 
  tidyr::pivot_wider(names_from  = species, values_from = value, 
                     values_fill = 0) %>% 
  tibble::column_to_rownames(var = "active.time_category_ordinal")

# Diurnal:
## filter diurnal species:
sp_filter_diurnal <- mFD::sp.filter(asb_nm = c("diurnal"),
                                sp_faxes_coord = sp_faxes_coord_xy,
                                asb_sp_w = asb_sp_act_df)
## get species coordinates:
sp_faxes_coord_diurnal <- sp_filter_diurnal$`species coordinates`

# Nocturnal:
## filter other species:
sp_filter_nocturnal <- mFD::sp.filter(asb_nm = c("nocturnal"),
                                    sp_faxes_coord = sp_faxes_coord_xy,
                                    asb_sp_w = asb_sp_act_df)
## get species coordinates:
sp_faxes_coord_nocturnal <- sp_filter_nocturnal$`species coordinates`

# Cathemeral:
## filter other species:
sp_filter_cath <- mFD::sp.filter(asb_nm = c("cathemeral"),
                                      sp_faxes_coord = sp_faxes_coord_xy,
                                      asb_sp_w = asb_sp_act_df)
## get species coordinates:
sp_faxes_coord_cath <- sp_filter_cath$`species coordinates`


# Retrieve names of sp being vertices:
# Diurnal:
vert_nm_diurnal <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_diurnal,
                             order_2D = TRUE,
                             check_input = TRUE)
# Noct:
vert_nm_nocturnal <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_nocturnal,
                              order_2D = TRUE,
                              check_input = TRUE)
# Cath:
vert_nm_cath <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_cath,
                                   order_2D = TRUE,
                                   check_input = TRUE)


# Add the convex hulls:
fctsp_hull_act <- mFD::fric.plot(ggplot_bg = plot_k,
                               asb_sp_coord2D = list("Diurnal" = sp_faxes_coord_diurnal,
                                                     "Nocturnal" = sp_faxes_coord_nocturnal,
                                                     "Cathemeral" = sp_faxes_coord_cath),
                               asb_vertices_nD = list("Diurnal" = vert_nm_diurnal,
                                                      "Nocturnal" = vert_nm_nocturnal,
                                                      "Cathemeral" = vert_nm_cath),
                               plot_sp = TRUE,
                               color_ch = NA,
                               fill_ch = c("Diurnal" = "#EEAD0E",
                                           "Nocturnal" = "grey30",
                                           "Cathemeral" = "#8FBC8F"),
                               alpha_ch = c("Diurnal" = 0.5,
                                            "Nocturnal" = 0.5,
                                            "Cathemeral" = 0.5),
                               shape_sp = c("Diurnal" = 16,
                                            "Nocturnal" = 16,
                                            "Cathemeral" = 16),
                               size_sp = c("Diurnal" = 0.6,
                                           "Nocturnal" = 0.6,
                                           "Cathemeral" = 0.6),
                               color_sp = c("Diurnal" = "#EEAD0E",
                                            "Nocturnal" = "grey30",
                                            "Cathemeral" = "#8FBC8F"),
                               fill_sp = c("Diurnal" = "#EEAD0E",
                                           "Nocturnal" = "grey30",
                                           "Cathemeral" = "#8FBC8F"),
                               shape_vert = c("Diurnal" = 16,
                                              "Nocturnal" = 16,
                                              "Cathemeral" = 16),
                               size_vert = c("Diurnal" = 0.6,
                                             "Nocturnal" = 0.6,
                                             "Cathemeral" = 0.6),
                               color_vert = c("Diurnal" = "#EEAD0E",
                                              "Nocturnal" = "grey30",
                                              "Cathemeral" = "#8FBC8F"),
                               fill_vert = c("Diurnal" = "#EEAD0E",
                                             "Nocturnal" = "grey30",
                                             "Cathemeral" = "#8FBC8F"))
fctsp_hull_act

# -----

## Do functional space highlighting taxa:

# Compute the range of functional axes:
range_sp_coord  <- range(sp_faxes_coord_all)

# Based on the range of species coordinates values, compute a nice range ...
# ... for functional axes:
range_faxes <- range_sp_coord +
  c(-1, 1) * (range_sp_coord[2] - range_sp_coord[1]) * 0.05
range_faxes


# get species coordinates along the two studied axes:
sp_faxes_coord_xy <- sp_faxes_coord_all[, c("PC1", "PC2")]

# Plot background with grey backrgound:
plot_k <- mFD::background.plot(range_faxes = range_faxes,
                               faxes_nm = c("PC1", "PC2"),
                               color_bg = "grey98")
plot_k


# Retrieve vertices coordinates along the two studied functional axes:
vert <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_xy,
                      order_2D = FALSE,
                      check_input = TRUE)

plot_k <- mFD::pool.plot(ggplot_bg = plot_k,
                         sp_coord2D = sp_faxes_coord_xy,
                         vertices_nD = vert,
                         plot_pool = FALSE,
                         color_pool = NA,
                         fill_pool = NA,
                         alpha_ch =  0.8,
                         color_ch = "white",
                         fill_ch = "white",
                         shape_pool = NA,
                         size_pool = NA,
                         shape_vert = NA,
                         size_vert = NA,
                         color_vert = NA,
                         fill_vert = NA)
plot_k

# Build an asb df with taxa:
asb_sp_taxa_df <- proj_traits_noNA_df %>%
  dplyr::select(c("species", "taxa")) %>% 
  dplyr::mutate(value = 1) %>%
  dplyr::distinct() %>% 
  tidyr::pivot_wider(names_from  = species, values_from = value, 
                     values_fill = 0) %>% 
  tibble::column_to_rownames(var = "taxa")

# Fish:
## filter fish species:
sp_filter_fish <- mFD::sp.filter(asb_nm = c("Fish"),
                                 sp_faxes_coord = sp_faxes_coord_xy,
                                 asb_sp_w = asb_sp_taxa_df)
## get species coordinates:
sp_faxes_coord_fish <- sp_filter_fish$`species coordinates`

# Amphibians:
## filter amphibians species:
sp_filter_amph <- mFD::sp.filter(asb_nm = c("Amphibians"),
                                 sp_faxes_coord = sp_faxes_coord_xy,
                                 asb_sp_w = asb_sp_taxa_df)
## get species coordinates:
sp_faxes_coord_amph <- sp_filter_amph$`species coordinates`

# Mammals:
## filter mammals species:
sp_filter_mamm <- mFD::sp.filter(asb_nm = c("Mammals"),
                                 sp_faxes_coord = sp_faxes_coord_xy,
                                 asb_sp_w = asb_sp_taxa_df)
## get species coordinates:
sp_faxes_coord_mamm <- sp_filter_mamm$`species coordinates`


# Retrieve names of sp being vertices:
# Fish:
vert_nm_fish <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_fish,
                              order_2D = TRUE,
                              check_input = TRUE)
# Amph:
vert_nm_amph <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_amph,
                              order_2D = TRUE,
                              check_input = TRUE)
# Mamm:
vert_nm_mamm <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_mamm,
                              order_2D = TRUE,
                              check_input = TRUE)

# Add the convex hulls:
fctsp_hull_all <- mFD::fric.plot(ggplot_bg = plot_k,
                                 asb_sp_coord2D = list("Fish" = sp_faxes_coord_fish,
                                                       "Amphibians" = sp_faxes_coord_amph,
                                                       "Mammals" = sp_faxes_coord_mamm),
                                 asb_vertices_nD = list("Fish" = vert_nm_fish,
                                                        "Amphibians" = vert_nm_amph,
                                                        "Mammals" = vert_nm_mamm),
                                 plot_sp = TRUE,
                                 color_ch = NA,
                                 fill_ch = c("Fish" = "#6BAED6",
                                             "Amphibians" = "#C51B7D",
                                             "Mammals" = "#C9A227"),
                                 alpha_ch = c("Fish" = 0.5,
                                              "Amphibians" = 0.5,
                                              "Mammals" = 0.5),
                                 
                                 shape_sp = c("Fish" = 16,
                                              "Amphibians" = 16,
                                              "Mammals" = 16),
                                 size_sp = c("Fish" = 0.6,
                                             "Amphibians" = 0.6,
                                             "Mammals" = 0.6),
                                 color_sp = c("Fish" = "#6BAED6",
                                              "Amphibians" = "#C51B7D",
                                              "Mammals" = "#C9A227"),
                                 fill_sp = c("Fish" = "#6BAED6",
                                             "Amphibians" = "#C51B7D",
                                             "Mammals" = "#C9A227"),
                                 shape_vert = c("Fish" = 16,
                                                "Amphibians" = 16,
                                                "Mammals" = 16),
                                 size_vert = c("Fish" = 0.6,
                                               "Amphibians" = 0.6,
                                               "Mammals" = 0.6),
                                 color_vert = c("Fish" = "#6BAED6",
                                                "Amphibians" = "#C51B7D",
                                                "Mammals" = "#C9A227"),
                                 fill_vert = c("Fish" = "#6BAED6",
                                               "Amphibians" = "#C51B7D",
                                               "Mammals" = "#C9A227"))
fctsp_hull_all


# -----

## Highlight mammals assemblages in the functional space:


# Compute the range of functional axes:
range_sp_coord  <- range(sp_faxes_coord_all)

# Based on the range of species coordinates values, compute a nice range ...
# ... for functional axes:
range_faxes <- range_sp_coord +
  c(-1, 1) * (range_sp_coord[2] - range_sp_coord[1]) * 0.05
range_faxes


# get species coordinates along the two studied axes:
sp_faxes_coord_xy <- sp_faxes_coord_all[, c("PC1", "PC2")]

# Plot background with grey backrgound:
plot_k <- mFD::background.plot(range_faxes = range_faxes,
                               faxes_nm = c("PC1", "PC2"),
                               color_bg = "grey98")
plot_k


# Retrieve vertices coordinates along the two studied functional axes:
vert <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_xy,
                      order_2D = FALSE,
                      check_input = TRUE)

plot_k <- mFD::pool.plot(ggplot_bg = plot_k,
                         sp_coord2D = sp_faxes_coord_xy,
                         vertices_nD = vert,
                         plot_pool = FALSE,
                         color_pool = NA,
                         fill_pool = NA,
                         alpha_ch =  0.8,
                         color_ch = "white",
                         fill_ch = "white",
                         shape_pool = NA,
                         size_pool = NA,
                         shape_vert = NA,
                         size_vert = NA,
                         color_vert = NA,
                         fill_vert = NA)
plot_k


# Do an asb df for mammals asb vs others:
asb_sp_mam_df <- proj_traits_noNA_df %>%
  dplyr::select(c("species", "project")) %>% 
  dplyr::mutate(mamm_asb = case_when(
    project == "SEV" ~ "SEV",
    project == "KBS_MAM" ~ "KBS_MAM",
    ! project %in% c("SEV", "KBS") ~ "Other")) %>% 
  dplyr::select(-c("project")) %>% 
  dplyr::mutate(value = 1) %>%
  dplyr::distinct() %>% 
  tidyr::pivot_wider(names_from  = species, values_from = value, 
                     values_fill = 0) %>% 
  tibble::column_to_rownames(var = "mamm_asb")

# SEV:
## filter SEV species:
sp_filter_SEV <- mFD::sp.filter(asb_nm = c("SEV"),
                                 sp_faxes_coord = sp_faxes_coord_xy,
                                 asb_sp_w = asb_sp_mam_df)
## get species coordinates:
sp_faxes_coord_SEV <- sp_filter_SEV$`species coordinates`

# KBS_MAM:
## filter KBS_MAM species:
sp_filter_KBS <- mFD::sp.filter(asb_nm = c("KBS_MAM"),
                                 sp_faxes_coord = sp_faxes_coord_xy,
                                 asb_sp_w = asb_sp_mam_df)
## get species coordinates:
sp_faxes_coord_KBS <- sp_filter_KBS$`species coordinates`

# Others:
## filter Other species:
sp_filter_other <- mFD::sp.filter(asb_nm = c("Other"),
                                sp_faxes_coord = sp_faxes_coord_xy,
                                asb_sp_w = asb_sp_mam_df)
## get species coordinates:
sp_faxes_coord_other <- sp_filter_other$`species coordinates`


# Retrieve names of sp being vertices:
# SEV:
vert_nm_SEV <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_SEV,
                              order_2D = TRUE,
                              check_input = TRUE)
# KBS:
vert_nm_KBS <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_KBS,
                              order_2D = TRUE,
                              check_input = TRUE)
# Others:
vert_nm_other <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_other,
                             order_2D = TRUE,
                             check_input = TRUE)


# Add the convex hulls:
fctsp_hull_mamm1 <- mFD::fric.plot(ggplot_bg = plot_k,
                                 asb_sp_coord2D = list("Other" = sp_faxes_coord_other),
                                 asb_vertices_nD = list("Other" = vert_nm_other),
                                 plot_sp = TRUE,
                                 color_ch = NA,
                                 fill_ch = c("Other" = "grey85"),
                                 alpha_ch = c("Other" = 0.4),
                                 shape_sp = c("Other" = 16),
                                 size_sp = c("Other" = 0.2),
                                 color_sp = c("Other" = "grey85"),
                                 fill_sp = c("Other" = "grey85"),
                                 shape_vert = c("Other" = 16),
                                 size_vert = c("Other" = 0.2),
                                 color_vert = c("Other" = "grey85"),
                                 fill_vert = c("Other" = "grey85"))
fctsp_hull_mamm1

fctsp_hull_mamm2 <- mFD::fric.plot(ggplot_bg = fctsp_hull_mamm1,
                                   asb_sp_coord2D = list("SEV" = sp_faxes_coord_SEV,
                                                         "KBS" = sp_faxes_coord_KBS),
                                   asb_vertices_nD = list("SEV" = vert_nm_SEV,
                                                          "KBS" = vert_nm_KBS),
                                   plot_sp = TRUE,
                                   color_ch = NA,
                                   fill_ch = c("SEV" = "#C9A227",
                                               "KBS" = "#8B6B3E"),
                                   alpha_ch = c("SEV" = 0.5,
                                                "KBS" = 0.5),
                                   shape_sp = c("SEV" = 16,
                                                 "KBS" = 16),
                                   size_sp = c("SEV" = 0.6,
                                               "KBS" = 0.6),
                                   color_sp = c("SEV" = "#C9A227",
                                                "KBS" = "#8B6B3E"),
                                   fill_sp = c("SEV" = "#C9A227",
                                               "KBS" = "#8B6B3E"),
                                   shape_vert = c("SEV" = 16,
                                                  "KBS" = 16),
                                   size_vert = c("SEV" = 0.6,
                                                 "KBS" = 0.6),
                                   color_vert = c("SEV" = "#C9A227",
                                                  "KBS" = "#8B6B3E"),
                                   fill_vert = c("SEV" = "#C9A227",
                                                 "KBS" = "#8B6B3E"))
fctsp_hull_mamm2


# 7 - Build functional space for mammal species ===========================


# Subset the asb and traits df to mammals:
asb_sp_mam_df <- asb_sp_df %>% 
  dplyr::filter(rownames(asb_sp_df) %in% c("SEV", "KBS_MAM"))
asb_sp_mam_df <- asb_sp_mam_df %>% 
  dplyr::select(which(!colSums(asb_sp_mam_df, na.rm=TRUE) %in% 0))

sp_tr_mam_df <- sp_tr_df %>% 
  dplyr::filter(rownames(sp_tr_df) %in% colnames(asb_sp_mam_df))

# Compute functional distance:
sp_dist_mamm <- mFD::funct.dist(
  sp_tr         = sp_tr_mam_df,
  tr_cat        = tr_cat_df,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)
dist_df <- mFD::dist.to.df(list("df" = sp_dist_all))

# Build functional space - check quality dimensions:
fspaces_quality_mamm <- mFD::quality.fspaces(
  sp_dist             = sp_dist_mamm,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality_mamm,
  quality_metric             = "mad",
  fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d", 
                                 "pcoa_4d"))

# Get species coordinates:
sp_faxes_coord_mamm <- fspaces_quality_mamm$"details_fspaces"$"sp_pc_coord"


# Get link between axes and traits:
mamm_tr_faxes <- mFD::traits.faxes.cor(
  sp_tr          = sp_tr_mam_df, 
  sp_faxes_coord = sp_faxes_coord_mamm[ , c("PC1", "PC2", "PC3", "PC4")], 
  plot           = TRUE)
mamm_tr_faxes

# Plot functional space:
fctsp_mamm <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_mamm[ , c("PC1", "PC2", "PC3", "PC4")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "#53868B",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "#EEAD0E",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "#EEAD0E",
  fill_vert       = "#EEAD0E",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)
fctsp_mamm


# Plot the two asb:
# First compute fric:
fric_mamm <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_mamm[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = as.matrix(asb_sp_mam_df),
  ind_vect         = c("fric"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

plots_fric_mamm <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = fric_mamm,
  plot_asb_nm              = c("SEV", "KBS_MAM"),
  ind_nm                   = c("fric"),
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "grey95",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = 1, asb2 = 1),
  color_sp                 = c(pool = "grey50", asb1 = "#C9A227", asb2 = "#8B6B3E"),
  color_vert               = c(pool = "grey50", asb1 = "#C9A227", asb2 = "#8B6B3E"),
  fill_sp                  = c(pool = NA, asb1 = "#C9A227", asb2 = "#8B6B3E"),
  fill_vert                = c(pool = NA, asb1 = "#C9A227", asb2 = "#8B6B3E"),
  color_ch                 = c(pool = NA, asb1 = "#C9A227", asb2 = "#8B6B3E"),
  fill_ch                  = c(pool = "white", asb1 = "#C9A227", asb2 = "#8B6B3E"),
  alpha_ch                 = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
  size_sp_nm               = 3, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "plain",
  save_file                = FALSE,
  check_input              = TRUE) 

