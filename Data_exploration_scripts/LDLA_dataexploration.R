################################################################################
##
## Data Exploration
## Taxa: Fish, Mammals, Amphibians
## Traits: Mass, Life Span, Trophic Level (num), Active Time
##
## Leander DL Anderegg
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
  # has some duplicates

# clean species list to remove duplicates # WILL EVENTUALLY BE REMOVED
# -----------Species list wrangling start --------

sp_pro_list_v2 <- sp_pro_list %>%
  dplyr::mutate(across(everything(), na_if, y = ""))

#Isolate all distinct taxa names, the original master list by project so duplicates across programs with same taxa
master_sp_list <- sp_pro_list %>%
  dplyr::select(scientific_name) %>%
  dplyr::distinct() 

#total_sci_name_duplicates <- sum(duplicated(master_sp_list$scientific_name))
#length(unique(master_sp_list$scientific_name)) # 2529 distinct taxa in master species list 

master_sp_list_v2 <- master_sp_list %>%
  dplyr::left_join(
    sp_pro_list_v2 %>%
      dplyr::select(project,habitat,raw_filename,scientific_name, phylum, class, order, family, genus)%>%
      dplyr::distinct(),
    by = "scientific_name")


#Check duplicates
master_sp_duplicates <- sum(duplicated(master_sp_list_v2$scientific_name))  
#scientific_name but have at least one difference in higher order taxonomic level 

#Extract duplicates scientific_names 160 unique 
master_sp_dup_names <-master_sp_list_v2 %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::filter(n() > 1) %>%
  ungroup()

##For now will arbitrarily keep the first occurrence but will go back and check correct class names. Also which source?
master_sp_remove_dup <- master_sp_dup_names %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::slice(1) %>% # select first row for each scientific name
  dplyr::ungroup() #now 160 obs 

# remove duplicate names from 'master_sp_list_v2' 

master_sp_no_dup <- master_sp_list_v2 %>%
  dplyr::anti_join(master_sp_dup_names, 
                   by= "scientific_name")

#combine master sp. list with no duplicates with paired down scientific_names 'master_sp_remove_dup'

master_sp_list_ready <- bind_rows(master_sp_no_dup, master_sp_remove_dup) #now should have same names as orig. 'master_sp_list'





# Load imputed traits:
imp_tr_df <- read.csv(file.path("Data", "traits_tidy-data", "consumer-trait-species-imputed-taxonmic-database.csv"))


# 2 - Merge traits with program species list ===================================================

#Join trait data with program species list 
program_sp_trt_data <- dplyr::left_join(master_sp_list_ready, imp_tr_df,
                                        by="scientific_name") %>%
  dplyr::mutate(
    order = coalesce(order.x, order.y),
    family = coalesce(family.x, family.y),
    genus = coalesce(genus.x, genus.y)) %>%
  dplyr::select(-ends_with(".x"), -ends_with(".y")) %>%
  dplyr::relocate(order, family, genus, .before = sex) %>%
  dplyr::relocate(source, .before = scientific_name) 
#some source info absent need to fix this later!!! 

program_sp_trt_data$age_life.span_years[which(program_sp_trt_data$age_life.span_years<=0)] <- NA


# we're gonna skip PIE (no traits)
# KBS_INS - insects
# KONZA - insects

# Create a new column for Taxa:
program_sp_trt_data <- program_sp_trt_data %>% 
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
    project == "KBS_AMP" ~ "Amphibians",
    project %in% c("HARVARD", "KBS_BIR","SBC_BEACH") ~ "Birds",
    project %in% c("NGA","Arctic","Palmer","CCE","NorthLakes") ~ "Zooplankton",
    TRUE ~ NA
    )) %>% filter(!is.na(taxa), scientific_name!= "Homo sapiens") %>% 
  dplyr::mutate(taxa = factor(taxa))


all_traits <- program_sp_trt_data %>% 
  dplyr::select(c(1:9, 
                "age_life.span_years",
                "diet_trophic.level_num",
                "reproduction_reproductive.rate_num.offspring.per.year",
                "mass_adult_g",
                "active.time_category_ordinal", "taxa")) %>%
  group_by(project) %>% mutate(tr.age.zp = scale(age_life.span_years)[,1],
                               tr.trophic.level.zp = scale(diet_trophic.level_num)[,1],
                               tr.reproductive.rate.zp = scale(reproduction_reproductive.rate_num.offspring.per.year)[,1],
                               tr.mass.adult.zp = scale(log(mass_adult_g, 10))[,1]
                               )

# Order and create the Active time categories:
all_traits1 <- all_traits %>% 
  dplyr::mutate(active.time_category_new = case_when(
    active.time_category_ordinal == "diurnal" ~ "diurnal",
    active.time_category_ordinal == "nocturnal" ~ "nocturnal",
    ! active.time_category_ordinal %in% c("diurnal", "nocturnal", "") ~ "cathemeral")) %>% 
  dplyr::select(-c(active.time_category_ordinal)) %>% 
  dplyr::rename(tr.active.time = active.time_category_new)

# ----

# Reduce the project trait dataframe to species with tr data complete:
all_traits.verts <- all_traits1 %>% filter(taxa %in% c("Fish","Amphibians","Mammals"))

all_traits.verts$n_nas <- rowSums(is.na(all_traits.verts[,grep(pattern = "tr.", x = colnames(all_traits.verts))]))
  
all_traits.final <- all_traits.verts %>% filter(n_nas < 3)




onlytraits <- data.frame(all_traits.final %>% ungroup() %>% select(starts_with("tr.")) %>% mutate(tr.active.time=as.factor(tr.active.time)))


rownames(onlytraits) <- all_traits.final$scientific_name
#colnames(onlytraits) <- c("tr.age.zp", "tr.trophic.level.zp", "tr.reproductive.rate.zp", "tr.mass.adult.zp", "tr.active.time" )

onlytraits.nat <- onlytraits %>% select(-tr.active.time)


# Build a dataframe gathering traits categories:
tr_nm <- colnames(onlytraits)
tr_cat <- c("Q", "Q", "Q","Q","N")
tr_cat_df <- as.data.frame(matrix(ncol = 2, nrow = 5))
tr_cat_df[, 1] <- tr_nm
tr_cat_df[, 2] <- tr_cat
colnames(tr_cat_df) <- c("trait_name", "trait_type")

# NO ACTIVE TIME Build a dataframe gathering traits categories:
tr_nm <- colnames(onlytraits.nat)
tr_cat <- c("Q", "Q", "Q","Q")
tr_cat_df <- as.data.frame(matrix(ncol = 2, nrow = 4))
tr_cat_df[, 1] <- tr_nm
tr_cat_df[, 2] <- tr_cat
colnames(tr_cat_df) <- c("trait_name", "trait_type")



# Explore traits repartition:
traits_summ <- mFD::sp.tr.summary(
  tr_cat     = tr_cat_df,   
  sp_tr      = onlytraits.nat, 
  stop_if_NA = FALSE)
traits_summ$tr_summary_list

# # Explore projects:
# asb_sp_matrix <- as.matrix(asb_sp_df)
# asb_sp_summ <- mFD::asb.sp.summary(asb_sp_w = asb_sp_matrix)
# asb_sp_summ$asb_sp_richn

# Look at continuous traits correlations:
# corr_df <- sp_tr_df %>% 
#   tibble::rownames_to_column(var = "species") %>% 
#   dplyr::left_join(proj_traits_noNA_df[, c(3, 4)], by = "species") %>% 
#   dplyr::distinct()
# GGally::ggpairs(corr_df[, c(2:4)], aes(color = corr_df$taxa, 
#                                        alpha = 0.5))


# 5 - Build functional space for all species ===================================


# Compute functional distance:
sp_dist_all <- mFD::funct.dist(
  sp_tr         = onlytraits.nat,
  tr_cat        = tr_cat_df,
  metric        = "gower",
  scale_euclid  = "noscale",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = FALSE)
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
  sp_tr          = onlytraits.nat, 
  sp_faxes_coord = sp_faxes_coord_all[ , c("PC1", "PC2", "PC3")], 
  plot           = TRUE, 
  stop_if_NA = FALSE)
all_tr_faxes

# Plot functional space:
fctsp_all <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_all[ , c("PC1", "PC2", "PC3")],
  faxes           = c("PC1", "PC2", "PC3"),
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



# ----- Reattach metadata to ordination space dataframe ---------

sp.faxes <- data.frame(sp_faxes_coord_all) %>% mutate(scientific.name=rownames(.))

taxa.df <- onlytraits.nat %>% mutate(scientific.name = rownames(.)) %>% 
  left_join(all_traits.final, by=c("scientific.name"="scientific_name")) %>%
  left_join(sp.faxes)










####### OLD CODE ###############

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