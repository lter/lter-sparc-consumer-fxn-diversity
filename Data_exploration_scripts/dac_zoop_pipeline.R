################################################################################
# dac_zoop_pipeline.R
#
# Purpose: Combined pipeline for zooplankton trait processing:
#   1. Read imputed consumer trait database
#   2. Filter to target project zooplankton (6 filtering steps)
#   3. Join community diet/biomass and dry weight data
#   4. Assign biomass conversion categories
#   5. Apply group-level length-to-drymass conversions (DW = a * L^b)
#   6. Join fecundity and lifespan data
#   7. Trait audit: identify orders with fully missing key traits
#
# Inputs:
#   - Data/traits_tidy-data/consumer-trait-species-imputed-taxonmic-database.csv
#   - Data/community_tidy-data/04_harmonized_consumer_excretion_sparc_cnd_site.csv
#   - Data/community_raw-data/Zooplankton_dry_ind_wt.csv
#   - Data/species_tidy-data/23_species_master-spp-list.csv
#   - Data/zooplankton_edit_traits/freshwater_zooplankton_sp_list.csv
#   - Data/zooplankton_edit_traits/AGO_zoop_fecundity.csv
#   - Data/zooplankton_edit_traits/Zooplankton_Lifespan - Sheet1.csv
#
# Outputs:
#   - Data/zooplankton_edit_traits/biomass_covnert_cat_review.csv
#   - Data/zooplankton_edit_traits/order_level_DW_L_coefficients.csv
#   - Data/zooplankton_edit_traits/zoop_trait_db_biomass_converted.csv
#   - Data/zooplankton_edit_traits/zoop_trait_db_biomass_share.csv
#   - Data/zooplankton_edit_traits/zoop_trait_audit_post_join.csv
#
# Authors: Shalanda Grier, Dante Capone
# Date: 2/2026
################################################################################

library(tidyverse)

base_dir <- here::here()
data_dir <- file.path(base_dir, "Data", "zooplankton_edit_traits")

# Use temp dir to avoid OneDrive file locks, then copy at end
temp_out <- file.path(tempdir(), "zoop_pipeline_out")
dir.create(temp_out, showWarnings = FALSE, recursive = TRUE)

## ========================================================================== ##
# Section 1: Read Input Data ----
## ========================================================================== ##

trait_db <- read_csv(
  file.path(base_dir, "Data", "traits_tidy-data",
            "consumer-trait-species-imputed-taxonmic-database.csv"),
  show_col_types = FALSE)
cat("Trait DB loaded:", nrow(trait_db), "rows x", ncol(trait_db), "columns\n")

comm_exc <- read_csv(
  file.path(base_dir, "Data", "community_tidy-data",
            "04_harmonized_consumer_excretion_sparc_cnd_site.csv"),
  show_col_types = FALSE)

zoop_drywt <- read_csv(
  file.path(base_dir, "Data", "community_raw-data",
            "Zooplankton_dry_ind_wt.csv"),
  show_col_types = FALSE) %>%
  rename_with(str_trim) %>%
  mutate(across(where(is.character), str_trim))

master_spp <- read_csv(
  file.path(base_dir, "Data", "species_tidy-data",
            "23_species_master-spp-list.csv"),
  show_col_types = FALSE)

zoop_coeff <- read_csv(
  file.path(data_dir, "freshwater_zooplankton_sp_list.csv"),
  show_col_types = FALSE)

fecundity_data <- read_csv(
  file.path(data_dir, "AGO_zoop_fecundity.csv"),
  show_col_types = FALSE)

lifespan_data <- read_csv(
  file.path(data_dir, "Zooplankton_Lifespan - Sheet1.csv"),
  show_col_types = FALSE)

## ========================================================================== ##
# Section 1.5: Supplement with Unmined Traits from trait_dataset_level2 ----
## ========================================================================== ##
# These traits are in the raw long-format data but were filtered out by
# 11_traits_harmonization.R (line 133). We extract them directly here,
# following the same harmonization pattern used in scripts 11 & 12.

# Read raw long-format marine zooplankton trait data
trtlong_raw <- read_csv(
  file.path(base_dir, "Data", "traits_raw-data",
            "trait_dataset_level2-2023-09-14.csv"),
  show_col_types = FALSE)

# Pare down to unmined traits of interest (matching 11's filter style)
trtlong_supp <- trtlong_raw %>%
  dplyr::filter(traitName %in% c("eggDiameter", "developmentDuration",
                                  "growthRate_15C")) %>%

  # Standardize trait names (matching 11's case_when pattern)
  dplyr::mutate(trait_std = dplyr::case_when(
    traitName == "eggDiameter"        ~ "length_egg",
    traitName == "developmentDuration" ~ "age_development.duration",
    traitName == "growthRate_15C"      ~ "growth_rate",
    TRUE ~ "NOT FIXED")) %>%

  # Standardize units (matching 11's unit tidying)
  dplyr::mutate(trait_unit = dplyr::case_when(
    traitUnit == "um"         ~ "um",
    traitUnit == "day"        ~ "days",
    traitUnit == "mg C h^-1"  ~ "mg.C.per.hour",
    TRUE ~ traitUnit))

# Verify no traits fell through
stopifnot(all(trtlong_supp$trait_std != "NOT FIXED"))

# Separate numeric and non-numeric (matching 11's pattern)
trtlong_supp_num <- trtlong_supp %>%
  dplyr::filter(!is.na(suppressWarnings(as.numeric(traitValue))))

# Average numeric values per taxon (matching 11's collapse pattern)
trtlong_supp_avg <- trtlong_supp_num %>%
  dplyr::mutate(traitValue = as.numeric(traitValue)) %>%
  dplyr::group_by(scientificName, trait_std, trait_unit, phylum, class,
                  order, family, genus) %>%
  dplyr::summarize(traitValue = mean(traitValue, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(traitValue = as.character(traitValue))

# Assemble wide-format column names (matching 11's paste pattern)
trtlong_supp_wide <- trtlong_supp_avg %>%
  dplyr::mutate(trait_columns = ifelse(!is.na(trait_unit) & nchar(trait_unit) != 0,
                                       yes = paste(trait_std, trait_unit, sep = "_"),
                                       no = trait_std)) %>%
  dplyr::select(-trait_std, -trait_unit) %>%
  tidyr::pivot_wider(names_from = trait_columns,
                     values_from = traitValue)

# Apply 12-style QC: replace empty strings with NA, force numeric
supp_numcols <- c("length_egg_um", "age_development.duration_days",
                   "growth_rate_mg.C.per.hour")
supp_numcols <- intersect(supp_numcols, names(trtlong_supp_wide))

trtlong_supp_wide <- trtlong_supp_wide %>%
  dplyr::mutate(dplyr::across(dplyr::everything(),
    ~ ifelse(nchar(.) == 0, NA, .))) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(supp_numcols), as.numeric))

# Unit conversion: egg diameter um → cm (matching 12's mm-to-cm pattern)
if ("length_egg_um" %in% names(trtlong_supp_wide)) {
  trtlong_supp_wide <- trtlong_supp_wide %>%
    dplyr::mutate(length_egg_cm = length_egg_um / 10000) %>%
    dplyr::select(-length_egg_um)
}

# Rename to match trait_db join column
trtlong_supp_wide <- trtlong_supp_wide %>%
  dplyr::rename(scientific_name = scientificName)

cat("\n=== Supplementary trait mining (trait_dataset_level2) ===\n")
cat("Traits extracted:", paste(setdiff(names(trtlong_supp_wide),
    c("scientific_name", "phylum", "class", "order", "family", "genus")),
    collapse = ", "), "\n")
cat("Species covered:", n_distinct(trtlong_supp_wide$scientific_name), "\n")

# Left-join supplementary traits into trait_db, coalescing where columns overlap
# (matching 12's coalesce pattern for backfilling taxonomy/traits)
overlap_cols <- intersect(names(trtlong_supp_wide), names(trait_db))
overlap_cols <- setdiff(overlap_cols, "scientific_name")

supp_join <- trtlong_supp_wide %>%
  dplyr::select(scientific_name, dplyr::all_of(
    setdiff(names(trtlong_supp_wide),
            c("phylum", "class", "order", "family", "genus"))))

trait_db <- trait_db %>%
  dplyr::left_join(
    supp_join %>% dplyr::rename_with(~ paste0(.x, "_supp"),
                                      .cols = -scientific_name),
    by = "scientific_name") %>%
  # Coalesce: keep existing values, fill gaps from supplement
  {
    df <- .
    for (col in setdiff(names(supp_join), "scientific_name")) {
      supp_col <- paste0(col, "_supp")
      if (col %in% names(df) && supp_col %in% names(df)) {
        df <- df %>% dplyr::mutate(
          !!col := dplyr::coalesce(!!rlang::sym(col), !!rlang::sym(supp_col)))
        df <- df %>% dplyr::select(-dplyr::all_of(supp_col))
      } else if (supp_col %in% names(df)) {
        df <- df %>% dplyr::rename(!!col := !!supp_col)
      }
    }
    df
  }

cat("Trait DB after supplement:", nrow(trait_db), "rows x", ncol(trait_db), "cols\n")
cat("  length_egg_cm non-NA:", sum(!is.na(trait_db$length_egg_cm)), "\n")
cat("  age_development.duration_days non-NA:",
    sum(!is.na(trait_db$age_development.duration_days)), "\n")
cat("  growth_rate_mg.C.per.hour non-NA:",
    sum(!is.na(trait_db$growth_rate_mg.C.per.hour)), "\n")

## ========================================================================== ##
# Section 2: Join Taxonomy from Master Species List ----
## ========================================================================== ##

taxonomy_lookup <- master_spp %>%
  filter(!is.na(phylum) | !is.na(order) | !is.na(family)) %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  select(scientific_name, phylum, order, family)

trait_db <- trait_db %>%
  select(-any_of(c("phylum", "order", "family"))) %>%
  left_join(taxonomy_lookup, by = "scientific_name")

cat("Taxonomy joined — non-NA phylum:", sum(!is.na(trait_db$phylum)),
    "order:", sum(!is.na(trait_db$order)),
    "family:", sum(!is.na(trait_db$family)),
    "of", nrow(trait_db), "rows\n")

# Collapse to one row per scientific_name: coalesce across sources so best data is kept
trait_db <- trait_db %>%
  group_by(scientific_name) %>%
  summarise(
    across(where(is.numeric), ~ first(na.omit(.))),
    across(where(is.character), ~ first(na.omit(.))),
    .groups = "drop"
  )
cat("Trait DB collapsed to one row per species:", nrow(trait_db), "rows\n")

## ========================================================================== ##
# Section 3: Filter to Target Project Zooplankton ----
## ========================================================================== ##

target_projects <- c("Arctic", "NorthLakes", "NGA", "CCE", "Palmer")

project_spp <- master_spp %>%
  filter(project %in% target_projects) %>%
  distinct(scientific_name, project, habitat)
view(project_spp)

cat("\n--- Species tracking through filters ---\n")
cat("Total species in project_spp:", n_distinct(project_spp$scientific_name), "\n")

# Step 1: Filter to target project species
trait_filtered <- trait_db %>%
  filter(scientific_name %in% project_spp$scientific_name)
cat("Step 1 — project filter:", nrow(trait_filtered), "\n")

# Step 2: Chordata filter — keep gelatinous zooplankton orders only
trait_filtered <- trait_filtered %>%
  filter(
    !(phylum == "Chordata" &
        !order %in% c("Doliolida", "Copelata", "Salpida", "Pyrosomida") &
        !is.na(order)) |
      scientific_name %in% c("Appendicularia", "Ascidiacea")
  )
cat("Step 2 — Chordata filter:", nrow(trait_filtered), "\n")

# Step 3: Order blacklist (non-zooplankton orders, fish orders)
order_blacklist <- c("Diptera", "Octopoda", "Cumacea", "Isopoda",
                     "Scalpellomorpha", "Myxopoda", "Orthoptera", "Radiolaria",
                     "Argentiniformes", "Scorpaeniformes", "Myctophiformes",
                     "Pleuronectiformes", "Perciformes")
trait_filtered <- trait_filtered %>%
  filter(!order %in% order_blacklist)
cat("Step 3 — order blacklist:", nrow(trait_filtered), "\n")

# Step 4: Family blacklist (fish families)
family_blacklist <- c("Bathylagidae", "Cottidae", "Cyclopteridae",
                      "Myctophidae", "Pleuronectidae", "Zaproridae", "Zoarcidae")
if ("family" %in% colnames(trait_filtered)) {
  trait_filtered <- trait_filtered %>%
    filter(!family %in% family_blacklist)
  cat("Step 4 — family blacklist:", nrow(trait_filtered), "\n")
}

# Step 5: Species blacklist
species_blacklist <- c("Ammodytes hexapterus", "Diaphus theta",
                       "Stenobrachius leucopsarus")
trait_filtered <- trait_filtered %>%
  filter(!scientific_name %in% species_blacklist)
cat("Step 5 — species blacklist:", nrow(trait_filtered), "\n")

# Step 6: Non-zooplankton / meroplankton filter
trait_filtered <- trait_filtered %>%
  filter(!order %in% c("Actiniaria", "Sabellida"),
         !phylum %in% c("Echinodermata", "Nematoda", "Nemertea",
                         "Phoronida", "Platyhelminthes"),
         !scientific_name %in% c("Acari", "Coelenterata", "Rhizaria"),
         !grepl(";", phylum))
cat("Step 6 — meroplankton/non-zoop filter:", nrow(trait_filtered), "\n")

# Join project information back to get distinct project×taxa combos
trait_filtered <- trait_filtered %>%
  left_join(project_spp %>% select(scientific_name, project), by = "scientific_name") %>%
  # Only keep rows that matched project_spp (filter out any extra scientific_name entries)
  filter(!is.na(project))

# Ensure distinct project×taxa combos (some species appear in multiple projects)
trait_filtered <- trait_filtered %>%
  distinct(project, scientific_name, .keep_all = TRUE)

cat("Final distinct project×taxa combos:", nrow(trait_filtered), "\n")
view(trait_filtered)

## ========================================================================== ##
# Section 4: Join Community Diet/Biomass and Dry Weight Data ----
## ========================================================================== ##

comm_diet_biomass <- comm_exc %>%
  select(scientific_name, diet_cat, dmperind_g.ind) %>%
  distinct(scientific_name, .keep_all = TRUE)

trait_filtered <- trait_filtered %>%
  left_join(comm_diet_biomass, by = "scientific_name") %>%
  left_join(zoop_drywt %>%
              select(scientific_name, drymass_g) %>%
              distinct(scientific_name, .keep_all = TRUE),
            by = "scientific_name") %>%
  mutate(
    mass_adult_g = as.numeric(mass_adult_g),
    drymass_g    = as.numeric(drymass_g),
    mass_adult_g = case_when(
      !is.na(drymass_g) & drymass_g > 0 ~ drymass_g,
      TRUE ~ mass_adult_g
    ),
    diet_trophic.level_num = case_when(
      diet_cat == "algae_invert"   ~ 1.5,
      diet_cat == "algae_detritus" ~ 1.0,
      diet_cat == "invert"         ~ 2.0,
      TRUE ~ as.numeric(diet_trophic.level_num)
    ),
    missing_length_and_mass = is.na(length_adult.max_cm) & is.na(drymass_g),
    missing_mass = is.na(mass_adult_g),
    has_length   = !is.na(length_adult.max_cm)
  )

cat("\nAfter community joins:", nrow(trait_filtered), "rows x",
    ncol(trait_filtered), "columns\n")
view(trait_filtered)
## ========================================================================== ##
# Section 5: Assign Biomass Conversion Categories ----
## ========================================================================== ##

# Initialize biomass_covnert_cat column
if (!"biomass_covnert_cat" %in% colnames(trait_filtered)) {
  trait_filtered <- trait_filtered %>% mutate(biomass_covnert_cat = NA_character_)
}

# Join existing categories from freshwater zooplankton list
if ("biomass_covnert_cat" %in% colnames(zoop_coeff)) {
  existing_cats <- zoop_coeff %>%
    filter(!is.na(biomass_covnert_cat) & biomass_covnert_cat != "") %>%
    select(scientific_name,
           biomass_covnert_cat_existing = biomass_covnert_cat) %>%
    distinct(scientific_name, .keep_all = TRUE)

  trait_filtered <- trait_filtered %>%
    left_join(existing_cats, by = "scientific_name") %>%
    mutate(biomass_covnert_cat = coalesce(biomass_covnert_cat,
                                          biomass_covnert_cat_existing)) %>%
    select(-biomass_covnert_cat_existing)
}

# Phylum + order -> biomass_covnert_cat mapping
cat_mapping <- tribble(
  ~phylum,          ~order,              ~proposed_cat,
  "Annelida",       "Phyllodocida",      "polychaetes",
  "Annelida",       "Sabellida",         NA_character_,
  "Arthropoda",     "Calanoida",         "copepods",
  "Arthropoda",     "Cyclopoida",        "copepods",
  "Arthropoda",     "Harpacticoida",     "copepods",
  "Arthropoda",     "Monstrilloida",     "copepods",
  "Arthropoda",     "Poecilostomatoida", "copepods",
  "Arthropoda",     "Amphipoda",         "amphipod",
  "Arthropoda",     "Decapoda",          "decapoda",
  "Arthropoda",     "Diplostraca",       "diplostraca",
  "Arthropoda",     "Euphausiacea",      "euphausiacea",
  "Arthropoda",     "Halocyprida",       "ostracoda",
  "Arthropoda",     "Mysida",            "mysid",
  "Arthropoda",     "Siphonophorida",    "siphonophore",
  "Chaetognatha",   "Aphragmophora",     "chaetognatha",
  "Chaetognatha",   "Phragmophora",      "chaetognatha",
  "Chordata",       "Copelata",          "copelata",
  "Chordata",       "Doliolida",         "doliolida",
  "Chordata",       "Pyrosomida",        "pyrosomida",
  "Chordata",       "Salpida",           "doliolida",
  "Cnidaria",       "Actiniaria",        NA_character_,
  "Cnidaria",       "Anthoathecata",     "hydrozoa",
  "Cnidaria",       "Anthoathecatae",    "hydrozoa",
  "Cnidaria",       "Coronatae",         "scyphozoa",
  "Cnidaria",       "Leptothecata",      "hydrozoa",
  "Cnidaria",       "Leptothecatae",     "hydrozoa",
  "Cnidaria",       "Limnomedusae",      "hydrozoa",
  "Cnidaria",       "Narcomedusae",      "hydrozoa",
  "Cnidaria",       "Semaeostomeae",     "scyphozoa",
  "Cnidaria",       "Siphonophorae",     "siphonophore",
  "Cnidaria",       "Trachymedusae",     "hydrozoa",
  "Ctenophora",     "Beroida",           "ctenophore",
  "Ctenophora",     "Cydippida",         "ctenophore",
  "Ctenophora",     "Lobata",            "ctenophore",
  "Ctenophora",     "Thalassocalycida",  "ctenophore",
  "Mollusca",       "Myopsida",          "cephalopod",
  "Mollusca",       "Oegopsida",         "cephalopod",
  "Mollusca",       "Pteropoda",         "thecosome pteropod",
  "Mollusca",       "Teuthida",          "cephalopod"
)

na_phyla <- c("Bryozoa", "Echinodermata", "Nematoda", "Nemertea",
              "Phoronida", "Platyhelminthes")

trait_filled <- trait_filtered %>%
  mutate(old_biomass_covnert_cat = biomass_covnert_cat) %>%
  left_join(cat_mapping, by = c("phylum", "order")) %>%
  mutate(
    biomass_covnert_cat = case_when(
      !is.na(old_biomass_covnert_cat) & old_biomass_covnert_cat != "" ~
        old_biomass_covnert_cat,
      !is.na(proposed_cat) ~ proposed_cat,
      grepl(";", phylum) ~ NA_character_,
      phylum %in% na_phyla ~ NA_character_,
      phylum == "Arthropoda" & is.na(order) ~ NA_character_,
      phylum == "Chaetognatha" ~ "chaetognatha",
      phylum == "Ctenophora"   ~ "ctenophore",
      phylum == "Cnidaria"     ~ "hydrozoa",
      phylum == "Annelida"     ~ "polychaetes",
      phylum == "Mollusca"     ~ "thecosome pteropod",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-proposed_cat)

# Export review of category assignments
review <- trait_filled %>%
  filter(is.na(old_biomass_covnert_cat) | old_biomass_covnert_cat == "") %>%
  select(scientific_name, phylum, order, old_biomass_covnert_cat,
         biomass_covnert_cat, length_adult.max_cm, mass_adult_g)

cat("\n=== Biomass category fill summary ===\n")
cat("Total rows:", nrow(trait_filled), "\n")
cat("Rows changed:", nrow(review), "\n")
cat("Breakdown of new assignments:\n")
print(table(review$biomass_covnert_cat, useNA = "ifany"))

write_csv(review, file.path(temp_out, "biomass_covnert_cat_review.csv"))
view(review)
## ========================================================================== ##
# Section 6: Apply Length-to-Drymass Conversions ----
## ========================================================================== ##

# All coefficients standardized to: DW(mg) = a * L(mm)^b
coeff_table <- tribble(
  ~biomass_convert_cat,   ~a_DW_mg,    ~b,     ~C_pct_of_DW, ~source,                                    ~original_form,                                          ~notes,
  "ostracoda",            0.017072,    2.545,  NA,           "Lavaniegos & Ohman (2007)",                 "DW(ug) = 17.072 * L(mm)^2.545",                        "ug->mg: /1000",
  "copelata",             0.0388,      2.574,  NA,           "Lavaniegos & Ohman (2007)",                 "DW(ug) = 38.8 * L(mm)^2.574",                          "ug->mg: /1000; Oikopleura longicauda",
  "pyrosomida",           0.111,       1.90,   NA,           "Lavaniegos & Ohman (2007)",                 "DW(mg) = 0.111 * L(mm)^1.90",                          "Already in mg; Pyrosoma atlanticum",
  "thecosome pteropod",   0.0026,      2.659,  NA,           "Lavaniegos & Ohman (2007)",                 "DW(ug) = 2.6 * L(mm)^2.659",                           "ug->mg: /1000; pooled thecosomes",
  "copepods",             0.001494,    2.512,  40,           "Lavaniegos & Ohman (2007); Omori (1969)",   "log10 C(ug) = -6.76 + 2.512*log10 L(um)",              "log10 form; um->mm, ug->mg, C/0.40; Bamstedt (1986)",
  "euphausiacea",         0.000765,    3.174,  44,           "Lavaniegos & Ohman (2007); Lasker (1966)",  "log10 C(ug) = -0.473 + 3.174*log10 L(mm)",             "log10 form; ug->mg, C/0.44; E. pacifica; Ross (1982)",
  "doliolida",            0.0085,      2.28,   6,            "Lavaniegos & Ohman (2007); Madin et al. (1981)", "C(ug) = 0.51 * L(mm)^2.28",                        "ug->mg, C/0.06; Deibel (1998)",
  "hydrozoa",             0.03777,     2.619,  5,            "Lavaniegos & Ohman (2007); Larson (1986)",  "C(ug) = 1.8885 * L(mm)^2.619",                         "ug->mg, C/0.05; Clarke et al. (1992)",
  "siphonophore",         0.10976,     0.834,  18.65,        "Lavaniegos & Ohman (2007); Gorsky (1988)",  "C(ug) = 20.47 * L(mm)^0.834",                          "ug->mg, C/0.1865; Biggs (1977)",
  "chaetognatha",         0.000252,    2.9093, 38,           "Lavaniegos & Ohman (2007); Ikeda & Kirkwood (1989)", "C(ug) = 0.0956 * L(mm)^2.9093",               "ug->mg, C/0.38; Bamstedt (1986)",
  "polychaetes",          0.02143,     1.3848, 35,           "Lavaniegos & Ohman (2007); Clarke et al. (1992)", "C(ug) = 7.5 * L(mm)^1.3848",                     "ug->mg, C/0.35; Omori (1969)",
  "decapoda",             0.3325,      2.44,   40,           "Lavaniegos & Ohman (2007); Childress & Nygaard (1974)", "C(mg) = 0.133 * L(mm)^2.44",               "C/0.40; Sergestidae; Omori (1969)",
  "ctenophore",           0.0815,      2.3,    NA,           "Literature average (in freshwater_zooplankton_sp_list.csv)", "DW(mg) = 0.0815 * L(mm)^2.3",          "Already DW in mg",
  "scyphozoa",            0.0114,      2.721,  NA,           "Lucas et al. (1999); in freshwater_zooplankton_sp_list.csv", "DW(mg) = 0.0114 * L(mm)^2.721",       "Already DW in mg",
  "mysid",                1.8197,      3.10,   NA,           "Ikeda (1992)",                              "log10 DM = 3.10*log10 BL + 0.26",                      "log10 form; 10^0.26 = 1.8197; DW in mg",
  "amphipod",             0.01645,     2.7826, NA,           "In freshwater_zooplankton_sp_list.csv",     "DW(mg) = 0.01645 * L(mm)^2.7826",                      "Gammaridea",
  "diplostraca",          0.006560,    2.9699, NA,           "Bottrell et al. (1976) Cladocera avg of 6 spp", "ln(DW ug) = ln(a) + b*ln(L mm)",                   "ug->mg: /1000; a=exp(mean ln(a)); mean b"
)

# Alias map: existing biomass_covnert_cat names -> coefficient table names
cat_aliases <- c(
  "calanoida"          = "copepods",
  "cyclopoida"         = "copepods",
  "Poecilostomatoid"   = "copepods",
  "euphausiid"         = "euphausiacea",
  "chaetognath"        = "chaetognatha",
  "doliolid"           = "doliolida",
  "amphipoda"          = "amphipod",
  "gammariid"          = "amphipod",
  "hyperiid amphipod"  = "amphipod",
  "ostracod"           = "ostracoda",
  "pyrosome"           = "pyrosomida",
  "appendicularian"    = "copelata",
  "salpida"            = "doliolida",
  "SCYPHOMEDUSAE"      = "scyphozoa",
  "cnidarian"          = "hydrozoa",
  "Diplostraca"        = "diplostraca"
)

write_csv(coeff_table, file.path(temp_out, "order_level_DW_L_coefficients.csv"))

# Apply conversions
trait_converted <- trait_filled %>%
  mutate(
    coeff_key = ifelse(
      biomass_covnert_cat %in% names(cat_aliases),
      cat_aliases[biomass_covnert_cat],
      biomass_covnert_cat
    )
  ) %>%
  left_join(
    coeff_table %>% select(biomass_convert_cat, a_DW_mg, b),
    by = c("coeff_key" = "biomass_convert_cat")
  ) %>%
  mutate(
    length_mm = length_adult.max_cm * 10,

    # Only apply conversion when no existing mass data but length is available
    needs_conversion = is.na(mass_adult_g) & is.na(drymass_g) & !is.na(length_mm),

    converted_drymass_g = ifelse(
      needs_conversion & !is.na(a_DW_mg) & !is.na(b),
      (a_DW_mg * length_mm^b) / 1000,
      NA_real_
    ),
    mass_source = case_when(
      !is.na(mass_adult_g) ~ "original",
      !is.na(drymass_g)    ~ "drymass_direct",
      !is.na(converted_drymass_g) ~ paste0("converted_", biomass_covnert_cat),
      TRUE ~ NA_character_
    ),
    mass_adult_g = case_when(
      !is.na(mass_adult_g) ~ mass_adult_g,
      !is.na(drymass_g)    ~ drymass_g,
      !is.na(converted_drymass_g) ~ converted_drymass_g,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-a_DW_mg, -b, -length_mm, -converted_drymass_g,
         -needs_conversion, -old_biomass_covnert_cat, -coeff_key)

cat("\n=== Biomass conversion summary ===\n")
cat("Total rows:", nrow(trait_converted), "\n")
cat("\nmass_source breakdown:\n")
print(table(trait_converted$mass_source, useNA = "ifany"))

cat("\nRows still missing mass_adult_g:\n")
still_missing <- trait_converted %>%
  filter(is.na(mass_adult_g)) %>%
  count(biomass_covnert_cat, name = "n_missing") %>%
  arrange(desc(n_missing))
print(still_missing, n = 30)

write_csv(trait_converted,
          file.path(temp_out, "zoop_trait_db_biomass_converted.csv"))

## ========================================================================== ##
# Section 7: Join Fecundity and Lifespan Data ----
## ========================================================================== ##

# Prepare fecundity subset
fecundity_subset <- fecundity_data %>%
  select(scientific_name, starts_with("reproduction_fecundity")) %>%
  distinct(scientific_name, .keep_all = TRUE)

# Prepare lifespan subset
lifespan_subset <- lifespan_data %>%
  select(scientific_name, contains("lifespan"), contains("life"),
         contains("longevity")) %>%
  distinct(scientific_name, .keep_all = TRUE)

# Remove overlapping columns from trait_converted before joining
fecundity_overlap <- setdiff(colnames(fecundity_subset), "scientific_name")
lifespan_overlap  <- setdiff(colnames(lifespan_subset), "scientific_name")

trait_enhanced <- trait_converted %>%
  select(-any_of(c(fecundity_overlap, lifespan_overlap))) %>%
  left_join(fecundity_subset, by = "scientific_name") %>%
  left_join(lifespan_subset, by = "scientific_name") %>%
  mutate(reproduction_fecundity_num = coalesce(
    reproduction_fecundity_num,
    reproduction_fecundity_num_eggs.female..1))

cat("\n=== Fecundity/lifespan join summary ===\n")
cat("Rows after join:", nrow(trait_enhanced), "\n")
cat("Fecundity matches:", sum(!is.na(trait_enhanced$reproduction_fecundity_num)), "\n")

for (col in lifespan_overlap) {
  if (col %in% colnames(trait_enhanced)) {
    cat("Lifespan column", col, "matches:",
        sum(!is.na(trait_enhanced[[col]])), "\n")
  }
}

write_csv(trait_enhanced,
          file.path(temp_out, "zoop_trait_db_biomass_share.csv"))

        view(trait_enhanced)

## ========================================================================== ##
# Section 8: Trait Audit ----
## ========================================================================== ##

# All traits to audit (excluding taxonomy and metadata columns)
all_traits <- colnames(trait_enhanced) %>%
  setdiff(c("scientific_name", "phylum", "order", "family", "project", "habitat",
            "biomass_covnert_cat", "missing_length_and_mass", "missing_mass", "has_length"))
cat("\n=== Auditing all traits ===\n")
cat("Total traits found:", length(all_traits), "\n")
cat("Traits:", paste(all_traits, collapse = ", "), "\n")

# Convert character trait columns to numeric presence for auditing
audit_data <- trait_enhanced %>%
  mutate(across(all_of(all_traits),
                ~ if (is.character(.x))
                  if_else(!is.na(.x) & .x != "", 1, NA_real_)
                else .x))

# Pivot to long form for flexible auditing
audit_long <- audit_data %>%
  select(scientific_name, any_of(c("phylum", "order", "family")),
         all_of(all_traits)) %>%
  pivot_longer(cols = all_of(all_traits),
               names_to = "trait", values_to = "value") %>%
  mutate(has_value = !is.na(value) & value != "")

# For each order x trait, count taxa with at least one non-NA value
order_coverage <- audit_long %>%
  group_by(order, trait) %>%
  summarise(
    phylum    = first(na.omit(phylum)),
    n_taxa    = n_distinct(scientific_name),
    n_present = sum(has_value),
    .groups   = "drop"
  )

# Create comprehensive audit showing all traits for all orders
audit <- order_coverage %>%
  mutate(
    status = case_when(
      n_present == 0 ~ "ALL MISSING",
      n_present == n_taxa ~ "COMPLETE",
      TRUE ~ paste0("PARTIAL (", n_present, "/", n_taxa, ")")
    )
  ) %>%
  select(phylum, order, n_taxa, trait, status) %>%
  pivot_wider(names_from = trait, values_from = status,
              values_fill = "ok") %>%
  mutate(n_traits_fully_missing = rowSums(
    across(-c(phylum, order, n_taxa), ~ . == "ALL MISSING"))) %>%
  arrange(desc(n_traits_fully_missing), order)

cat("\n=== Orders with trait coverage ===\n")
print(audit, n = nrow(audit))

write_csv(audit, file.path(temp_out, "zoop_trait_audit_post_join.csv"))

# Key traits of interest audit
key_traits <- c("mass_adult_g", "diet_trophic.level_num", "length_adult.max_cm",
                "reproduction_fecundity_num", "age_life.span_years", "drymass_g",
                "length_egg_cm", "age_development.duration_days",
                "growth_rate_mg.C.per.hour")
key_traits <- intersect(key_traits, colnames(trait_enhanced))

cat("\n=== Key traits of interest audit ===\n")
cat("Key traits:", paste(key_traits, collapse = ", "), "\n")

key_audit_long <- audit_long %>%
  filter(trait %in% key_traits)

# Per-project summary
key_project <- key_audit_long %>%
  left_join(trait_enhanced %>% select(scientific_name, project) %>% distinct(),
            by = "scientific_name") %>%
  group_by(project, trait) %>%
  summarise(
    n_taxa    = n_distinct(scientific_name),
    n_present = sum(has_value),
    pct_coverage = round(100 * sum(has_value) / n(), 1),
    .groups = "drop"
  ) %>%
  select(project, trait, n_taxa, n_present, pct_coverage) %>%
  pivot_wider(names_from = trait, values_from = c(n_present, pct_coverage))

cat("\n--- Key trait coverage by project ---\n")
print(key_project, n = nrow(key_project))

# Per-order summary for key traits only
key_order <- key_audit_long %>%
  group_by(order, trait) %>%
  summarise(
    phylum    = first(na.omit(phylum)),
    n_taxa    = n_distinct(scientific_name),
    n_present = sum(has_value),
    .groups   = "drop"
  ) %>%
  mutate(
    status = case_when(
      n_present == 0 ~ "ALL MISSING",
      n_present == n_taxa ~ "COMPLETE",
      TRUE ~ paste0("PARTIAL (", n_present, "/", n_taxa, ")")
    )
  ) %>%
  select(phylum, order, n_taxa, trait, status) %>%
  pivot_wider(names_from = trait, values_from = status,
              values_fill = "ok") %>%
  arrange(order)

cat("\n--- Key trait coverage by order ---\n")
print(key_order, n = nrow(key_order))

write_csv(key_order, file.path(temp_out, "zoop_key_trait_audit.csv"))
view(key_order)

## ========================================================================== ##
# Section 9: Impute Traits to Order Level ----
## ========================================================================== ##
# Following the same imputation pattern as imputed_consumer-trait-database.R:
#   1. Pivot to long format
#   2. Summarize means at genus / family / order levels
#   3. Coalesce: species-level → genus avg → family avg → order avg
#   4. Track imputation level for each value

# Identify trait columns (exclude taxonomy, metadata, and existing impute cols)
meta_cols <- c("scientific_name", "phylum", "order", "class", "family", "genus",
               "project", "habitat", "biomass_covnert_cat",
               "missing_length_and_mass", "missing_mass", "has_length")
trait_cols <- setdiff(names(trait_enhanced), meta_cols)
trait_cols <- trait_cols[!grepl("_impute\\.level$", trait_cols)]

cat("\n=== Section 9: Imputation to order level ===\n")
cat("Traits to impute:", length(trait_cols), "\n")

# Pivot to long format (matching imputed script's pattern)
zoop_long <- trait_enhanced %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(),
    ~ ifelse(nchar(.) == 0 | . %in% c("NA", "NaN"), NA, .))) %>%
  tidyr::pivot_longer(cols = dplyr::all_of(trait_cols),
                      names_to = "trait_name",
                      values_to = "trait_value_orig")

# Summarize to genus (matching imputed script's group_by → summarise pattern)
gen_long_z <- trait_enhanced %>%
  dplyr::group_by(order, family, genus) %>%
  dplyr::summarise(
    dplyr::across(dplyr::where(is.numeric),
                  ~ mean(., na.rm = TRUE)),
    dplyr::across(dplyr::where(is.character),
                  ~ paste(setdiff(unique(.), NA), collapse = "; ")),
    .groups = "drop") %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(),
    ~ ifelse(nchar(.) == 0 | . %in% c("NA", "NaN"), NA, .))) %>%
  tidyr::pivot_longer(cols = dplyr::all_of(intersect(trait_cols, names(.))),
                      names_to = "trait_name",
                      values_to = "trait_value_genus") %>%
  # Keep only taxonomy + trait_name + trait_value to avoid duplicates
  dplyr::select(order, family, genus, trait_name, trait_value_genus)

# Summarize to family
fam_long_z <- trait_enhanced %>%
  dplyr::group_by(order, family) %>%
  dplyr::summarise(
    dplyr::across(dplyr::where(is.numeric),
                  ~ mean(., na.rm = TRUE)),
    dplyr::across(dplyr::where(is.character),
                  ~ paste(setdiff(unique(.), NA), collapse = "; ")),
    .groups = "drop") %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(),
    ~ ifelse(nchar(.) == 0 | . %in% c("NA", "NaN"), NA, .))) %>%
  tidyr::pivot_longer(cols = dplyr::all_of(intersect(trait_cols, names(.))),
                      names_to = "trait_name",
                      values_to = "trait_value_family") %>%
  # Keep only taxonomy + trait_name + trait_value to avoid duplicates
  dplyr::select(order, family, trait_name, trait_value_family)

# Summarize to order
ord_long_z <- trait_enhanced %>%
  dplyr::group_by(order) %>%
  dplyr::summarise(
    dplyr::across(dplyr::where(is.numeric),
                  ~ mean(., na.rm = TRUE)),
    dplyr::across(dplyr::where(is.character),
                  ~ paste(setdiff(unique(.), NA), collapse = "; ")),
    .groups = "drop") %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(),
    ~ ifelse(nchar(.) == 0 | . %in% c("NA", "NaN"), NA, .))) %>%
  tidyr::pivot_longer(cols = dplyr::all_of(intersect(trait_cols, names(.))),
                      names_to = "trait_name",
                      values_to = "trait_value_order") %>%
  # Keep only taxonomy + trait_name + trait_value to avoid duplicates
  dplyr::select(order, trait_name, trait_value_order)

# Join taxonomic-level summaries (matching imputed script's left_join chain)
# No suffix needed since we only keep trait_value columns from each level
zoop_long_v02 <- zoop_long %>%
  dplyr::left_join(gen_long_z, by = c("order", "family", "genus", "trait_name")) %>%
  dplyr::left_join(fam_long_z, by = c("order", "family", "trait_name")) %>%
  dplyr::left_join(ord_long_z, by = c("order", "trait_name"))

# Coalesce: keep original, fill from genus, family, order
zoop_long_v03 <- zoop_long_v02 %>%
  dplyr::mutate(dplyr::across(dplyr::everything(),
    ~ ifelse(nchar(.) == 0 | . %in% c("NA", "NaN"), NA, .))) %>%
  dplyr::mutate(trait_value = dplyr::case_when(
    !is.na(trait_value_orig)   ~ trait_value_orig,
    !is.na(trait_value_genus)  ~ trait_value_genus,
    !is.na(trait_value_family) ~ trait_value_family,
    !is.na(trait_value_order)  ~ trait_value_order,
    TRUE ~ NA_character_)) %>%
  # Track imputation level (matching imputed script's imputed column)
  dplyr::mutate(impute_level = dplyr::case_when(
    !is.na(trait_value_orig)   ~ "not imputed",
    !is.na(trait_value_genus)  ~ "genus average",
    !is.na(trait_value_family) ~ "family average",
    !is.na(trait_value_order)  ~ "order average",
    TRUE ~ NA_character_)) %>%
  dplyr::select(-dplyr::starts_with("trait_value_")) %>%
  dplyr::distinct()

# Report how many NAs were resolved
na_before <- sum(is.na(zoop_long$trait_value_orig))
na_after  <- sum(is.na(zoop_long_v03$trait_value))
cat("NAs before imputation:", na_before, "\n")
cat("NAs after imputation:", na_after, "\n")
cat("NAs resolved:", na_before - na_after, "\n")

# Imputation level summary
cat("\n--- Imputation level breakdown ---\n")
print(table(zoop_long_v03$impute_level, useNA = "ifany"))

# Pivot trait values back to wide format
zoop_imputed_wide <- zoop_long_v03 %>%
  dplyr::select(-impute_level) %>%
  tidyr::pivot_wider(names_from = trait_name, values_from = trait_value)

# Pivot imputation levels to wide format (matching imputed script's _impute.level columns)
imp_wide_z <- zoop_long_v03 %>%
  dplyr::mutate(name_actual = paste0(trait_name, "_impute.level")) %>%
  dplyr::select(-trait_name, -trait_value) %>%
  tidyr::pivot_wider(names_from = name_actual, values_from = impute_level)

# Attach imputation level columns to the imputed trait data
zoop_imputed <- zoop_imputed_wide %>%
  dplyr::left_join(
    imp_wide_z,
    by = intersect(names(zoop_imputed_wide), names(imp_wide_z)))

# Force numeric columns back to numeric
num_trait_cols <- trait_cols[!grepl("_ordinal$|_category$", trait_cols)]
num_trait_cols <- intersect(num_trait_cols, names(zoop_imputed))
zoop_imputed <- zoop_imputed %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(num_trait_cols),
                               ~ suppressWarnings(as.numeric(.))))

cat("\nImputed zooplankton trait DB:", nrow(zoop_imputed), "rows x",
    ncol(zoop_imputed), "cols\n")

write_csv(zoop_imputed, file.path(temp_out, "zoop_trait_db_imputed.csv"))

## ========================================================================== ##
# Section 10: Functional Trait Space (mFD) ----
## ========================================================================== ##
# Following the same pattern as funct_spaces_across_taxa_Camille.R:
#   - Subset to key zooplankton traits
#   - Log-transform skewed numeric traits
#   - Build trait category df, assemblage × species matrix
#   - Compute Gower distance → PCoA → plot functional space
#   - Convex hulls by project

librarian::shelf(mFD, tibble)

# Select key zooplankton traits for functional space
# Mass, Lifespan, Trophic Level (num), Diet (ordinal) — 4 traits
zoop_fspace <- zoop_imputed %>%
  dplyr::select(scientific_name, project,
                mass_adult_g,
                age_life.span_years,
                diet_trophic.level_num,
                diet_trophic.level_ordinal) %>%
  dplyr::rename(species = scientific_name)
colnames(zoop_imputed)
# Clean: replace 0 or negative values with NA for log-transformable traits
zoop_fspace <- zoop_fspace %>%
  dplyr::mutate(
    mass_adult_g = ifelse(mass_adult_g <= 0, NA, mass_adult_g),
    age_life.span_years = ifelse(age_life.span_years <= 0, NA, age_life.span_years))

# Standardize diet ordinal categories (matching Camille's active.time cleanup)
zoop_fspace <- zoop_fspace %>%
  dplyr::mutate(diet_trophic.level_ordinal = dplyr::case_when(
    tolower(diet_trophic.level_ordinal) %in% c("herbivore", "herbivorous") ~ 1,
    tolower(diet_trophic.level_ordinal) %in% c("carnivore", "carnivorous",
                                                "carnivore-parasite") ~ 2,
    tolower(diet_trophic.level_ordinal) %in% c("omnivore", "omnivorous",
                                                "omniherb") ~ 3,
    tolower(diet_trophic.level_ordinal) %in% c("detritivore") ~ 4,
    TRUE ~ NA_real_)) %>%
  dplyr::mutate(diet_trophic.level_ordinal = as.numeric(diet_trophic.level_ordinal))

# Drop species with incomplete trait data (matching Camille's drop_na)
zoop_fspace_complete <- zoop_fspace %>%
  tidyr::drop_na(mass_adult_g, age_life.span_years,
                 diet_trophic.level_num, diet_trophic.level_ordinal)

cat("\n=== Section 10: mFD Functional Trait Space ===\n")
cat("Species with complete traits:", n_distinct(zoop_fspace_complete$species),
    "of", n_distinct(zoop_fspace$species), "\n")

# Log-transform skewed numeric traits (matching Camille's log10(x + 1))
zoop_fspace_complete <- zoop_fspace_complete %>%
  dplyr::mutate(
    mass_adult_g = log10(mass_adult_g + 1),
    age_life.span_years = log10(age_life.span_years + 1))

# Build species × trait matrix (rownames = species, matching Camille's sp_tr_df)
sp_tr_zoop <- zoop_fspace_complete %>%
  dplyr::select(-project) %>%
  dplyr::distinct(species, .keep_all = TRUE) %>%
  tibble::column_to_rownames(var = "species")

# Build trait categories dataframe (matching Camille's tr_cat_df)
# All traits are now quantitative (Q) since diet ordinal is numeric
tr_nm_z <- colnames(sp_tr_zoop)
tr_cat_z <- c("Q", "Q", "Q", "Q")  # All quantitative now
tr_cat_df_z <- data.frame(trait_name = tr_nm_z,
                           trait_type = tr_cat_z,
                           stringsAsFactors = FALSE)

cat("Trait categories:\n")
print(tr_cat_df_z)

# Build assemblage × species matrix (projects as assemblages)
asb_sp_zoop <- zoop_fspace_complete %>%
  dplyr::select(project, species) %>%
  dplyr::mutate(value = 1) %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(names_from = species, values_from = value,
                     values_fill = 0) %>%
  tibble::column_to_rownames(var = "project")

# Ensure species match between trait and assemblage matrices
shared_spp <- intersect(rownames(sp_tr_zoop), colnames(asb_sp_zoop))
sp_tr_zoop <- sp_tr_zoop[shared_spp, , drop = FALSE]
asb_sp_zoop <- asb_sp_zoop[, shared_spp, drop = FALSE]

cat("Shared species in trait space:", length(shared_spp), "\n")
cat("Projects:", paste(rownames(asb_sp_zoop), collapse = ", "), "\n")

# Explore traits summary
traits_summ_z <- mFD::sp.tr.summary(
  tr_cat     = tr_cat_df_z,
  sp_tr      = sp_tr_zoop,
  stop_if_NA = TRUE)

# Compute functional distance (Gower, matching Camille's settings)
sp_dist_zoop <- mFD::funct.dist(
  sp_tr         = sp_tr_zoop,
  tr_cat        = tr_cat_df_z,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

# Build functional space and check quality (matching Camille's settings)
fspaces_quality_zoop <- mFD::quality.fspaces(
  sp_dist             = sp_dist_zoop,
  maxdim_pcoa         = min(10, length(shared_spp) - 1),
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

# Plot quality of functional spaces
mFD::quality.fspaces.plot(
  fspaces_quality   = fspaces_quality_zoop,
  quality_metric    = "mad",
  fspaces_plot      = c("tree_average", "pcoa_2d", "pcoa_3d", "pcoa_4d"))

# Get species coordinates
sp_faxes_coord_zoop <- fspaces_quality_zoop$"details_fspaces"$"sp_pc_coord"

# Get trait–axes correlations
zoop_tr_faxes <- mFD::traits.faxes.cor(
  sp_tr          = sp_tr_zoop,
  sp_faxes_coord = sp_faxes_coord_zoop[, c("PC1", "PC2", "PC3", "PC4")],
  plot           = TRUE)
cat("\nTrait-axis correlations:\n")
print(zoop_tr_faxes$"tr_faxes_stat")

# Plot global functional space (matching Camille's funct.space.plot call)
fctsp_zoop <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_zoop[, c("PC1", "PC2", "PC3", "PC4")],
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
fctsp_zoop

# --- Convex hulls by zooplankton project (matching Camille's taxa hull pattern) ---

# Compute range of functional axes
range_sp_coord_z <- range(sp_faxes_coord_zoop)
range_faxes_z <- range_sp_coord_z +
  c(-1, 1) * (range_sp_coord_z[2] - range_sp_coord_z[1]) * 0.05

# Get PC1 × PC2 coordinates
sp_faxes_coord_xy_z <- sp_faxes_coord_zoop[, c("PC1", "PC2")]

# Background plot
plot_z <- mFD::background.plot(range_faxes = range_faxes_z,
                                faxes_nm = c("PC1", "PC2"),
                                color_bg = "grey98")

# Global vertices and pool
vert_z <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_xy_z,
                         order_2D = FALSE,
                         check_input = TRUE)

plot_z <- mFD::pool.plot(ggplot_bg = plot_z,
                          sp_coord2D = sp_faxes_coord_xy_z,
                          vertices_nD = vert_z,
                          plot_pool = FALSE,
                          color_pool = NA, fill_pool = NA,
                          alpha_ch = 0.8, color_ch = "white", fill_ch = "white",
                          shape_pool = NA, size_pool = NA,
                          shape_vert = NA, size_vert = NA,
                          color_vert = NA, fill_vert = NA)

# Build assemblage df by project
asb_sp_proj_zoop <- zoop_fspace_complete %>%
  dplyr::select(species, project) %>%
  dplyr::mutate(value = 1) %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(names_from = species, values_from = value,
                     values_fill = 0) %>%
  tibble::column_to_rownames(var = "project")

# Define project colors (ocean-themed palette for zooplankton)
proj_names <- rownames(asb_sp_proj_zoop)
proj_colors <- c("Arctic"     = "#1B9E77",
                 "NorthLakes" = "#D95F02",
                 "NGA"        = "#7570B3",
                 "CCE"        = "#E7298A",
                 "Palmer"     = "#66A61E")
proj_colors <- proj_colors[intersect(names(proj_colors), proj_names)]

# Filter species per project, compute vertices, and build hull lists
sp_coord_list <- list()
vert_list     <- list()

for (proj in names(proj_colors)) {
  if (proj %in% rownames(asb_sp_proj_zoop)) {
    sp_filt <- mFD::sp.filter(asb_nm = proj,
                               sp_faxes_coord = sp_faxes_coord_xy_z,
                               asb_sp_w = asb_sp_proj_zoop)
    sp_coord_list[[proj]] <- sp_filt$`species coordinates`
    # Need at least 3 species for a convex hull
    if (nrow(sp_filt$`species coordinates`) >= 3) {
      vert_list[[proj]] <- mFD::vertices(
        sp_faxes_coord = sp_filt$`species coordinates`,
        order_2D = TRUE,
        check_input = TRUE)
    }
  }
}

# Only plot projects that have ≥ 3 species (convex hull requirement)
hull_projs <- names(vert_list)
cat("\nProjects with convex hulls:", paste(hull_projs, collapse = ", "), "\n")

if (length(hull_projs) >= 1) {
  hull_colors <- proj_colors[hull_projs]

  fctsp_hull_zoop <- mFD::fric.plot(
    ggplot_bg        = plot_z,
    asb_sp_coord2D   = sp_coord_list[hull_projs],
    asb_vertices_nD  = vert_list[hull_projs],
    plot_sp          = TRUE,
    color_ch         = NA,
    fill_ch          = hull_colors,
    alpha_ch         = setNames(rep(0.4, length(hull_projs)), hull_projs),
    shape_sp         = setNames(rep(16, length(hull_projs)), hull_projs),
    size_sp          = setNames(rep(0.8, length(hull_projs)), hull_projs),
    color_sp         = hull_colors,
    fill_sp          = hull_colors,
    shape_vert       = setNames(rep(17, length(hull_projs)), hull_projs),
    size_vert        = setNames(rep(1.2, length(hull_projs)), hull_projs),
    color_vert       = hull_colors,
    fill_vert        = hull_colors)

  print(fctsp_hull_zoop)
  cat("\nFunctional trait space with project convex hulls plotted.\n")
}

# Compute FRic for projects with enough species
asb_sp_matrix_z <- as.matrix(asb_sp_zoop[hull_projs, , drop = FALSE])
if (length(hull_projs) >= 2) {
  fric_zoop <- mFD::alpha.fd.multidim(
    sp_faxes_coord   = sp_faxes_coord_zoop[, c("PC1", "PC2", "PC3", "PC4")],
    asb_sp_w         = asb_sp_matrix_z,
    ind_vect         = c("fric"),
    scaling          = TRUE,
    check_input      = TRUE,
    details_returned = TRUE)

  cat("\n--- Functional Richness (FRic) by project ---\n")
  print(fric_zoop$"functional_diversity_indices")
}

## ========================================================================== ##
# Section 11: Copy Outputs to Data Directory ----
## ========================================================================== ##

for (f in list.files(temp_out, full.names = TRUE)) {
  dest <- file.path(data_dir, basename(f))
  tryCatch({
    file.copy(f, dest, overwrite = TRUE)
    cat("Copied:", basename(f), "\n")
  }, error = function(e) {
    cat("Could not copy", basename(f), "- file may be locked\n")
  })
}

cat("\nPipeline complete.\n")

# End ----
