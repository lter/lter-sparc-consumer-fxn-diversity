# Zooplankton Trait Data Processing Pipeline: Complete Methods Documentation

**Script**: `dac_zoop_pipeline.R`  
**Authors**: Dante Capone, Shalanda Grier  
**Date**: February 2026

This document describes the complete workflow for zooplankton trait data processing, including data aggregation, taxonomic filtering, biomass conversion, trait gap-filling, hierarchical imputation, and functional diversity analysis.

## 1. Data Sources

### 1.1 Primary Input Files

- **Imputed trait database**: `consumer-trait-species-imputed-taxonmic-database-copy.csv` — multi-taxa trait database containing species-level values for age, trophic level, maximum adult length (cm), adult mass (g), and fecundity.
- **Community excretion data**: `04_harmonized_consumer_excretion_sparc_cnd_site-copy.csv` — harmonized community-level data including diet categories and dry mass per individual.
- **Zooplankton dry weight lookup**: `Zooplankton_dry_ind_wt.csv` — individual dry weight measurements for zooplankton taxa.
- **Species master list**: `23_species_master-spp-list-copy.csv` — master species list linking taxa to LTER projects.
- **Freshwater zooplankton species list**: `freshwater_zooplankton_sp_list.csv` — taxon-specific length-to-drymass conversion coefficients (a, b) and carbon-to-dry weight percentages compiled from literature.
- **Fecundity data**: `AGO_zoop_fecundity.csv` — zooplankton fecundity measurements.
- **Lifespan data**: `Zooplankton_Lifespan - Sheet1.csv` — zooplankton lifespan information.

### 1.2 Supplementary Trait Mining

Additional traits were extracted from the raw marine zooplankton trait database (`trait_dataset_level2-2023-09-14.csv`) that were filtered out during initial harmonization:

- **Egg diameter** (`length_egg_cm`): Converted from µm to cm
- **Development duration** (`age_development.duration_days`): Days to maturity
- **Growth rate** (`growth_rate_mg.C.per.hour`): Growth rate at 15°C

These traits were averaged per species and joined to the main trait database using a coalesce strategy (existing values retained, gaps filled from supplementary data).

## 2. Project Scope

Analysis was restricted to zooplankton taxa occurring in five LTER projects: **Arctic**, **NorthLakes**, **NGA** (Northern Gulf of Alaska), **CCE** (California Current Ecosystem), and **Palmer** (Palmer Antarctica).

## 3. Taxonomic Filtering

### 3.1 Chordata filter

All Chordata were removed except gelatinous zooplankton orders retained as holoplankton:
- **Retained orders**: Doliolida, Copelata, Salpida, Pyrosomida
- **Whitelisted taxa** (blank order field): Appendicularia, Ascidiacea

### 3.2 Order blacklist

The following orders were removed as non-zooplankton (insects, benthic invertebrates, fish):
- Diptera, Octopoda, Cumacea, Isopoda, Scalpellomorpha, Myxopoda, Orthoptera, Radiolaria
- Fish orders: Argentiniformes, Scorpaeniformes, Myctophiformes, Pleuronectiformes, Perciformes

### 3.3 Family blacklist

The following fish families were removed (conditional on the `family` column existing in the dataset):
- Bathylagidae, Cottidae, Cyclopteridae, Myctophidae, Pleuronectidae, Zaproridae, Zoarcidae

### 3.4 Species blacklist

Three fish species were explicitly removed:
- *Ammodytes hexapterus* (Pacific sand lance)
- *Diaphus theta* (California headlightfish)
- *Stenobrachius leucopsarus* (northern lampfish)

### 3.5 Non-zooplankton and meroplankton assigned NA for biomass conversion

During the biomass conversion category assignment step, 14 taxa were assigned `NA` for `biomass_covnert_cat` because they are non-zooplankton, meroplankton (larval benthic forms), or taxonomically unresolvable multi-phylum aggregates. These taxa lack appropriate length-to-drymass conversion equations and were excluded from biomass conversion:

| Scientific name | Phylum | Order | Reason |
|---|---|---|---|
| *Peachia* | Cnidaria | Actiniaria | Benthic anemone (meroplankton) |
| Oweniidae | Annelida | Sabellida | Benthic polychaete (meroplankton) |
| Acari | Arthropoda | NA | Mites; non-zooplankton |
| Asteroidea | Echinodermata | NA | Sea stars; meroplankton larvae |
| Echinodermata | Echinodermata | NA | Echinoderm larvae; meroplankton |
| Echinoidea | Echinodermata | NA | Sea urchin larvae; meroplankton |
| Holothuroidea | Echinodermata | NA | Sea cucumber larvae; meroplankton |
| Ophiuroidea | Echinodermata | NA | Brittle star larvae; meroplankton |
| Nematoda | Nematoda | NA | Nematodes; non-zooplankton |
| Nemertea | Nemertea | NA | Ribbon worms; non-zooplankton |
| Phoronida | Phoronida | NA | Horseshoe worm larvae; meroplankton |
| Platyhelminthes | Platyhelminthes | NA | Flatworms; non-zooplankton |
| Coelenterata | Multi-phylum | NA | Unresolvable multi-phylum aggregate |
| Rhizaria | Multi-phylum | NA | Unresolvable multi-phylum aggregate |

These taxa should be added to the filtering steps in `dac_zoop_qc.R` or flagged during downstream analyses.

## 4. Trait Gap-Filling

### 4.1 Trophic level assignment

`diet_trophic.level_num` was overwritten based on diet category:
- `algae_invert` → 1.5
- `algae_detritus` → 1.0
- `invert` → 2.0

### 4.2 Dry mass replacement

Where available, `mass_adult_g` was replaced with directly measured `drymass_g` values from the zooplankton dry weight lookup table.

## 5. Higher-Level Biomass Conversion

### 5.1 Biomass conversion category assignment

For taxa lacking species-specific length-to-drymass conversions, each taxon was assigned to a `biomass_covnert_cat` (biomass conversion category) based on its higher-level taxonomy (phylum and order). Categories were assigned programmatically using the following mapping:

| Phylum | Order(s) | Assigned category |
|---|---|---|
| Annelida | Phyllodocida | polychaetes |
| Arthropoda | Amphipoda | amphipod |
| Arthropoda | Monstrilloida | copepods |
| Arthropoda | Mysida | mysid |
| Arthropoda | Siphonophorida | siphonophore |
| Cnidaria | Anthoathecata, Anthoathecatae | hydrozoa |
| Cnidaria | Leptothecata, Leptothecatae | hydrozoa |
| Cnidaria | Limnomedusae | hydrozoa |
| Cnidaria | Narcomedusae | hydrozoa |
| Cnidaria | Semaeostomeae | scyphozoa |
| Cnidaria | Siphonophorae | siphonophore |
| Cnidaria | Trachymedusae | hydrozoa |
| Ctenophora | Thalassocalycida | ctenophore |
| Mollusca | (blank order, Gastropoda) | thecosome pteropod |
| Mollusca | Myopsida, Teuthida | cephalopod |

Note: "Siphonophorida" under Arthropoda is a database artifact (likely a typo for Siphonophorae) and was mapped to `siphonophore`.

### 5.2 Standardized conversion equations

All length-to-drymass regressions were standardized to the form:

**DW(mg) = a × L(mm)^b**

where DW is dry weight in milligrams and L is total length in millimeters.

#### 5.2.1 Equations from Lavaniegos & Ohman (2007)

**Direct dry weight equations** (unit conversion only, µg → mg):

| Category | a (mg) | b | Original form | Source |
|---|---|---|---|---|
| ostracoda | 0.017072 | 2.545 | DW(µg) = 17.072 × L(mm)^2.545 | Lavaniegos & Ohman (2007) |
| copelata | 0.0388 | 2.574 | DW(µg) = 38.8 × L(mm)^2.574 | Lavaniegos & Ohman (2007) |
| pyrosomida | 0.111 | 1.90 | DW(mg) = 0.111 × L(mm)^1.90 | Lavaniegos & Ohman (2007) |
| thecosome pteropod | 0.0026 | 2.659 | DW(µg) = 2.6 × L(mm)^2.659 | Lavaniegos & Ohman (2007) |

**Carbon-based equations** (required C-to-DW conversion):

For regressions reported as carbon mass, dry weight was calculated as:

DW = C / (C% of DW / 100)

where C% of DW is the carbon content as a percentage of dry weight, derived from literature values.

For log10-form regressions (Copepoda, Euphausiacea), the algebraic conversion to power-law form was:

log10(C) = intercept + b × log10(L) → C = 10^(intercept) × L^b

with subsequent unit conversions (µm → mm where applicable, µg → mg) and C-to-DW factor applied.

| Category | a (mg) | b | C% of DW | C% references | Original form |
|---|---|---|---|---|---|
| copepods | 0.001494 | 2.512 | 40% | Omori (1969); Båmstedt (1986) | log10 C(µg) = -6.76 + 2.512 × log10 L(µm) |
| euphausiacea | 0.000765 | 3.174 | 44% | Lasker (1966); Ross (1982) | log10 C(µg) = -0.473 + 3.174 × log10 L(mm) |
| doliolida | 0.0085 | 2.28 | 6% | Madin et al. (1981); Deibel (1998) | C(µg) = 0.51 × L(mm)^2.28 |
| hydrozoa | 0.03777 | 2.619 | 5% | Larson (1986); Clarke et al. (1992) | C(µg) = 1.8885 × L(mm)^2.619 |
| siphonophore | 0.10976 | 0.834 | 18.65% | Gorsky (1988); Biggs (1977) | C(µg) = 20.47 × L(mm)^0.834 |
| chaetognatha | 0.000252 | 2.9093 | 38% | Ikeda & Kirkwood (1989); Båmstedt (1986) | C(µg) = 0.0956 × L(mm)^2.9093 |
| polychaetes | 0.02143 | 1.3848 | 35% | Clarke et al. (1992); Omori (1969) | C(µg) = 7.5 × L(mm)^1.3848 |
| decapoda | 0.3325 | 2.44 | 40% | Childress & Nygaard (1974); Omori (1969) | C(mg) = 0.133 × L(mm)^2.44 |

#### 5.2.2 Equations from other sources

| Category | a (mg) | b | Source | Notes |
|---|---|---|---|---|
| ctenophore | 0.0815 | 2.3 | Literature average | Already DW in mg |
| scyphozoa | 0.0114 | 2.721 | Lucas et al. (1999) | Already DW in mg |
| mysid | 1.8197 | 3.10 | Ikeda (1992) | log10 form converted: 10^0.26 = 1.8197 |
| amphipod | 0.01645 | 2.7826 | Freshwater zooplankton sp. list | Gammaridea |
| diplostraca | 0.006560 | 2.9699 | Bottrell et al. (1976) | Cladocera: avg of 6 spp; ln(a) form, a = exp(mean ln(a)); µg → mg |

### 5.3 Category alias mapping

Because the trait database used inconsistent naming for biomass conversion categories, an alias mapping was applied before joining with the coefficient table:

| Trait DB category | Maps to coefficient |
|---|---|
| calanoida | copepods |
| cyclopoida | copepods |
| Poecilostomatoid | copepods |
| euphausiid | euphausiacea |
| chaetognath | chaetognatha |
| doliolid | doliolida |
| amphipoda | amphipod |
| gammariid | amphipod |
| hyperiid amphipod | amphipod |
| ostracod | ostracoda |
| pyrosome | pyrosomida |
| appendicularian | copelata |
| salpida | doliolida |
| SCYPHOMEDUSAE | scyphozoa |
| cnidarian | hydrozoa |
| Diplostraca | diplostraca |

### 5.4 Conversion application

For each taxon where `mass_adult_g` was missing but `length_adult.max_cm` was available:
1. Length was converted from cm to mm (× 10)
2. The group-level equation DW(mg) = a × L(mm)^b was applied
3. Dry weight was converted from mg to g (÷ 1000)
4. The `mass_source` column was flagged as `converted_<category>`

Priority order for `mass_adult_g`:
1. Original value (if present)
2. Direct drymass measurement (`drymass_g`)
3. Length-to-drymass conversion

### 5.5 Special cases

- **Spongiobranchia**: Uses an exponential form DW(mg) = 1.615 × e^(0.088 × L_mm) rather than the standard power-law (applied in `dac_zoop_qc.R`).
- **Siphonophorae**: Carbon-to-DW conversion uses C% = 18.65% from Gorsky (1988).

## 6. Carbon-to-Dry Weight Literature References

| Taxon group | C% of DW | Range | Primary references |
|---|---|---|---|
| Copepoda | 40% | 35–50% | Omori (1969) J. Oceanogr. Soc. Japan; Båmstedt (1986) "The Biological Chemistry of Marine Copepods"; Ikeda et al. (2001) J. Plankton Res. |
| Euphausiacea | 44% | 40–47% | Lasker (1966) J. Exp. Mar. Biol. Ecol.; Ross (1982) Mar. Biol.; Ikeda & Mitchell (1982) |
| Chaetognatha | 38% | 36–42% | Ikeda & Kirkwood (1989) Mar. Biol.; Båmstedt (1986) |
| Hydromedusae | 5% | 3–10% | Larson (1986) J. Exp. Mar. Biol. Ecol.; Clarke et al. (1992) J. Mar. Biol. Assoc. UK |
| Siphonophorae | 18.65% | 10–25% | Gorsky (1988); Biggs (1977) Ecology |
| Doliolida | 6% | 4–8% | Madin et al. (1981) Mar. Biol.; Deibel (1998) "The Biology of Pelagic Tunicates" |
| Polychaeta (pelagic) | 35% | 30–40% | Clarke et al. (1992); Omori (1969) |
| Decapoda (Sergestidae) | 40% | 38–44% | Childress & Nygaard (1974) Deep-Sea Res.; Omori (1969) |

## 7. Taxonomic Imputation

For taxa with missing trait values, hierarchical taxonomic imputation was applied following the same methodology as `imputed_consumer-trait-database.R`:

### 7.1 Imputation hierarchy

1. **Species-level**: Original measured/reported values (not imputed)
2. **Genus-level**: Mean of all species within the same genus
3. **Family-level**: Mean of all species within the same family  
4. **Order-level**: Mean of all species within the same order

### 7.2 Implementation

- Trait data were pivoted to long format with one row per taxon × trait combination
- Taxonomic means were calculated separately for genus, family, and order levels
- Values were coalesced in hierarchical order: species → genus → family → order
- An `impute_level` column tracks the source of each value ("not imputed", "genus average", "family average", "order average")
- Imputation level columns (`<trait_name>_impute.level`) were created for each trait to document data provenance

### 7.3 Deduplication

Before pivoting back to wide format, duplicate rows per `scientific_name × trait_name` were removed using `distinct()` to prevent `pivot_wider()` errors.

## 8. Fecundity and Lifespan Data Integration

After biomass conversion, additional trait data were joined from manually curated zooplankton-specific sources:

### 8.1 Fecundity data

- **Source**: `AGO_zoop_fecundity.csv`
- **Join key**: `scientific_name`
- **Trait added**: `reproduction_fecundity_num` (number of eggs/offspring)
- Fecundity values were coalesced to preserve existing values and fill gaps from the AGO dataset

### 8.2 Lifespan data

- **Source**: `Zooplankton_Lifespan - Sheet1.csv`
- **Join key**: `scientific_name`
- **Trait added**: `age_life.span_years`
- Lifespan values were coalesced to preserve existing values and fill gaps from the lifespan dataset

## 9. Trait Coverage Auditing

Comprehensive trait auditing was performed to identify data gaps and guide imputation strategies:

### 9.1 All-trait audit

- All trait columns (excluding taxonomy and metadata) were identified
- Character trait columns were converted to numeric presence indicators (1 = present, NA = missing)
- Data were pivoted to long format (one row per taxon × trait)
- Coverage was summarized by order × trait, with status categories:
  - **ALL MISSING**: No taxa in the order have this trait
  - **COMPLETE**: All taxa in the order have this trait
  - **PARTIAL (n/N)**: Some taxa have the trait (n out of N total)

### 9.2 Key trait audit

Nine key zooplankton traits were prioritized for detailed auditing:

1. `mass_adult_g` — Adult body mass
2. `diet_trophic.level_num` — Numeric trophic level
3. `length_adult.max_cm` — Maximum adult length
4. `reproduction_fecundity_num` — Fecundity
5. `age_life.span_years` — Lifespan
6. `drymass_g` — Directly measured dry mass
7. `length_egg_cm` — Egg diameter
8. `age_development.duration_days` — Development duration
9. `growth_rate_mg.C.per.hour` — Growth rate at 15°C

Coverage was summarized by:
- **Project**: Percent coverage of each trait across all taxa in each LTER project
- **Order**: Status of each trait for each taxonomic order

**Output**: `zoop_key_trait_audit.csv`

## 10. Hierarchical Taxonomic Imputation

For taxa with missing trait values, hierarchical taxonomic imputation was applied following the same methodology as `imputed_consumer-trait-database.R`:

### 10.1 Imputation hierarchy

1. **Species-level**: Original measured/reported values (not imputed)
2. **Genus-level**: Mean of all species within the same genus
3. **Family-level**: Mean of all species within the same family  
4. **Order-level**: Mean of all species within the same order

### 10.2 Implementation

1. **Pivot to long format**: Trait data were converted to one row per taxon × trait combination
2. **Calculate taxonomic means**: Separate summaries were computed for genus, family, and order levels
   - Numeric traits: arithmetic mean
   - Character traits: concatenated unique values
3. **Join taxonomic summaries**: Long-format species data were joined with genus, family, and order summaries
4. **Coalesce values**: Values were filled hierarchically: species → genus → family → order
5. **Track imputation level**: An `impute_level` column documents the source of each value:
   - "not imputed" — original species-level value
   - "genus average" — imputed from genus mean
   - "family average" — imputed from family mean
   - "order average" — imputed from order mean
6. **Create imputation columns**: For each trait, a `<trait_name>_impute.level` column was added to document data provenance
7. **Pivot back to wide format**: Data were returned to one row per species with all traits as columns

### 10.3 Deduplication

Before pivoting back to wide format, duplicate rows per `scientific_name × trait_name` were removed using `distinct()` to prevent `pivot_wider()` errors.

### 10.4 Imputation summary

The pipeline reports:
- Number of NA values before imputation
- Number of NA values after imputation
- Number of NA values resolved
- Breakdown of imputation levels across all trait values

**Output**: `zoop_trait_db_imputed.csv`

## 11. Functional Trait Space Analysis

Functional diversity was assessed using the `mFD` package following the methodology of Carmona et al. (2021) and the analysis pattern from `funct_spaces_across_taxa_Camille.R`.

### 11.1 Trait selection

Four key zooplankton traits were selected for functional space construction:

1. **mass_adult_g** — Adult body mass (log10-transformed)
2. **age_life.span_years** — Lifespan (log10-transformed)
3. **diet_trophic.level_num** — Numeric trophic level
4. **diet_trophic.level_ordinal** — Ordinal diet category (converted to numeric: 1=herbivore, 2=carnivore, 3=omnivore, 4=detritivore)

All traits were treated as quantitative (Q) for Gower distance calculation.

### 11.2 Data preparation

- **Negative/zero values**: Replaced with NA for log-transformable traits (mass, lifespan)
- **Log transformation**: `log10(x + 1)` applied to mass and lifespan to normalize skewed distributions
- **Diet standardization**: Ordinal diet categories were standardized to numeric codes
- **Complete cases**: Only species with all four traits were retained for functional space analysis

### 11.3 Functional space construction

1. **Species × trait matrix**: Built with species as rows, traits as columns
2. **Trait categories**: All traits classified as quantitative (Q)
3. **Assemblage × species matrix**: Projects (Arctic, NorthLakes, NGA, CCE, Palmer) as assemblages
4. **Gower distance**: Computed using `mFD::funct.dist()` with:
   - Metric: Gower
   - Scaling: scale_center
   - Ordinal handling: classic
   - Weighting: equal
5. **PCoA ordination**: Principal Coordinates Analysis performed on Gower distance matrix
6. **Quality assessment**: Functional space quality evaluated using Mean Absolute Deviation (MAD)

### 11.4 Functional diversity metrics

- **Trait-axis correlations**: Pearson correlations between original traits and PC axes
- **Functional richness (FRic)**: Convex hull volume in 4D trait space for each project
- **Convex hulls**: 2D projections (PC1 × PC2) plotted for each project with ≥3 species

### 11.5 Visualization

- **Global functional space**: All species plotted in PC1-PC4 space
- **Project convex hulls**: Color-coded by project (Arctic=#1B9E77, NorthLakes=#D95F02, NGA=#7570B3, CCE=#E7298A, Palmer=#66A61E)
- **Quality plots**: MAD values across functional space dimensions (tree, 2D, 3D, 4D PCoA)

## 12. Output Files

### 12.1 Primary Outputs

- `zoop_trait_db_biomass_converted.csv` — Trait database after biomass conversion and fecundity/lifespan joins
- `zoop_trait_db_biomass_share.csv` — Trait database with biomass share calculations
- `zoop_trait_db_imputed.csv` — Final trait database with hierarchical taxonomic imputation and imputation level tracking
- `zoop_trait_audit_post_join.csv` — Comprehensive audit of all traits by order (post-fecundity/lifespan join)
- `zoop_key_trait_audit.csv` — Focused audit of 9 key traits by order and project

### 12.2 Intermediate/Review Files

- `biomass_covnert_cat_review.csv` — Review table of programmatically assigned biomass conversion categories
- `order_level_DW_L_coefficients.csv` — Standardized coefficient table with a, b parameters for each taxonomic group

---

## References

### Biomass Conversion Literature

**Lavaniegos, B. E., & Ohman, M. D. (2007).** Coherence of long-term variations of zooplankton in two sectors of the California Current System. *Progress in Oceanography*, 75(1), 42-69.

**Omori, M. (1969).** Weight and chemical composition of some important oceanic zooplankton in the North Pacific Ocean. *Journal of the Oceanographical Society of Japan*, 25(1), 3-13.

**Båmstedt, U. (1986).** Chemical composition and energy content. In *The Biological Chemistry of Marine Copepods* (pp. 1-58). Oxford University Press.

**Ikeda, T., Kanno, Y., Ozaki, K., & Shinada, A. (2001).** Metabolic rates of epipelagic marine copepods as a function of body mass and temperature. *Marine Biology*, 139(3), 587-596.

**Lasker, R. (1966).** Feeding, growth, respiration, and carbon utilization of a euphausiid crustacean. *Journal of the Fisheries Research Board of Canada*, 23(9), 1291-1317.

**Ross, R. M. (1982).** Energetics of *Euphausia pacifica*. I. Effects of body carbon and nitrogen and temperature on measured and predicted production. *Marine Biology*, 68(1), 1-13.

**Ikeda, T., & Mitchell, A. W. (1982).** Oxygen uptake, ammonia excretion and phosphate excretion by krill and other Antarctic zooplankton in relation to their body size and chemical composition. *Marine Biology*, 71(3), 283-298.

**Ikeda, T., & Kirkwood, J. M. (1989).** Metabolism and elemental composition of a giant chaetognath *Sagitta gazellae* from the Southern Ocean. *Marine Biology*, 100(2), 261-267.

**Larson, R. J. (1986).** Water content, organic content, and carbon and nitrogen composition of medusae from the northeast Pacific. *Journal of Experimental Marine Biology and Ecology*, 99(2), 107-120.

**Clarke, A., Holmes, L. J., & Gore, D. J. (1992).** Proximate and elemental composition of gelatinous zooplankton from the Southern Ocean. *Journal of Experimental Marine Biology and Ecology*, 155(1), 55-68.

**Gorsky, G. (1988).** Aspects de la biologie des siphonophores. *Oceanis*, 14(4), 497-511.

**Biggs, D. C. (1977).** Respiration and ammonium excretion by open ocean gelatinous zooplankton. *Limnology and Oceanography*, 22(1), 108-117.

**Madin, L. P., Cetta, C. M., & McAlister, V. L. (1981).** Elemental and biochemical composition of salps (Tunicata: Thaliacea). *Marine Biology*, 63(2), 113-120.

**Deibel, D. (1998).** The abundance, distribution and ecological impact of doliolids. In *The Biology of Pelagic Tunicates* (pp. 171-186). Oxford University Press.

**Childress, J. J., & Nygaard, M. (1974).** Chemical composition and buoyancy of midwater crustaceans as function of depth of occurrence off Southern California. *Marine Biology*, 27(3), 225-238.

### Functional Diversity Methods

**Carmona, C. P., de Bello, F., Mason, N. W., & Lepš, J. (2021).** Trait probability density (TPD): measuring functional diversity across scales based on TPD with R. *Ecology*, 102(1), e03527.

**Villéger, S., Mason, N. W., & Mouillot, D. (2008).** New multidimensional functional diversity indices for a multifaceted framework in functional ecology. *Ecology*, 89(8), 2290-2301.
