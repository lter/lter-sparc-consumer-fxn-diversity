### Script to explore traits distribution across taxa and programs for the
## in-person meeting
## 
## 
##
## Lauren Enright
##
## 02/2026

# Load libraries
librarian::shelf(tidyverse)


### Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

### DATA STORED LOCALLY FROM GOOGLE DRIVE. IF UPDATES HAVE BEEN MAKE TO THOSE DATA SHEETS, NEED TO RERUN SCRIPT 
# 000_group-member-shortcut.R

comm_data_aquatic <- read.csv(file.path("Data", "community_tidy-data", "04_harmonized_consumer_excretion_sparc_cnd_site.csv"))
species_data <- read.csv(file.path("Data", "species_tidy-data", "23_species_master-spp-list.csv"))
##### Quick Data Explore #####
glimpse(comm_data)
unique(comm_data$project) #12 projects 
unique(species_data$project) #22 projects

#### ADD COLORS #####

cols <- c(
  CoastalCA = "#08306B",
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

cols2 <- c(
  PISCO_C = "#08306B",
  PISCO_S = "darkblue",
  SBC_ocean = "#1B9E77",
  FCE = "#00441B",
  VCR = "#40B5C4",
  MCR = "#2171B5",
  RLS  = "#6BAED6",
  FISHGLOB = "#636363")


##### Total Species Richness at Project Level #####

#need 14 projects
species_data_sub <- species_data %>% 
  filter(project %in% c("RLS", "FCE", "SBC", "CoastalCA", "MCR", "VCR", "KBS_AMP",
                        "KBS_MAM", "KONZA", "SEV", "MOHONK"))

species_data_sub <- species_data_sub %>% 
  mutate(proj_taxa = case_when(
    project == "CoastalCA" ~ "Fish",
    project == "FCE" ~ "Fish",
    project == "SBC" ~ "Fish",
    project == "MCR" ~ "Fish",
    project == "VCR" ~ "Fish",
    project == "RLS" ~ "Fish",
    project == "KBS_MAM" ~ "Mammals",
    project == "SEV" ~ "Mammals",
    project == "MOHONK" ~ "Amphibians",
    project == "KBS_AMP" ~ "Amphibians")) %>% 
  dplyr::mutate(proj_taxa = factor(proj_taxa))  

unique(species_richness_program$project) 

species_richness_program <- species_data_sub %>%
  group_by(project, proj_taxa) %>%
  summarise(species_richness_total =  n_distinct(scientific_name))

species_richness_program %>%
  filter(proj_taxa %in% c("Amphibians", "Mammals")) %>%
  ggplot(aes(x = project, y = species_richness_total, color = project, fill = project)) +
  geom_col(alpha = 0.5) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_color_manual(values = cols, guide = "none") +
  ggplot2::theme_classic() +
  labs(x = "Project", y = "Total Species Richness") + 
  ggplot2::facet_wrap(~proj_taxa, ncol = 1)
 

#### do a quick check with comm data. these should match #### 

species_richness_total_withab <- comm_data_aquatic %>% 
  filter(!project %in% c("Arctic", "Palmer", "NorthLakes", "PIE", "CCE")) %>%
  #filter(project != "MCR") %>%
  group_by(project, habitat) %>%
  summarise(species_richness = length(unique(scientific_name[dmperind_g.ind != 0])))

species_richness_program
species_richness_total_withab

#####  Species Richness at Site Level - Through Time #####

unique(comm_data_aquatic$project)

#first calculate species richness at lowest point for each 
#Mack --> has NA? mine does not when I read it in? 
species_richness_sites <- comm_data_aquatic %>% 
         filter(!project %in% c("Arctic", "Palmer", "NorthLakes", "PIE", "CCE", "NGA")) %>%
          filter(project != "MCR") %>%
    group_by(project, habitat, year, month, 
                               site, subsite_level1, subsite_level2, subsite_level3) %>%
  summarise(species_richness = length(unique(scientific_name[dmperind_g.ind != 0])))


unique(species_richness_sites$project)

# MCR needs to be added together? --> are there other like this?
species_richness_MCR <- comm_data_aquatic %>%
  filter(project == "MCR") %>%
  group_by(project, habitat, year, month, 
           site, subsite_level1, subsite_level2) %>%
  summarise(species_richness = length(unique(scientific_name[dmperind_g.ind != 0])))
  
species_richness_all <- bind_rows(species_richness_sites, species_richness_MCR)


##### Wrangle data according to site #### 

# each program has their own thing going on, steal from Mack to get this 
program_system_richness_all <- species_richness_all %>%
  mutate(project = as.factor(project)) %>% 
    filter(!project %in% c('Arctic', 'FCE')) %>% 
       mutate(project = case_when(
             project == 'CoastalCA' & site == 'CENTRAL' ~ 'PISCO_C',
             project == 'CoastalCA' & site == 'SOUTH' ~ 'PISCO_S',
             project == 'SBC' & habitat == 'beach' ~ 'SBC_beach',
             project == 'SBC' & habitat == 'ocean' ~ 'SBC_ocean',
             TRUE ~ project)) %>%
mutate(
    system = case_when(
      project == 'SBC_beach' & habitat == 'beach' ~ site,
      project == 'SBC_ocean' & habitat == 'ocean' ~ site,
      project == 'Palmer' ~ site,
      project == 'CCE' ~ site,
      project == 'NGA' ~ subsite_level1,
      project == 'NorthLakes' ~ site,
      project == 'VCR' ~ site,
      project == 'PIE' ~ site,
      project == 'MCR' ~ subsite_level1,
      project == 'PISCO_C' & site == 'CENTRAL' ~ subsite_level1,
      project == 'PISCO_S' & site == 'SOUTH' ~ subsite_level1,
      project == 'FISHGLOB' ~ site,
      project == 'RLS' ~ site,
    ))


program_system_richness_avg <- program_system_richness_all %>%
  filter(project != "SBC_beach") %>%
  group_by(project, system, year) %>% 
  summarize(
    mean_richness = mean(species_richness),
    .groups = 'drop'
  )

#### PLOT #### 

program_system_richness_avg %>%
ggplot(aes(x = year, y = mean_richness, fill = project, color = project, group = system)) + 
  geom_point(aes(fill = project, color = project), shape = 21, size = 2, stroke = 1, alpha = 0.6) +
  geom_line() +
  scale_fill_manual(values = cols2, guide = "none") +
  scale_color_manual(values = cols2, guide = "none") +
 # geom_smooth(method = 'loess') +
  # geom_boxplot(aes(fill = project), position = position_dodge(0.8),
  #              outlier.shape = NA, alpha = 1, size = 1, color = 'black') +
  # scale_y_log10(
  #       breaks = 10^(-2:6),
  #       labels = label_number(big.mark = ",")
  # ) +
  facet_wrap(~project, scale = 'free') + 
  labs(x = 'Project', y = 'Species Richness',
       title = 'Mean Annual Species Richness by Site') + 
  theme(
    strip.text = element_text(size = 16, face = "bold", colour = "black"),
    strip.background = element_blank(),  
    axis.text = element_text(size = 12, face = "bold", colour = "black"),
    axis.title = element_text(size = 14, face = "bold", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", colour = "black",
                              hjust = 0.5)
  )




##### Some Basic Models ###### 
#these aren't linear data, so kinda a bust? 

sR_mod1 <- lmer(species_richness ~ year + year*project + (1|project/site/subsite_level2), data = species_richness_all)
#is singular
sR_mod2 <- lmer(species_richness ~ year + year*project + (1|project/site), data = species_richness_all)

summary(sR_mod2)
car::Anova(sR_mod2, type = "II")
performance::check_model(sR_mod2)


##### Messing around with PISCO specific data ####
program_system_richness_PISCO <- species_richness_all %>%
  filter(project == "CoastalCA") %>%
  mutate(island = str_extract(subsite_level2, pattern = "^[^_]+")) %>%
  mutate(site_new = paste(island, subsite_level1, sep = "_"))

program_system_richness_PISCO_avg <- program_system_richness_PISCO %>%
  group_by(project, habitat, site, year, island, subsite_level1, site_new) %>%
  summarize(species_richness_ann = mean(species_richness),
            se_species_richness = sd(species_richness)/sqrt(n()))

program_system_richness_SBC <- systems %>%
  filter(project == "SBC") %>%
  # mutate(island = str_extract(subsite_level2, pattern = "^[^_]+")) %>%
  mutate(site_new = paste(habitat, site, sep = "_"))

program_system_richness_PISCO_avg %>%
  filter(site == "SOUTH") %>%
  ggplot(aes(x = year, y = species_richness_ann, fill = island, color = island, group = island, shape = subsite_level1)) + 
  geom_point(aes(color = island, shape = subsite_level1, fill = island),
             size = 2, stroke = 1, alpha = 0.6) +
  geom_line() +
  theme(
    strip.text = element_text(size = 16, face = "bold", colour = "black"),
    strip.background = element_blank(),  
    axis.text = element_text(size = 12, face = "bold", colour = "black"),
    axis.title = element_text(size = 14, face = "bold", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold", colour = "black",
                              hjust = 0.5))


unique(program_system_richness_PISCO_avg$site)
species_richness_avg <- species_richness %>%
  group_by(project, habitat, site, year, subsite_level1) %>%
  summarize(species_richness_ann = mean(species_richness),
            se_species_richness = sd(species_richness)/sqrt(n()))