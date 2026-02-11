#----------------------------------##
# SPARC - Consumer Functional Diversity (CFD)
###--------------------------------##

# Script author(s): Shalanda Grier, Li Kui, Nick Lyons
# Sites: Palmer LTER, Arctic LTER, North Lakes LTER

# Load libraries
librarian::shelf(tidyverse,taxize)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()

# do we want to check species 
run_species_check <- "N"

# read in the harmonized data
df <- read.csv(file.path("Data", "community_tidy-data","02_community_wrangled.csv"),stringsAsFactors = F,na.strings =c(""))

#remove the columns that are not needed
dfrm <- df %>%
  select(-c(density_num_km2,biomass_kg,taxonomicLevel))
#### calculate excretion rate

# take out the rows that are needed

df1 <-dfrm %>%
  dplyr::rename(dmperind=dmperind_g.ind,temp=temp_c)

peace<- df1 %>%
  filter(is.na(dmperind)) %>%
  distinct(project,habitat,sp_code,scientific_name,dmperind)

# missing diet_cat
peace2<-df1 %>%
  filter(is.na(diet_cat)) %>%
  distinct(project,habitat,sp_code,scientific_name,diet_cat)

peace3 <-df1 |>
  filter(is.na(scientific_name))

# remove all the species that should not be in the data
df2<-df1 %>%
  filter(!is.na(scientific_name))%>%
  filter(!is.na(diet_cat))



# df6 <- df1 %>%
#   mutate(family = case_when(
#     scientific_name == "Cichlasoma urophthalmus" ~ "Cichlidae",
#     scientific_name == "Aphrododerus sayanus" ~ "Aphrododeridae",
#     scientific_name == "Farfantepenaeus duorarum" ~ "Penaeidae",
#     scientific_name == "Gambusia holbrooki" ~ "Poeciliidae",
#     scientific_name == "Kleptolebias marmoratus" ~ "Rivulidae",
#     scientific_name == "Palaemonetes pugio" ~ "Palaemonidae",
#     TRUE ~ family  # Keep the existing family if none of the above conditions are met
#   ))


###########################
#using bradley's code below for excretion calculation
cons <- df2

cons_np_ratio <- cons %>%
  #N
  mutate(N_vert_coef = if_else(phylum == "Chordata", 0.7804, 0),
         N_diet_coef = if_else(diet_cat == "algae_detritus", -0.0389,
                               if_else(diet_cat == "invert", -0.2013,
                                       if_else(diet_cat == "fish", -0.0537,
                                               if_else(diet_cat == "fish_invert", -0.1732,
                                                       if_else(diet_cat == "algae_invert", 0,
                                                               NA))))),
         Nexc_log10 = ifelse(`dmperind` > 0, 1.461 + 0.6840*(log10(`dmperind`)) + 0.0246*temp + N_diet_coef + N_vert_coef,NA),
         `nind_ug/hr` = 10^Nexc_log10) %>%
  # p
  mutate(P_vert_coef = if_else(phylum == "Chordata", 0.7504, 0),
         P_diet_coef = if_else(diet_cat == "algae_detritus", 0.0173,
                               if_else(diet_cat == "invert", -0.2480,
                                       if_else(diet_cat == "fish", -0.0337,
                                               if_else(diet_cat == "fish_invert", -0.4525,
                                                       if_else(diet_cat == "algae_invert",0,
                                                               NA))))),
         Pexc_log10 = ifelse(`dmperind` >0, 0.6757 + 0.5656*(log10(`dmperind`)) + 0.0194*temp + P_diet_coef + P_vert_coef, NA),
         `pind_ug/hr` = 10^Pexc_log10)


##################### end of bradley's code #####################

### getting the local cfd file than the all sites files. 
col_list <- c("project","habitat","raw_filename","site","subsite_level1","subsite_level2","subsite_level3","date","year","month","day",
  "sp_code","count_num","transectarea_m","transectarea_m2","length_cm","biomass_g","density_num.m","density_num.m2","density_num.m3",
  "dmperind_g.ind","nind_ug.hr","pind_ug.hr","temp_c",
  "scientific_name","diet_cat","taxa_group","kingdom","phylum","class","order","family","genus") 

exc_df <- cons_np_ratio |>
  dplyr::select(-c(N_vert_coef,N_diet_coef,Nexc_log10,P_vert_coef,P_diet_coef,Pexc_log10,data_type))%>%
  dplyr::rename(dmperind_g.ind=dmperind,temp_c=temp,raw_filename=source,taxa_group=taxon_group,nind_ug.hr= `nind_ug/hr`,pind_ug.hr=`pind_ug/hr`)%>%
  mutate(subsite_level1=as.character(subsite_level1),
         subsite_level2=as.character(subsite_level2),
         subsite_level3=as.character(subsite_level3)) %>%
  dplyr::mutate(`density_num.m` = NA,'transectarea_m' = NA)  %>%# add this for the CND project
  dplyr::select(all_of(col_list)) 

#merge with the CND data
cnd <- read.csv(file.path("Data", "community_raw-data","harmonized_consumer_excretion_CLEAN_v3.csv"),stringsAsFactors = F,na.strings =c("."))

# > colnames(cnd)
# [1] "project"         "habitat"         "raw_filename"    "row_num"         "year"            "month"           "day"            
#  [8] "date"            "site"            "subsite_level1"  "subsite_level2"  "subsite_level3"  "sp_code"         "scientific_name"
# [15] "species"         "density_num.m"   "dmperind_g.ind"  "temp_c"          "count_num"       "transectarea_m"  "density_num.m2" 
# [22] "transectarea_m2" "density_num.m3"  "common_name"     "kingdom"         "phylum"          "class"           "order"          
# [29] "family"          "genus"           "taxa_group"      "diet_cat"        "nind_ug.hr"      "pind_ug.hr" 


cnd1<-cnd %>%
  dplyr::select(-row_num,-common_name,-species) %>%
  dplyr::mutate(length_cm=NA,count_num=NA, biomass_g=NA)

###combine consumer data
#check differences in column names and check column names to prepare to combine all consumer data
# setdiff(sort(names(exc_df)),sort(names(cnd1)))
comball <- rbind(exc_df, cnd1) %>%
  dplyr::select(all_of(col_list)) 


##########Only run this if we need to check the species against ITIS###############


if (run_species_check =="Y"){

 #read in the species check file if it exists, if not create a new one and run the check. This file will be updated with the new species that are added to the dataset and will be used for future checks.
  if (file.exists(file.path("Data", "community_tidy-data","04_all_community_taxa_itis_checked.csv"))) {
    message("Species check file already exists. Loading existing file.")
   spe_check <- read.csv(file.path("Data", "community_tidy-data","04_all_community_taxa_itis_checked.csv"),stringsAsFactors = F,na.strings =c(""))

  } else {
    message("Species check file does not exist. Creating new file and running species check.")
    spe_check <- data.frame(scientific_name = character(),
                            kingdom = character(),
                            phylum = character(),
                            class = character(),
                            order = character(),
                            family = character(),
                            genus = character(),
                            species = character(),
                            stringsAsFactors = FALSE)
  }

spe_check1 <- spe_check %>%
  dplyr::filter(!is.na(kingdom)) 

taxa_check <- comball %>%
 #select the scientific_name column. This column originally filled based on LTER sites reported species names
 dplyr::select(scientific_name)%>%
#Grab all unique species names
 mutate(scientific_name = sub("\\s+spp?\\.?$", "", scientific_name),
        scientific_name = trimws(scientific_name)) %>%
 dplyr::distinct() %>%
#exclude the species that have already been checked and have a match in the spe_check1 dataset
 dplyr::filter(!scientific_name %in% spe_check1$scientific_name) %>%
#Create an empty placeholder column to fill later
 dplyr::mutate(kingdom = NA,
               phylum = NA,
               class = NA,
               order = NA,
               family = NA,
               genus = NA,
               species = NA)


### add kingdom, phylum, order, family, genus and species name using taxize

for (i in 1:length(taxa_check$scientific_name)) {

  sp <- taxa_check[i, ]$scientific_name

  # Query species taxonomic information with error handling
  identified_species_names <- tryCatch(
    taxize::tax_name(
      sci = sp,
      get = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
      db = "itis",
      accepted = TRUE,
      ask = FALSE
    ),
    error = function(e) NULL
  )

  # Fill taxonomy info safely
  taxa_check[i, ]$kingdom <- if (!is.null(identified_species_names)) paste0(identified_species_names$kingdom, collapse = "") else NA
  taxa_check[i, ]$phylum  <- if (!is.null(identified_species_names)) paste0(identified_species_names$phylum, collapse = "") else NA
  taxa_check[i, ]$class   <- if (!is.null(identified_species_names)) paste0(identified_species_names$class, collapse = "") else NA
  taxa_check[i, ]$order   <- if (!is.null(identified_species_names)) paste0(identified_species_names$order, collapse = "") else NA
  taxa_check[i, ]$family  <- if (!is.null(identified_species_names)) paste0(identified_species_names$family, collapse = "") else NA
  taxa_check[i, ]$genus   <- if (!is.null(identified_species_names)) paste0(identified_species_names$genus, collapse = "") else NA
  taxa_check[i, ]$species <- if (!is.null(identified_species_names)) paste0(identified_species_names$species, collapse = "") else NA

} #close the for loop

# --- Error check at the end ---
# Find which species didn’t get a match
unmatched <- taxa_check[taxa_check$kingdom=="NA", "scientific_name"]

if (length(unmatched) > 0) {
  message("Warning: No ITIS records found for these species:\n",
          paste(unmatched, collapse = ", "))
} else {
  message("All species matched successfully!")
}

taxa_check_v2 <- taxa_check
#Replace the string "NA" with actual NA values
taxa_check_v2[taxa_check_v2 == "NA"] <- NA

#combine with spe_check1 to get the full list of species and their taxonomic information
taxa_check_v3<- rbind(spe_check1, taxa_check_v2) %>%
  arrange(kingdom, phylum, class, order, family, genus, species)
  
write.csv(x = taxa_check_v3, row.names = F, na = '',
          file = file.path("Data", "community_tidy-data","04_all_community_taxa_itis_checked.csv"))
} # close the species check

#load the species check and merge with the full dataset

spe <- read.csv(file.path("Data", "community_tidy-data","04_all_community_taxa_itis_checked.csv"),stringsAsFactors = F,na.strings =c("")) %>%
  #change the name to scientific_name_check to avoid confusion with the scientific_name column in the comball dataset
  dplyr::rename(scientific_name_check = scientific_name) %>%
  dplyr::select(-species) # remove the species column since it is not needed for the merge

#modify the species information from the list

exc_df_v99 <- exc_df %>%
    dplyr::mutate(scientific_name_check = scientific_name) %>%
  dplyr::left_join(spe, by = "scientific_name_check") %>%
    dplyr::mutate(kingdom = if_else(is.na(kingdom.y), kingdom.x, kingdom.y),
                  phylum = if_else(is.na(phylum.y), phylum.x, phylum.y),
                  class = if_else(is.na(class.y), class.x, class.y),
                  order = if_else(is.na(order.y), order.x, order.y),
                  family = if_else(is.na(family.y), family.x, family.y),
                  genus = if_else(is.na(genus.y), genus.x, genus.y)) %>%
  dplyr::select(all_of(col_list)) #reorder the columns to match the original order

# merge the spe table with the full dataset, if spe's taxonomic information is missing, it will be filled with the information from the original table
comball_v99 <- comball %>%
   dplyr::mutate(scientific_name_check = scientific_name) %>%
  dplyr::left_join(spe, by = "scientific_name_check") %>%
    dplyr::mutate(kingdom = if_else(is.na(kingdom.y), kingdom.x, kingdom.y),
                  phylum = if_else(is.na(phylum.y), phylum.x, phylum.y),
                  class = if_else(is.na(class.y), class.x, class.y),
                  order = if_else(is.na(order.y), order.x, order.y),
                  family = if_else(is.na(family.y), family.x, family.y),
                  genus = if_else(is.na(genus.y), genus.x, genus.y)) %>%
  dplyr::select(all_of(col_list)) #reorder the columns to match the original order

#### export and write to the drive
# Export only the sparc data
tidy_filename <- "03_harmonized_consumer_excretion_sparcsite.csv"
tidy_path <- file.path("Data", "community_tidy-data", tidy_filename)

# Export locally, only the new data
write.csv(x = exc_df_v99, na = '', row.names = F, file = tidy_path)

#export full data
tidy_filename1 <- "04_harmonized_consumer_excretion_sparc_cnd_site.csv"
tidy_path1 <- file.path("Data", "community_tidy-data", tidy_filename1)

# Export locally
write.csv(x = comball_v99, na = '', row.names = F, file = tidy_path1)

###open the file
# cfd <- read.csv(file.path("Data", "community_tidy-data","03_harmonized_consumer_excretion_sparcsite.csv"),stringsAsFactors = F,na.strings =c(""))
# all <- read.csv(file.path("Data", "community_tidy-data","04_harmonized_consumer_excretion_sparc_cnd_site.csv"),stringsAsFactors = F,na.strings =c(""))


# End ----
