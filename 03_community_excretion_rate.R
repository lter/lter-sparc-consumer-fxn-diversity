#----------------------------------##
# SPARC - Consumer Functional Diversity (CFD) 
###--------------------------------##

# Script author(s): Shalanda Grier, Li Kui, Nick Lyons
# Sites: Palmer LTER, Arctic LTER, North Lakes LTER

# Load libraries
librarian::shelf(tidyverse)

# Get set up
source("00_setup.R")

# Clear environment & collect garbage
rm(list = ls()); gc()


# read in the harmonized data
df <- read.csv(file.path("Data", "community_tidy-data","02_community_wrangled.csv"),stringsAsFactors = F,na.strings =c("")) 

#remove the columns that are not needed
dfrm <- df %>%
  select(-c(density_num_km2,biomass_kg,taxonomicLevel))
#### calculate excretion rate

# take out the rows that are needed 

df1 <-dfrm %>%
  rename(dmperind=`dmperind_g.ind`,temp=temp_c) 

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
         `nind_ug/hr` = 10^Nexc_log10,
         `nind_ug/hr` = ifelse(is.na(`nind_ug/hr`),0,`nind_ug/hr`)) %>%
  # p
  mutate(P_vert_coef = if_else(phylum == "Chordata", 0.7504, 0),
         P_diet_coef = if_else(diet_cat == "algae_detritus", 0.0173,
                               if_else(diet_cat == "invert", -0.2480,
                                       if_else(diet_cat == "fish", -0.0337,
                                               if_else(diet_cat == "fish_invert", -0.4525, 
                                                       if_else(diet_cat == "algae_invert",0,
                                                               NA))))),
         Pexc_log10 = ifelse(`dmperind` >0, 0.6757 + 0.5656*(log10(`dmperind`)) + 0.0194*temp + P_diet_coef + P_vert_coef, NA),
         `pind_ug/hr` = 10^Pexc_log10,
         `pind_ug/hr` = ifelse(is.na(`pind_ug/hr`),0,`pind_ug/hr`))  


##################### end of bradley's code #####################

### skipping pivot longer below because my machine can't handle it... haha
exc_df <- cons_np_ratio |>
  dplyr::select(-c(N_vert_coef,N_diet_coef,Nexc_log10,P_vert_coef,P_diet_coef,Pexc_log10,data_type))%>%
  rename(`dmperind_g.ind`=dmperind,temp_c=temp,raw_filename=source,taxa_group=taxon_group,nind_ug.hr=`nind_ug/hr`,pind_ug.hr=`pind_ug/hr`)%>%
  mutate(subsite_level1=as.character(subsite_level1),
         subsite_level2=as.character(subsite_level2),
         subsite_level3=as.character(subsite_level3))


#merge with the CND data
cnd <- read.csv(file.path("Data", "community_raw-data","harmonized_consumer_excretion_CLEAN.csv"),stringsAsFactors = F,na.strings =c("."))

# colnames(cnd)
# #[1] "project"         "habitat"         "raw_filename"    "row_num"         "year"            "month"           "day"             "date"           
# #[9] "site"            "subsite_level1"  "subsite_level2"  "subsite_level3"  "sp_code"         "scientific_name" "species"         "density_num.m"  
# #[17] "dmperind_g.ind"  "temp_c"          "density_num.m2"  "density_num.m3"  "common_name"     "kingdom"         "phylum"          "class"          
# #[25] "order"           "family"          "genus"           "taxa_group"      "diet_cat"        "nind_ug.hr"      "pind_ug.hr" 

cnd1<-cnd %>%
  dplyr::select(-row_num,-density_num.m,-common_name,-species) %>%
  mutate(length_cm=NA,count_num=NA, biomass_g=NA) 

###combine consumer data 
#check differences in column names and check column names to prepare to combine all consumer data 
#setdiff(names(exc_df),names(cnd1))
comball <- rbind(exc_df, cnd1)

#### export and write to the drive
# Export only the sparc data
tidy_filename <- "03_harmonized_consumer_excretion_sparcsite.csv"
tidy_path <- file.path("Data", "community_tidy-data", tidy_filename)

# Export locally
write.csv(x = exc_df, na = '', row.names = F, file = tidy_path)

#export full data
tidy_filename1 <- "04_harmonized_consumer_excretion_sparc_cnd_site.csv"
tidy_path1 <- file.path("Data", "community_tidy-data", tidy_filename1)

# Export locally
write.csv(x = comball, na = '', row.names = F, file = tidy_path1)



# End ----
