
########## CFD ###########################

### Bird Species List for March Virtual Meeting
##################################

#Purpose : Gather bird speices list from sites with abundance data
#Author: Shalanda G.
#Date: March 2026 

#Note: ###Tentative list will reformat and add to harmonization efforts later on when we finish wrangling bird surveys 

library(librarian)
shelf(tidyr, dplyr)

### Call in Bird Data 
names(df) <- gsub("\\.", "", names(df))

#Cornell Bird Taxonmy Data

birdtaxa_data <-read.csv(file=file.path('Data', "community_raw-data", "eBird_taxonomy_v2025.csv")) %>%
  dplyr::rename(common_name = PRIMARY_COM_NAME, 
                order = ORDER,
                family = FAMILY,
                sci_name = SCI_NAME) %>%
  dplyr::filter(CATEGORY %in% "species") %>%
  dplyr::select(common_name, sci_name,family, order)

# bird sp _codes from Institute of Bird Populations 4 and 6 letter species codes
bird_4L_code <-read.csv(file=file.path('Data',"community_raw-data", "IBP-AOS-list25.csv")) %>%
  dplyr::rename(sp_code = SPEC,
                common_name= COMMONNAME,
                sci_name= SCINAME) %>%
  dplyr::select(sp_code, common_name, sci_name)


## Plum Island LTER  

Plum <- read.csv(file=file.path('Data', "community_raw-data", "MON-EX-PRNWR-Breeding-Birds.csv")) %>%
  dplyr::select(Scientific.Name, Common.Name) %>%
  dplyr::rename(scientific_name = Scientific.Name, 
                common_name = Common.Name) %>%
  dplyr::mutate(project = "PLUM") %>% 
  dplyr::relocate(project, .before= scientific_name) %>%
  dplyr::distinct()

#join  by common names 

Plum_v2 <- left_join(Plum, birdtaxa_data, by = "common_name") #only 9 species with no name 

#sum(is.na(Plum_v2$sci_name))

Plum_v3 <-left_join(Plum_v2, bird_4L_code, by = "common_name", 
                    suffix = c(".x", ".y")) %>%
  dplyr::mutate(species = coalesce(sci_name.x, sci_name.y)) %>% # only 7 species with no name 
  dplyr::select(-c("sci_name.x", "sci_name.y"))
#sum(is.na(Plum_v3$species))
missing_sci_names_Plum <- Plum_v3 %>%
  filter(is.na(species)) 

Plum_ready <- Plum_v3 %>%
  dplyr::select(c(1,3,4:5,7)) %>%
  dplyr::rename(scientific_name = species) %>%
  dplyr::mutate(habitat = "estuary", class = "Aves", raw_filename = "MON-EX-PRNWR-Breeding-Birds.csv") %>%
  dplyr::relocate(habitat, raw_filename, class, order, family, common_name, .before = scientific_name) %>%
  dplyr::mutate(scientific_name = case_when(
    common_name == "Herring Gull" ~ "Larus argentatus",
    common_name == "Rufous-sided Towhee" ~ "Pipilo erythrophthalmus",
    common_name == "Northern Oriole" ~ "Icterus galbula",
    common_name == "Great black-backed Gull" ~ "Larus marinus",
    common_name == "Green-backed Heron" ~ "Butorides striata",
    common_name == "nelsons sharp-tailed sparrow" ~ "Ammospiza nelsoni",
    common_name == "Black-crowned Night-Heron" ~ "Nycticorax nycticorax",
    T ~ scientific_name
  )) %>%
  dplyr::mutate(order = case_when(
    scientific_name == "Larus argentatus" ~ "Charadriiformes",
    scientific_name == "Pipilo erythrophthalmus" ~ "Passeriformes",
    scientific_name == "Icterus galbula" ~ "Passeriformes",
    scientific_name == "Larus marinus" ~ "Charadriiformes",
    scientific_name == "Butorides striata" ~ "Pelecaniformes",
    scientific_name == "Ammospiza nelsoni" ~ "Passeriformes",
    scientific_name == "Nycticorax nycticorax" ~ "Pelecaniformes", 
    scientific_name == "Setophaga petechia" ~ "Passeriformes",
    T ~ order 
  )) %>%
  dplyr::mutate(family = case_when(
    scientific_name == "Larus argentatus" ~ "Laridae",
    scientific_name == "Pipilo erythrophthalmus" ~ "Passerellidae",
    scientific_name == "Icterus galbula" ~ "Icteridae",
    scientific_name == "Larus marinus" ~ "Laridae",
    scientific_name == "Butorides striata" ~ "Ardeidae",
    scientific_name == "Ammospiza nelsoni" ~ "Passerellidae",
    scientific_name == "Nycticorax nycticorax" ~ "Ardeidae",
    scientific_name == "Setophaga petechia" ~ "Parulidae",
    T ~ family
  )) %>%
  dplyr::select(-common_name) %>%
  dplyr::mutate(genus = str_extract(scientific_name, pattern = "\\w+")) %>%
  dplyr::relocate(genus, .before = "scientific_name")

## CCE LTER

CCE <- read.csv(file=file.path('Data', "community_raw-data", "table_256.csv")) %>%
  dplyr::select(Species) %>%
  dplyr::rename(sp_code = Species) %>%
  dplyr::distinct() %>%
  dplyr::mutate(project = "CCE",
                common_name = NA) %>%
  dplyr::relocate(project, .before = sp_code) %>%
  dplyr::mutate(common_name = case_when(
    sp_code == "BRPE" ~ "Brown Pelican",
    sp_code == "BRCO" ~ "Brandt",
    sp_code == "PALO" ~ "Pacific Loon",
    sp_code == "BFAL" ~ "Black-footed Albatross",
    sp_code == "NOFU" ~ "Northern Fulmar",
    sp_code == "STSH" ~ "Short-tailed Shearwater",
    sp_code == "BVSH" ~ "Black-vented Shearwater",
    sp_code == "LHSP" ~ "Leach",
    sp_code == "FTSP" ~ "Fork-tailed Storm-Petrel",
    sp_code == "PECO" ~ "Pelagic Cormorant",
    sp_code == "RTLO" ~ "Red-throated Loon",
    sp_code == "REPH" ~ "Red Phalarope",
    sp_code == "RNPL" ~ "Red-necked Phalarope",
    sp_code == "EAGR" ~ "Eared Grebe",
    sp_code == "WEGR" ~ "Western Grebe",
    sp_code == "CLGR" ~ "Clarks Grebe",
    sp_code == "POJA" ~ "Pomarine Jaeger",
    sp_code == "LTJA" ~ "Long-tailed Jaeger",
    sp_code == "HEEG" ~ "Heermann's Gull",
    sp_code == "HEGU" ~ "Herring Gull",
    sp_code == "GWGU" ~ "Glaucus-winged Gull",
    sp_code == "WEGU" ~ "Western Gull",
    sp_code == "CAGU" ~ "California Gull",
    sp_code == "BOGU" ~ "Bonaparte",
    sp_code == "ROTE" ~ "Royal Tern",
    sp_code == "BLKI" ~ "Black-legged Kittiwake",    
    sp_code == "COMU" ~ "Common Murre",
    sp_code == "RHAU" ~ "Rhinoceros Auklet",
    sp_code == "CAAU" ~ "Cassins Auklet",
    sp_code == "HYGU" ~ "Hybrid Gull",
    sp_code == "WE/CLGR" ~ "Western Grebe",
    sp_code == "GW/WEGU" ~ "Western Gull",
    sp_code == "UNID Alcids" ~ "Unidentified Alcids",
    sp_code == "Tern Spp" ~ "Tern",
    sp_code == "Jaegar Sp" ~ "Jaegar",
    sp_code == "Phal Spp" ~ "Phalarope",
    sp_code %in% c("UNCE", "CBWH", "BBWH", "BLWH", "FIWH", "SEWH", "HUWH", "SPWH", "MWH",
                   "GRWH", "PSWH", "DSWH", "UNWH", "KIWH", "PIWH/SEP", "FKWH", "CODO",
                   "LBCD", "SBCD", "PWSD", "RIDO", "NRWD", "BODO", "STDO", "UNDO", "CAPO", 
                   "HAPO", "HASE", "CASL", "GFSE", "NESE", "NFSE", "UNPI"
                   ) ~ "mammal"
      
  )) %>%
  dplyr::filter(!common_name %in% "mammal") #remove mammal species 


CCE_v2 <- left_join(CCE, bird_4L_code, by = "sp_code",
                    suffix = c(".x", ".y")) %>%
  mutate(common_name = coalesce(common_name.x, common_name.y)) %>%
  dplyr::select(-c("common_name.x", "common_name.y"))

CCE_test <- left_join (CCE_v2, birdtaxa_data, by = "common_name" ,
                       suffix = c(".x", ".y")) %>%
  mutate(sci_name = coalesce(sci_name.x, sci_name.y)) %>%
  dplyr::select(-c("sci_name.x", "sci_name.y"))

#sum(is.na(CCE_test$sci_name))  #70 species no reported common name from code after removing known mammals. 
#Likely more mammals, unidentified species, expired species codes or codes not present in either database. 

CCE_ready <- CCE_test %>%  # remove species without scientific name 
  dplyr::select((c(1,3,4:6))) %>%
  dplyr::filter(!is.na(sci_name))%>%
  dplyr::rename(scientific_name = sci_name) %>%
  dplyr::mutate(raw_filename = "table_256.csv", class = "Aves", habitat = "coastal") %>%
  dplyr::mutate(genus = str_extract(scientific_name, pattern = "\\w+")) %>%
  dplyr::relocate(habitat, raw_filename, class, order, family, genus, common_name, .before = "scientific_name") %>%
  dplyr::filter(!str_starts(common_name, "Unidentified")) %>% # remove species without specified scientific name
  dplyr::select(-common_name)

    

##Baltimore LTER 

Baltimore <- read.csv(file=file.path('Data', "community_raw-data", "bird-survey-2001-2015-birds.csv")) %>%
  dplyr::select(species_id) %>%
  dplyr::mutate(project = "Baltimore") %>%
  dplyr::relocate(project, .before =species_id) %>%
  dplyr::distinct() %>%
  dplyr::filter(!species_id %in% "temp") 

Baltimore_v2 <- left_join(Baltimore, bird_4L_code, by = c("species_id" = "sp_code")) #only 14 missing 

Baltimore_v3 <-left_join(Baltimore_v2, birdtaxa_data, by = "common_name",
                         suffix = c(".x", ".y")) %>%
  dplyr::mutate(sci_name = coalesce(sci_name.x, sci_name.y)) %>%
  dplyr::select(-c("sci_name.x", "sci_name.y")) %>%
  dplyr::mutate(sci_name = case_when(
    species_id == "AMGO" ~ "Spinus tristis",
    species_id == "RODO" ~ "Streptopelia roseogrisea",
    species_id == "UNAC" ~ "Not_located",
    species_id == "HERG" ~ "Larus argentatus",
    species_id == "HOWR" ~ "Troglodytes aedon",
    species_id == "CAGO" ~ "Branta canadensis",
    species_id == "ETTI" ~ "Baeolophus bicolor",
    species_id == "UNKN" ~ "Unidentified",
    species_id == "TRSW" ~ "Tachycineta bicolor",
    species_id == "YWAR" ~ "Not_located",
    species_id == "RPHE" ~ "Phasianus colchicus",
    species_id == "WAVI" ~ "Vireo gilvus",
    species_id == "UNGU" ~ "Unidentified Gull",
    species_id == "BNOW" ~ "Tyto alba",
    T ~ sci_name
  )) %>%
  dplyr::mutate(order = case_when(
    sci_name == "Spinus tristis" ~ "Passeriformes",
    sci_name == "Streptopelia roseogrisea" ~ "Columbiformes",
    sci_name == "Larus argentatus" ~ "Charadriiformes",
    sci_name == "Troglodytes aedon" ~ "Passeriformes",
    sci_name == "Branta canadensis" ~ "Anseriformes",
    sci_name == "Baeolophus bicolor" ~ "Passeriformes",
    sci_name == "Tachycineta bicolor" ~ "Passeriformes",
    sci_name == "Phasianus colchicus" ~ "Galliformes",
    sci_name == "Vireo gilvus" ~ "Passeriformes",
    sci_name == "Tyto alba" ~ "Strigiformes",
    T ~ order
  )) %>%
  dplyr::mutate(family = case_when(
    sci_name == "Spinus tristis" ~ "Fringillidae",
    sci_name == "Streptopelia roseogrisea" ~ "Columbidae",
    sci_name == "Larus argentatus" ~ "Laridae",
    sci_name == "Troglodytes aedon" ~ "Troglodytidae",
    sci_name == "Branta canadensis" ~ "Anatidae",
    sci_name == "Baeolophus bicolor" ~ "Paridae",
    sci_name == "Tachycineta bicolor" ~ "Hirundinidae",
    sci_name == "Phasianus colchicus" ~ "Phasianidae",
    sci_name == "Vireo gilvus" ~ "Vireonidae",
    sci_name == "Tyto alba" ~ "Tytonidae",
    T ~ family
  )) 
  

missing_sci_names_Bal <- Baltimore_v3 %>%
  filter(is.na(sci_name))

#sum(is.na(Baltimore_v2$sci_name))

Baltimore_ready <- Baltimore_v3 %>%
  dplyr::select(1,2:6) %>%
  dplyr::rename(scientific_name = sci_name) %>%
  dplyr::mutate(raw_filename = "bird-survey-2001-2015-birds.csv", habitat = "urban", class = "Aves") %>%
  dplyr::mutate(genus = str_extract(scientific_name, pattern = "\\w+")) %>%
  dplyr::relocate(habitat, raw_filename, class, order, family, genus, .before = "scientific_name") %>%
  dplyr::select(-c(common_name, species_id))


### Combine all bird program data 

#call in data from Lauren E.

LNE_birdlist <- read.csv(file=file.path('Data', "species_raw-data", "LNE_specieslist_03042026_v3.csv"))


#combinded bird species list data for Luquillo, Hubbard, Andrews Forest, CAP, Arctic, SONGS, Plum, CCE, BES (Baltimore) 
master_birdlist <- rbind(LNE_birdlist, Baltimore_ready, CCE_ready, Plum_ready)

#Make last version of data
mb_v99 <-master_birdlist 
 
# Check structure
dplyr::glimpse(mb_v99)

# Identify the file name & path
bird_file <- "master_birdlist.csv"
bird_path <- file.path("Data", "species_tidy-data", bird_file)

# Export locally
write.csv(x =  mb_v99, na = '', row.names = F, file = bird_path)  
  
