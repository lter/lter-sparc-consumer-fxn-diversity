#----------------------------------##
# SPARC - Consumer Functional Diversity (CFD) - pre-processing data
###--------------------------------##

#Script authour(s): Shalanda Grier... 

#Sites: Palmer LTER, Arctic LTER, North Lakes LTER 


#Data Type:Consumer  Data Group: Zooplankton 

#Purpose 
#Prepare data for harmonization by transforming data from wide to long format 

#call in libraries 

library(librarian)
librarian::shelf(tidyverse, googledrive, readr, readxl, purrr)


#set up drive to access files from data folder 

CFD_drive <- googledrive::as_id("https://drive.google.com/drive/folders/1n6iqs3aK2xWkROI8nPSVfk6ZkYPNpF7F")


#download data from google drive 

#could be a loop 

#North Lakes LTER 
#data in long format no need to transform 
#data_folder1 <- googledrive::drive_ls(path = CFD_drive,
                                     #type= "csv", 
                                     #pattern ="ntl90_v11", overwrite= T)


#googledrive::drive_download(file=data_folder1$id)


#Arctic LTER 
data_folder2 <- googledrive::drive_ls(path=CFD_drive,
                                      type="csv",
                                      pattern = "2003-2022ArcLTERZoops")


googledrive::drive_download(file=data_folder2$id)


#Palmer LTER 
data_folder3 <- googledrive::drive_ls(path=CFD_drive,
                                      type="csv",
                                      pattern = "ZooplanktonDensity-EDI-20250415 ")


googledrive::drive_download(file=data_folder3$id)

###### transform data from each site from wide to long format then export processed csv to shared drive 

#Arctic "Toolik Lake"  LTER 

Arctic_wide <- read.csv("2003-2022ArcLTERZoops.csv")


Arctic_long <- Arctic_wide %>% tidyr::pivot_longer(cols = -c(1:4), names_to = "species", values_to = "density")

#remove "." between across column names 
names(Arctic_long) <- gsub("\\.", " ",names(Arctic_long))

#remove "." across species names 
Arctic_long$species <- gsub("\\.", " ", Arctic_long$species)


#write csv locally and to drive 

write.csv(Arctic_long, "ArcLTERZoopsLong.csv", row.names=FALSE)
#googledrive::drive_upload("ArcLTERZoopsLong.csv", path = "CFD_drive")


# Palmer LTER 

Palmer_wide <- read.csv("ZooplanktonDensity-EDI-20250415 .csv")

# remove all bio-volume columns with suffix "Vol" and remove "Num" from species names that indicate density (num/1000m3)

Palmer_long <- Palmer_wide %>% dplyr::select(-ends_with("Vol")) %>% tidyr::pivot_longer(cols = -c(1:26), names_to = "species", values_to = "density")
Palmer_long$species <- gsub("Num", "", Palmer_long$species)

write.csv(Palmer_long, "ZooplanktonDensity_long.csv", row.names=FALSE)

