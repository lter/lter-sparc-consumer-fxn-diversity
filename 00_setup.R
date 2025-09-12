## ---------------------------------------------------- ##
# CFD - Step 1: Harmonization
## ---------------------------------------------------- ##
# Purpose:
## Standarize and combine (i.e., "harmonize") input datasets

# Load libraries
librarian::shelf(tidyverse, googledrive, fs)

# Clear environment & collect garbage
rm(list = ls()); gc()


## --------------------------- ##
# Make Folders ----
## --------------------------- ##


local_root <- "data"  #local root folder

# List of folder names on Google Drive
folders <- c(
  "01_community_raw_data",
  "02_community_processed_data",
  "11_traits_raw_data",
  "12_traits_processed_data",
  "00_keys"
)

 # Identify the folder
parent <- googledrive::as_id("https://drive.google.com/drive/folders/1AxdFQ0EjNqaLUTzms4prF52cqbVFec0F")
 
 # Get children folders under parent
children <- drive_ls(parent, type = "folder")
  
# ---- FOR LOOP ----
for (fld in folders) {
  hit <- children[children$name == fld, ]
  
  if (nrow(hit) == 0) {
    warning("Subfolder not found: ", fld)
    next
  }
  
  local_dir <- path(local_root, fld)
  dir_create(local_dir, recurse = TRUE)
  
  message("\n== Downloading folder: ", fld, " ==")
  
  # List everything inside this subfolder (non-recursive)
  items <- drive_ls(hit$id)
  
  if (nrow(items) == 0) {
    message("  (empty)")
    next
  }
  
  for (i in seq_len(nrow(items))) {
    it <- items[i, ]
    name <- it$name
    mime <- it$drive_resource[[1]]$mimeType
    
    # Build output filename
    out_path <- path(local_dir, paste0(path_ext_remove(name), ".csv"))
    
    if (mime %in% c("application/vnd.google-apps.spreadsheet")) {
      # Export Google-native files as CSV
      message("⬇️  Exporting as CSV: ", out_path)
      drive_download(as_id(it$id), path = out_path, type = "csv", overwrite = TRUE)
    } else {
      # For non-Google-native: just download as-is
      message("⬇️  Downloading as-is: ", path(local_dir, name))
      drive_download(as_id(it$id), path = path(local_dir, name), overwrite = TRUE)
    }
  }
}


message("\n✅ Finished downloading all requested subfolders (Google-native exported to CSV).")
